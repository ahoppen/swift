//===--- SyntaxTreeCreator.cpp - Syntax Tree Creation  ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SyntaxParse/SyntaxTreeCreator.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxVisitor.h"
#include "swift/Syntax/Trivia.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Parse/Token.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/OwnedString.h"

using namespace swift;
using namespace swift::syntax;

SyntaxTreeCreator::SyntaxTreeCreator(SourceManager &SM, unsigned bufferID,
                                     SyntaxParsingCache *syntaxCache,
                                     RC<syntax::SyntaxArena> arena)
    : SM(SM), BufferID(bufferID), Arena(std::move(arena)),
      SyntaxCache(syntaxCache) {
  StringRef BufferContent = SM.getEntireTextForBuffer(BufferID);
  if (!BufferContent.empty()) {
    char *Data = (char *)Arena->Allocate(BufferContent.size(), alignof(char *));
    std::uninitialized_copy(BufferContent.begin(), BufferContent.end(), Data);
    ArenaSourceBuffer = StringRef(Data, BufferContent.size());
    assert(ArenaSourceBuffer == BufferContent);
    Arena->setHotUseMemoryRegion(ArenaSourceBuffer.begin(),
                                 ArenaSourceBuffer.end());
  } else {
    ArenaSourceBuffer = StringRef();
  }
}

SyntaxTreeCreator::~SyntaxTreeCreator() {
  // Release all deferred nodes that are being kept alive by this
  for (auto node : DeferredNodes) {
    static_cast<RawSyntax *>(node)->Release();
  }
  Arena.reset();
}

namespace {
/// This verifier traverses a syntax node to emit proper diagnostics.
class SyntaxVerifier: public SyntaxVisitor {
  SourceManager &SourceMgr;
  unsigned BufferID;
  DiagnosticEngine &Diags;

  template<class T>
  SourceLoc getSourceLoc(T Node) {
    return SourceMgr.getLocForOffset(
        BufferID, Node.getAbsolutePositionAfterLeadingTrivia().getOffset());
  }
public:
  SyntaxVerifier( SourceManager &SM, unsigned bufID, DiagnosticEngine &diags)
    : SourceMgr(SM), BufferID(bufID), Diags(diags) {}

  void visit(UnknownDeclSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "declaration");
    visitChildren(Node);
  }
  void visit(UnknownExprSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "expression");
    visitChildren(Node);
  }
  void visit(UnknownStmtSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "statement");
    visitChildren(Node);
  }
  void visit(UnknownTypeSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "type");
    visitChildren(Node);
  }
  void visit(UnknownPatternSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "pattern");
    visitChildren(Node);
  }
  void verify(Syntax Node) {
    Node.accept(*this);
  }
};
} // anonymous namespace

Optional<SourceFileSyntax>
SyntaxTreeCreator::realizeSyntaxRoot(OpaqueSyntaxNode rootN,
                                     const SourceFile &SF) {
  auto raw = transferOpaqueNode(rootN);
  auto rootNode = makeRoot<SourceFileSyntax>(raw);

  // Verify the tree if specified.
  if (SF.getASTContext().LangOpts.VerifySyntaxTree) {
    ASTContext &ctx = SF.getASTContext();
    SyntaxVerifier Verifier(ctx.SourceMgr, SF.getBufferID().getValue(),
                            ctx.Diags);
    Verifier.verify(rootNode);
  }
  return rootNode;
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordMissingToken(tok kind, SourceLoc loc) {
  auto raw = RawSyntax::missing(kind, getTokenText(kind), Arena);
  OpaqueSyntaxNode opaqueN = raw.get();
  raw.resetWithoutRelease();
  return opaqueN;
}

SyntaxParseActions::DeferredNodeInfo
SyntaxTreeCreator::getDeferredChild(OpaqueSyntaxNode node, size_t ChildIndex) {
  RawSyntax *raw = static_cast<RawSyntax *>(node);
  RawSyntax *Child = raw->getChildRef(ChildIndex);
  if (Child->isToken()) {
    return DeferredNodeInfo(Child, SyntaxKind::Token,
                            Child->getTokenKind(), Child->isMissing());
  } else {
    return DeferredNodeInfo(Child, Child->getKind(), tok::NUM_TOKENS,
                            Child->isMissing());
  }
}

std::pair<size_t, OpaqueSyntaxNode>
SyntaxTreeCreator::lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
  if (!SyntaxCache)
    return {0, nullptr};
  auto cacheLookup = SyntaxCache->lookUp(lexerOffset, kind);
  if (!cacheLookup)
    return {0, nullptr};
  RC<RawSyntax> raw = cacheLookup->getRaw();
  OpaqueSyntaxNode opaqueN = raw.get();
  size_t length = raw->getTextLength();
  raw.resetWithoutRelease();
  return {length, opaqueN};
}

void SyntaxTreeCreator::discardRecordedNode(OpaqueSyntaxNode opaqueN) {
  if (!opaqueN)
    return;
  static_cast<RawSyntax *>(opaqueN)->Release();
}
