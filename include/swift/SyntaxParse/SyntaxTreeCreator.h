//===--- SyntaxTreeCreator.h - Syntax Tree Creation  ------------*- C++ -*-===//
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

#ifndef SWIFT_SYNTAX_PARSE_SYNTAXTREECREATOR_H
#define SWIFT_SYNTAX_PARSE_SYNTAXTREECREATOR_H

#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Syntax/References.h"
#include "llvm/ADT/StringRef.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Basic/SourceManager.h"

namespace swift {
  class SourceManager;
  class SyntaxParsingCache;
  class SourceFile;

namespace syntax {
class SyntaxArena;
class SourceFileSyntax;
}

/// Receives the parsed syntax info from the parser and constructs a persistent
/// syntax tree by converting the data into \c RawSyntax objects, allocated from
/// a \c SyntaxArena.
///
/// It also handles caching re-usable RawSyntax objects and skipping parsed
/// nodes via consulting a \c SyntaxParsingCache.
class SyntaxTreeCreator final : public SyntaxParseActions {
  SourceManager &SM;
  unsigned BufferID;
  RC<syntax::SyntaxArena> Arena;

  /// A string allocated in \c Arena that contains an exact copy of the source
  /// file for which this \c SyntaxTreeCreator creates a syntax tree. \c
  /// RawSyntax nodes can safely reference text inside this buffer since they
  /// retain the \c SyntaxArena which holds the buffer.
  StringRef ArenaSourceBuffer;

  /// A cache of nodes that can be reused when creating the current syntax
  /// tree.
  SyntaxParsingCache *SyntaxCache;

  /// Contains all the RawSyntax nodes that were initially created as deferred
  /// nodes and are thus being kept alive by this \c SyntaxTreeCreator.
  /// All of these nodes will receive a \c Release call when the \c
  /// SyntaxTreeCreator is destructed.
  std::vector<OpaqueSyntaxNode> DeferredNodes;

public:
  SyntaxTreeCreator(SourceManager &SM, unsigned bufferID,
                    SyntaxParsingCache *syntaxCache,
                    RC<syntax::SyntaxArena> arena);
  ~SyntaxTreeCreator();

  Optional<syntax::SourceFileSyntax>
  realizeSyntaxRoot(OpaqueSyntaxNode root, const SourceFile &SF) override;

  OpaqueSyntaxNode recordToken(tok tokenKind, StringRef leadingTrivia,
                               StringRef trailingTrivia,
                               CharSourceRange range) override {
    auto tokStartOffset = SM.getLocOffsetInBuffer(range.getStart(), BufferID);
    auto leadingTriviaStartOffset = tokStartOffset - leadingTrivia.size();
    auto trailingTriviaStartOffset = tokStartOffset + range.getByteLength();

    // Get StringRefs of the token's texts that point into the syntax arena's
    // buffer.
    StringRef leadingTriviaText =
        ArenaSourceBuffer.substr(leadingTriviaStartOffset, leadingTrivia.size());
    StringRef tokenText = ArenaSourceBuffer.substr(tokStartOffset, range.getByteLength());
    StringRef trailingTriviaText = ArenaSourceBuffer.substr(
        trailingTriviaStartOffset, trailingTrivia.size());

    size_t TextLength = leadingTrivia.size() + range.getByteLength() + trailingTrivia.size();
    auto raw = syntax::RawSyntax::make(tokenKind, tokenText, TextLength,
                               leadingTriviaText, trailingTriviaText,
                                       syntax::SourcePresence::Present, Arena);
    OpaqueSyntaxNode opaqueN = raw;
    return opaqueN;
  }

  OpaqueSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc) override;

  OpaqueSyntaxNode
  recordRawSyntax(syntax::SyntaxKind kind,
                  const SmallVector<OpaqueSyntaxNode, 4> &elements) override {
//    SmallVector<RC<syntax::RawSyntax>, 16> parts;
//    parts.reserve(elements.size());
    size_t ByteLength = 0;
    for (OpaqueSyntaxNode opaqueN : elements) {
      if (opaqueN != nullptr) {
        ByteLength += static_cast<syntax::RawSyntax *>(opaqueN)->getTextLength();
//        static_cast<syntax::RawSyntax *>(opaqueN)->Release();
      }
//      parts.push_back(transferOpaqueNode(opaqueN));
    }
    ArrayRef<syntax::RawSyntax *> parts(reinterpret_cast<syntax::RawSyntax *const*>(elements.data()), elements.size());
    auto raw =
    syntax::RawSyntax::make(kind, parts, ByteLength, syntax::SourcePresence::Present, Arena);
    OpaqueSyntaxNode opaqueN = raw;
    return opaqueN;
  }

  OpaqueSyntaxNode makeDeferredToken(tok tokenKind, StringRef leadingTrivia,
                                     StringRef trailingTrivia,
                                     CharSourceRange range,
                                     bool isMissing) override {
    // Instead of creating dedicated deferred nodes that will be recorded only if
    // needed, the SyntaxTreeCreator always records all nodes and forms RawSyntax
    // nodes for them. This eliminates a bunch of copies that would otherwise
    // be required to record the deferred nodes.
    // Should a deferred node not be recorded, its data stays alive in the
    // SyntaxArena. This causes a small memory leak but since most nodes are
    // being recorded, it is acceptable.
    if (isMissing) {
      auto Node = recordMissingToken(tokenKind, range.getStart());
      return Node;
    } else {
      auto Node = recordToken(tokenKind, leadingTrivia, trailingTrivia, range);
      return Node;
    }
  }

  OpaqueSyntaxNode
  makeDeferredLayout(syntax::SyntaxKind k,
                     bool IsMissing,
                     const SmallVector<OpaqueSyntaxNode, 4> &children) override{
    // Also see comment in makeDeferredToken
    auto Node = recordRawSyntax(k, children);
    return Node;
  }

  OpaqueSyntaxNode recordDeferredToken(OpaqueSyntaxNode deferred) override {
    return deferred;
  }
  OpaqueSyntaxNode recordDeferredLayout(OpaqueSyntaxNode deferred) override {
    return deferred;
  }

  DeferredNodeInfo getDeferredChild(OpaqueSyntaxNode node, size_t ChildIndex) override;
  
  size_t getByteLength(OpaqueSyntaxNode node) override {
    syntax::RawSyntax *raw = static_cast<syntax::RawSyntax *>(node);
    return raw->getTextLength();
  }
  
  tok getTokenKind(OpaqueSyntaxNode node) override {
    syntax::RawSyntax *raw = static_cast<syntax::RawSyntax *>(node);
    return raw->getTokenKind();
  }
  
  syntax::SyntaxKind getSyntaxKind(OpaqueSyntaxNode node) override {
    syntax::RawSyntax *raw = static_cast<syntax::RawSyntax *>(node);
    return raw->getKind();
  }
  
  bool isMissing(OpaqueSyntaxNode node) override {
    syntax::RawSyntax *raw = static_cast<syntax::RawSyntax *>(node);
    return raw->isMissing();
  }

  void discardRecordedNode(OpaqueSyntaxNode node) override;
  
  syntax::RawSyntax *getLibSyntaxNodeFor(OpaqueSyntaxNode node) override {
    return static_cast<syntax::RawSyntax *>(node);
  }
  
  OpaqueSyntaxNode getExplicitNodeFor(OpaqueSyntaxNode node) override {
    return node;
  }
  
  std::shared_ptr<SyntaxTreeCreator> getLibSyntaxAction(std::shared_ptr<SyntaxParseActions> sharedThis) override {
    return std::static_pointer_cast<SyntaxTreeCreator>(sharedThis);
  }

  std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) override;
};

} // end namespace swift

#endif
