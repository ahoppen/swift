//===--- ASTGen.cpp -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/ASTGen.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/TypeRepr.h"

using namespace swift;
using namespace swift::syntax;

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig, ASTContext &Context) {
  char *start = static_cast<char *>(Context.Allocate(Orig.size(), 1));
  char *p = start;

  if (p) {
    for (char c : Orig) {
      if (c != '_') {
        *p++ = c;
      }
    }
  }

  return StringRef(start, p - start);
}

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig) {
  return copyAndStripUnderscores(Orig, *Context);
}

DeclNameRef ASTGen::generateDeclNameRef(DeclNameSyntaxRef DeclNameSyntax) {
  auto baseName = DeclNameSyntax.getDeclBaseName();
  DeclBaseName declBaseName;
  switch (baseName->getTokenKind()) {
  case tok::kw_init:
    declBaseName = DeclBaseName::createConstructor();
    break;
  case tok::kw_deinit:
    declBaseName = DeclBaseName::createDestructor();
    break;
  case tok::kw_subscript:
    declBaseName = DeclBaseName::createSubscript();
    break;
  default:
    declBaseName =
        DeclBaseName(Context->getIdentifier(baseName->getIdentifierText()));
    break;
  }
  if (DeclNameSyntax.getDeclNameArguments().hasValue()) {
    SmallVector<Identifier, 2> argumentLabels;
    auto arguments = DeclNameSyntax.getDeclNameArguments()->getArguments();
    for (auto arg : arguments.getRef()) {
      auto argName = arg->getName()->getIdentifierText();
      argumentLabels.push_back(Context->getIdentifier(argName));
    }
    return DeclNameRef(DeclName(*Context, declBaseName, argumentLabels));
  } else {
    return DeclNameRef(declBaseName);
  }
}

//===--------------------------------------------------------------------===//
// MARK: - Diagnostics

void ASTGen::diagnoseDollarIdentifierImpl(const TokenSyntaxRef &Token,
                                          bool DiagnoseDollarPrefix) {
  assert(Token.getIdentifierText()[0] == '$' && "The non '$' case should have been handled by diagnoseDollarIdentifier");
  
  // We have a standalone '$' that is not guarded by backticks.
  // Offer to replace it with '`$`'.
  if (Token.getText() == "$") {
    diagnose(Token, diag::standalone_dollar_identifier)
        .fixItReplace(getLoc(Token), "`$`");
    return;
  }

  if (!DiagnoseDollarPrefix) {
    // We've looked for the unescaped dollar identifier and are done diagnosing
    return;
  }
  if (Token.getIdentifierText() == "$") {
    // We've seen an escaped dollar identifier, `$`. This is fine
    return;
  }
  if (Context->LangOpts.EnableDollarIdentifiers || Mode == LexerMode::SIL ||
      Mode == LexerMode::SwiftInterface) {
    // Dollar identifiers are allowed here.
    return;
  }

  diagnose(Token, diag::dollar_identifier_decl,
           Context->getIdentifier(Token.getText()));
}

//===--------------------------------------------------------------------===//
// MARK: - Locations

SourceLoc ASTGen::getLeadingTriviaLoc(const SyntaxRef &Node) {
  auto offset = Node.getAbsolutePositionBeforeLeadingTrivia().getOffset();
  return TreeStartLoc.getAdvancedLoc(offset);
}

SourceLoc ASTGen::getContentStartLoc(const SyntaxRef &Node) {
  auto offset = Node.getAbsolutePositionAfterLeadingTrivia().getOffset();
  return TreeStartLoc.getAdvancedLoc(offset);
}

SourceRange ASTGen::getASTRange(const SyntaxRef &Node) {
  // SourceRange has an implicit requirement that the start and end loc point
  // to start positions of tokens. The range then includes everything from the
  // start to the end token, inclusively. E.g. the source range spanning the
  // following statements is represented by the following two locations
  //  let x = 123
  //  <--------->
  //  ^       ^
  //  Start   End

  auto firstToken = Node.getAbsoluteRaw().getFirstToken();
  auto lastToken = Node.getAbsoluteRaw().getLastToken();
  assert(firstToken.hasValue() == lastToken.hasValue() &&
         "Either the node contains present tokens or not");
  if (firstToken && lastToken) {
    assert(!firstToken->getRaw()->isMissing() &&
           !lastToken->getRaw()->isMissing());
    // The node contains a non-missing token
    SourceLoc StartLoc = getLoc(*firstToken);
    SourceLoc EndLoc = getLoc(*lastToken);
    return SourceRange(StartLoc, EndLoc);
  }

  // The node does not contain any present tokens. In accordance with the
  // current best practice in the AST, we use the range of the *previous*
  // token to represent the node's source range in the AST.
  if (auto previousNode = Node.getPreviousNodeRef()) {
    auto previousToken = previousNode->getAbsoluteRaw().getLastToken();
    assert(previousToken && "getPreviousNode always returns a node which "
                            "contains a non-missing token");
    return SourceRange(getLoc(*previousToken));
  } else {
    // We could not determine the previous node from the parent because we have
    // reached the beginning of the (partial) tree. Return the location of the
    // token preceeding the tree.
    return SourceRange(PreviousTokLoc);
  }
}

CharSourceRange ASTGen::getRangeWithTrivia(const SyntaxRef &Node) {
  auto startOffset = Node.getAbsolutePositionBeforeLeadingTrivia().getOffset();
  auto endOffset = Node.getAbsoluteEndPositionAfterTrailingTrivia().getOffset();
  auto startLoc = TreeStartLoc.getAdvancedLoc(startOffset);
  auto length = endOffset - startOffset;
  return CharSourceRange(startLoc, length);
}

CharSourceRange ASTGen::getRangeWithoutTrivia(const SyntaxRef &Node) {
  auto startOffset = Node.getAbsolutePositionAfterLeadingTrivia().getOffset();
  auto endOffset =
      Node.getAbsoluteEndPositionBeforeTrailingTrivia().getOffset();
  auto startLoc = TreeStartLoc.getAdvancedLoc(startOffset);
  auto length = endOffset - startOffset;
  return CharSourceRange(startLoc, length);
}
