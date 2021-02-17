//===--- ParsedRawSyntaxRecorder.cpp - Raw Syntax Parsing Recorder --------===//
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
//
// This file defines the ParsedRawSyntaxRecorder, which is the interface the
// parser is using to pass parsed syntactic elements to a SyntaxParseActions
// receiver and get a ParsedRawSyntaxNode object back.
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/ParsedRawSyntaxRecorder.h"
#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"

using namespace swift;
using namespace swift::syntax;

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordMissingToken(tok tokenKind, SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordMissingToken(tokenKind, loc);
  return ParsedRawSyntaxNode(SyntaxKind::Token,
                             /*isMissing=*/true, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordEmptyRawSyntaxCollection(SyntaxKind kind,
                                                        SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, {}, /*ByteLength=*/0);
  return ParsedRawSyntaxNode(kind, /*IsMissing=*/false, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::makeDeferredMissing(tok tokKind, SourceLoc loc) {
  OpaqueSyntaxNode Data = SPActions->makeDeferredToken(
      tokKind, /*leadingTrivia=*/StringRef(), /*trailingTrivia=*/StringRef(),
      CharSourceRange(loc, /*ByteLength=*/0), /*isMissing=*/true);
  return ParsedRawSyntaxNode(
      loc, /*IsMissing=*/true,
      Data, ParsedRawSyntaxNode::DataKind::DeferredToken);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::getDeferredChild(const ParsedRawSyntaxNode &parent,
                                          size_t childIndex) const {
  assert(parent.isDeferredLayout());
  auto childInfo = SPActions->getDeferredChild(parent.getData(), childIndex);
  return ParsedRawSyntaxNode(
      childInfo.SyntaxKind,
      /*IsMissing=*/false, childInfo.Data,
      childInfo.SyntaxKind == SyntaxKind::Token
          ? ParsedRawSyntaxNode::DataKind::DeferredToken
          : ParsedRawSyntaxNode::DataKind::DeferredLayout);
}

void ParsedRawSyntaxRecorder::discardRecordedNode(ParsedRawSyntaxNode &node) {
  SPActions->discardRecordedNode(node.takeData());
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::lookupNode(size_t lexerOffset, SourceLoc loc,
                                    SyntaxKind kind) {
  size_t length;
  OpaqueSyntaxNode n;
  std::tie(length, n) = SPActions->lookupNode(lexerOffset, kind);
  if (length == 0) {
    return ParsedRawSyntaxNode::null();
  }
  CharSourceRange range{loc, unsigned(length)};
  return ParsedRawSyntaxNode(kind, /*IsMissing=*/false, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
}
