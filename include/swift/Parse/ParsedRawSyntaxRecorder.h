//===--- ParsedRawSyntaxRecorder.h - Raw Syntax Parsing Recorder ----------===//
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

#ifndef SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H
#define SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H

#include "swift/Basic/LLVM.h"
#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/SyntaxParseActions.h"
#include <memory>

namespace swift {

class CharSourceRange;
class ParsedRawSyntaxNode;
struct ParsedTrivia;
class ParsedTriviaPiece;
class SyntaxParseActions;
class SyntaxParsingContext;
class SourceLoc;
class Token;
enum class tok;

namespace syntax {
  enum class SyntaxKind;
}

class ParsedRawSyntaxRecorder final {
  std::shared_ptr<SyntaxParseActions> SPActions;
  SyntaxParseActions *SPActionsP;

  /// Assuming that \p node is a deferred layout or token node, record it and
  /// return the recorded node.
  ParsedRawSyntaxNode recordDeferredNode(const ParsedRawSyntaxNode &node) {
    assert(!node.isNull() && !node.isRecorded());
    if (node.isDeferredLayout()) {
      OpaqueSyntaxNode Data = SPActions->recordDeferredLayout(node.getData());
      return ParsedRawSyntaxNode(Data, ParsedRawSyntaxNode::DataKind::Recorded);
    } else {
      assert(node.isDeferredToken());
      OpaqueSyntaxNode Data = SPActions->recordDeferredToken(node.getData());
      return ParsedRawSyntaxNode(Data, ParsedRawSyntaxNode::DataKind::Recorded);
    }
  }

public:
  explicit ParsedRawSyntaxRecorder(std::shared_ptr<SyntaxParseActions> spActions)
    : SPActions(std::move(spActions)) {
      SPActionsP = SPActions.get();
    }

  SyntaxParseActions *getActions() const {
    return SPActionsP;
  }
  
  ParsedRawSyntaxNode recordToken(const Token &tok, StringRef leadingTrivia,
                                  StringRef trailingTrivia) {
    return recordToken(tok.getKind(), tok.getRange(), leadingTrivia,
                       trailingTrivia);
  }

  ParsedRawSyntaxNode recordToken(tok tokKind, CharSourceRange tokRange,
                                  StringRef leadingTrivia,
                                  StringRef trailingTrivia) {
    OpaqueSyntaxNode n =
        SPActionsP->recordToken(tokKind, leadingTrivia, trailingTrivia, tokRange);
    return ParsedRawSyntaxNode(n, ParsedRawSyntaxNode::DataKind::Recorded);
  }

  /// Record a missing token. \p loc can be invalid or an approximate location
  /// of where the token would be if not missing.
  ParsedRawSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc);

  /// The provided \p elements are an exact layout appropriate for the syntax
  /// \p kind. Missing optional elements are represented with a null
  /// ParsedRawSyntaxNode object.
  ParsedRawSyntaxNode recordRawSyntax(syntax::SyntaxKind kind,
                                      MutableArrayRef<ParsedRawSyntaxNode> elements) {
  SmallVector<OpaqueSyntaxNode, 4> subnodes;
  if (!elements.empty()) {
    for (auto &subnode : elements) {
      if (subnode.isNull()) {
        subnodes.push_back(nullptr);
      } else if (subnode.isRecorded()) {
        subnodes.push_back(subnode.takeData());
      } else {
        assert(subnode.isDeferredLayout() || subnode.isDeferredToken());
        auto recorded = recordDeferredNode(subnode);
        subnodes.push_back(recorded.takeData());
      }
    }
  }
  OpaqueSyntaxNode n = SPActionsP->recordRawSyntax(kind, subnodes);
  return ParsedRawSyntaxNode(n, ParsedRawSyntaxNode::DataKind::Recorded);
}


  /// Record a raw syntax collecton without eny elements. \p loc can be invalid
  /// or an approximate location of where an element of the collection would be
  /// if not missing.
  ParsedRawSyntaxNode recordEmptyRawSyntaxCollection(syntax::SyntaxKind kind,
                                                     SourceLoc loc);

  /// Create a deferred layout node.
  ParsedRawSyntaxNode
  makeDeferred(syntax::SyntaxKind k,
               MutableArrayRef<ParsedRawSyntaxNode> deferredNodes,
               SyntaxParsingContext &ctx);

  /// Create a deferred token node.
  ParsedRawSyntaxNode makeDeferred(Token tok, StringRef leadingTrivia,
                                   StringRef trailingTrivia);

  /// Form a deferred missing token node.
  ParsedRawSyntaxNode makeDeferredMissing(tok tokKind, SourceLoc loc);

  /// For a deferred layout node \p parent, retrieve the deferred child node
  /// at \p ChildIndex.
  ParsedRawSyntaxNode getDeferredChild(const ParsedRawSyntaxNode &parent,
                                       size_t ChildIndex) const;

  void discardRecordedNode(ParsedRawSyntaxNode &node);

  /// Used for incremental re-parsing.
  ParsedRawSyntaxNode lookupNode(size_t lexerOffset, SourceLoc loc,
                                 syntax::SyntaxKind kind);

};

} // end namespace swift

#endif // SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H
