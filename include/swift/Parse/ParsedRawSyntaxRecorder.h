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
      return ParsedRawSyntaxNode(node.isMissing(), Data,
                                 ParsedRawSyntaxNode::DataKind::Recorded);
    } else {
      assert(node.isDeferredToken());
      OpaqueSyntaxNode Data = SPActions->recordDeferredToken(node.getData());
      return ParsedRawSyntaxNode(node.isMissing(), Data,
                                 ParsedRawSyntaxNode::DataKind::Recorded);
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
    SourceLoc offset = tokRange.getStart().getAdvancedLoc(-leadingTrivia.size());
    unsigned length =
        leadingTrivia.size() + tokRange.getByteLength() + trailingTrivia.size();
    CharSourceRange range(offset, length);
    OpaqueSyntaxNode n =
        SPActions->recordToken(tokKind, leadingTrivia, trailingTrivia, range);
    return ParsedRawSyntaxNode(/*IsMissing=*/false, n,
                               ParsedRawSyntaxNode::DataKind::Recorded);
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
  size_t ByteLength = 0;
  if (!elements.empty()) {
    for (auto &subnode : elements) {
      if (subnode.isNull()) {
        subnodes.push_back(nullptr);
      } else if (subnode.isRecorded()) {
        ByteLength += subnode.getByteLength(SPActions.get());
        subnodes.push_back(subnode.takeData());
      } else {
        assert(subnode.isDeferredLayout() || subnode.isDeferredToken());
        ByteLength += subnode.getByteLength(SPActions.get());
        auto recorded = recordDeferredNode(subnode);
        subnodes.push_back(recorded.takeData());
      }
    }
  }
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, subnodes, ByteLength);
  return ParsedRawSyntaxNode(/*IsMissing=*/false, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
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
               SyntaxParsingContext &ctx) {
    assert(k != syntax::SyntaxKind::Token &&
           "Use makeDeferredToken to create deferred tokens");
    if (deferredNodes.empty()) {
      OpaqueSyntaxNode Data =
          SPActions->makeDeferredLayout(k, /*ByteLength=*/0, /*IsMissing=*/false, {});
      return ParsedRawSyntaxNode(Data,
                                 ParsedRawSyntaxNode::DataKind::DeferredLayout);
    }
    SmallVector<OpaqueSyntaxNode, 4> children;

    size_t ByteLength = 0;
    for (auto &node : deferredNodes) {
      // Cached range.
      if (!node.isNull() && !node.isMissing()) {
        ByteLength += node.getByteLength(SPActions.get());
      }
      children.push_back(node.getData());
    }
    OpaqueSyntaxNode Data =
        SPActions->makeDeferredLayout(k, ByteLength, /*IsMissing=*/false, children);
    return ParsedRawSyntaxNode(Data,
                               ParsedRawSyntaxNode::DataKind::DeferredLayout);
  }

  /// Create a deferred token node.
  ParsedRawSyntaxNode makeDeferred(Token tok, StringRef leadingTrivia,
                                   StringRef trailingTrivia) {
    // Compute the range that includes the token and its trivia.
    SourceLoc rangeBegin =
        tok.getRange().getStart().getAdvancedLoc(-leadingTrivia.size());
    unsigned rangeLen = leadingTrivia.size() + tok.getRange().getByteLength() +
                        trailingTrivia.size();
    auto range = CharSourceRange(rangeBegin, rangeLen);

    OpaqueSyntaxNode Data = SPActionsP->makeDeferredToken(
        tok.getKind(), leadingTrivia, trailingTrivia, range, /*isMissing=*/false);
    return ParsedRawSyntaxNode(tok.getLoc(),
                               /*IsMissing=*/false, Data,
                               ParsedRawSyntaxNode::DataKind::DeferredToken);
  }

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
