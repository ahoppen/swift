//===--- ParsedRawSyntaxNode.h - Parsed Raw Syntax Node ---------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_PARSEDRAWSYNTAXNODE_H
#define SWIFT_PARSE_PARSEDRAWSYNTAXNODE_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {

/// A opaque syntax node. It has the requirement that nullptr must always
/// represent a non-existent node.
typedef void *OpaqueSyntaxNode;
class SyntaxParsingContext;
class SyntaxParseActions;

/// Represents a raw syntax node formed by the parser.
///
/// It can be either 'recorded', in which case it encapsulates an
/// \c OpaqueSyntaxNode that was returned from a \c SyntaxParseActions
/// invocation, or 'deferred' which captures the data for a
/// \c SyntaxParseActions invocation to occur later.
///
/// An \c OpaqueSyntaxNode can represent both the result of 'recording' a token
/// as well as 'recording' a syntax layout, so there's only one
/// \c RecordedSyntaxNode structure that can represent both.
///
/// The 'deferred' form is used for when the parser is backtracking and when
/// there are instances that it's not clear what will be the final syntax node
/// in the current parsing context.
class ParsedRawSyntaxNode {
public:
  enum class DataKind : uint8_t {
    Null,
    Recorded,
    DeferredLayout,
    DeferredToken,
    /// The node has been destroyed because it has been moved somewhere else.
    Destroyed,
  };

private:
  friend class ParsedRawSyntaxRecorder;

  /// The opaque data created by \c SyntaxParseActions. Depending on \c DK, this
  /// can be data required to record a deferred node or the deferred node
  /// itself. The \c SyntaxParseActions that created this node need to be
  /// consulted to interpret the data.
  OpaqueSyntaxNode Data;

  DataKind DK;

  /// Create a deferred token node.
  ParsedRawSyntaxNode(SourceLoc tokLoc, OpaqueSyntaxNode Data, DataKind DK)
      : Data(Data),
        DK(DK) {
    assert(DK == DataKind::DeferredToken);
  }
  ParsedRawSyntaxNode(const ParsedRawSyntaxNode &other) = delete;
  ParsedRawSyntaxNode &operator=(const ParsedRawSyntaxNode &other) = delete;

public:
  ParsedRawSyntaxNode()
      : Data(nullptr),
        DK(DataKind::Null) {}

  /// Create an arbitrary syntax node. If \p is \c Token, \p tokKind must be set
  /// otherwise \p tokKind must be \c NUM_TOKENS.
  ParsedRawSyntaxNode(OpaqueSyntaxNode n, DataKind DK)
      : Data(n), DK(DK) {
  }

#ifndef NDEBUG
  bool ensureDataIsNotRecorded() {
    if (!isRecorded()) {
      return true;
    }
    llvm::dbgs() << "Leaking node: ";
    dump(llvm::dbgs());
    llvm::dbgs() << "\n";
    return false;
  }
#endif

  ParsedRawSyntaxNode &operator=(ParsedRawSyntaxNode &&other) {
    assert(ensureDataIsNotRecorded() &&
           "recorded data is being destroyed by assignment");
    Data = std::move(other.Data);
    DK = std::move(other.DK);
    other.reset();
    return *this;
  }
  ParsedRawSyntaxNode(ParsedRawSyntaxNode &&other)
      : Data(std::move(other.Data)),
        DK(std::move(other.DK)) {
    other.reset();
  }

  ~ParsedRawSyntaxNode() {
    assert(ensureDataIsNotRecorded() && "recorded data is being destructed");
  }

  syntax::SyntaxKind getKind(SyntaxParsingContext *SyntaxContext) const;
  tok getTokenKind(SyntaxParsingContext *SyntaxContext) const;

  /// Retrieve the opaque data of this node. This does not transfer ownership
  /// of the data. If the data is being consumed, use \p takeData to reset this
  /// node and supress any warnings about recorded nodes being destructed.
  OpaqueSyntaxNode getData() const { return Data; }

  /// Return the data of this node and reset it.
  OpaqueSyntaxNode takeData() {
    OpaqueSyntaxNode Data = this->Data;
    reset();
    return Data;
  }

  /// If this node is a deferred layout node, return the child at index \p
  /// ChildIndex.
  /// Note that this may be an expensive operation since the \c
  /// SyntaxParseAction, which created the node (implicitly passed via the
  /// \p SyntaxContext) needs to be consulted to retrieve the child.
  ParsedRawSyntaxNode
  getDeferredChild(size_t ChildIndex,
                   const SyntaxParsingContext *SyntaxContext) const;

  bool isToken(SyntaxParsingContext *SyntaxContext) const {
    return getKind(SyntaxContext) == syntax::SyntaxKind::Token;
  }
  bool isToken(tok tokKind, SyntaxParsingContext *SyntaxContext) const {
    return getTokenKind(SyntaxContext) == tokKind;
  }

  bool isNull() const {
    return DK == DataKind::Null;
  }

  bool isRecorded() const { return DK == DataKind::Recorded; }
  bool isDeferredLayout() const { return DK == DataKind::DeferredLayout; }
  bool isDeferredToken() const { return DK == DataKind::DeferredToken; }

  /// Primary used for a deferred missing token.
  bool isMissing(SyntaxParseActions *Actions) const;

  void reset() { DK = DataKind::Destroyed; }

  ParsedRawSyntaxNode unsafeCopy() const {
    ParsedRawSyntaxNode copy;
    copy.Data = Data;
    copy.DK = DK;
    return copy;
  }

  size_t getByteLength(SyntaxParseActions *Actions) const;

  //==========================================================================//

  /// Dump this piece of syntax recursively for debugging or testing.
  SWIFT_DEBUG_DUMP;

  /// Dump this piece of syntax recursively.
  void dump(raw_ostream &OS, unsigned Indent = 0) const;

  static ParsedRawSyntaxNode null() {
    return ParsedRawSyntaxNode{};
  }
};

} // end namespace swift

#endif
