//===----------- LibSyntaxGenerator.h -------------------------------------===//
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

#ifndef SWIFT_PARSE_LIBSYNTAXGENERATOR_H
#define SWIFT_PARSE_LIBSYNTAXGENERATOR_H

#include "swift/Parse/HiddenLibSyntaxAction.h"
#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/ParsedRawSyntaxRecorder.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/SyntaxParse/SyntaxTreeCreator.h"

namespace swift {
// TODO: (swift-parse) remove when possible
/// Generates libSyntax nodes either by looking them up using
/// HiddenLibSyntaxAction (based on provided OpaqueSyntaxNode) or by recording
/// them with ParsedRawSyntaxRecorder.
class LibSyntaxGenerator {
  std::shared_ptr<SyntaxParseActions> Actions;
  ParsedRawSyntaxRecorder Recorder;

public:
  explicit LibSyntaxGenerator(std::shared_ptr<SyntaxParseActions> spActions)
      : Actions(std::move(spActions)), Recorder(Actions->getLibSyntaxAction(Actions)) {
  }

  /// Create a \c TokenSyntax from the raw data.
  TokenSyntax createToken(const ParsedRawSyntaxNode &Node) {
    assert(Node.isDeferredToken());
    // The HiddenLibSyntaxAction always creates RawSyntax nodes, even for
    // deferred nodes. We can thus simply return the node that has already been
    // created. Don't transfer ownership to the caller, however, since it is
    // just creating a new view into the syntax tree.
    RawSyntax *Raw = Actions->getLibSyntaxNodeFor(Node.getData());
    return makeRoot<TokenSyntax>(Raw);
  }

  /// Create a \c SyntaxNode from the raw data.
  template <typename SyntaxNode>
  SyntaxNode createNode(const ParsedRawSyntaxNode &Node) {
    assert(Node.isDeferredLayout());
    // The HiddenLibSyntaxAction always creates RawSyntax nodes, even for
    // deferred nodes. We can thus simply return the node that has already been
    // created. Don't transfer ownership to the caller, however, since it is
    // just creating a new view into the syntax tree.
    RawSyntax *Raw = Actions->getLibSyntaxNodeFor(Node.getData());
    return makeRoot<SyntaxNode>(Raw);
  }
  
  /// Create a \c TokenSyntax from the raw data.
  TokenSyntaxRef createTokenRef(const ParsedRawSyntaxNode &Node, SyntaxDataRef *DataMem) {
    assert(Node.isDeferredToken());
    // The HiddenLibSyntaxAction always creates RawSyntax nodes, even for
    // deferred nodes. We can thus simply return the node that has already been
    // created. Don't transfer ownership to the caller, however, since it is
    // just creating a new view into the syntax tree.
    RawSyntax *Raw = Actions->getLibSyntaxNodeFor(Node.getData());
    return makeRootRef<TokenSyntaxRef>(Raw, DataMem);
  }

  /// Create a \c SyntaxNode from the raw data.
  template <typename SyntaxNode>
  SyntaxNode createNodeRef(const ParsedRawSyntaxNode &Node, SyntaxDataRef *DataMem) {
    assert(Node.isDeferredLayout());
    // The HiddenLibSyntaxAction always creates RawSyntax nodes, even for
    // deferred nodes. We can thus simply return the node that has already been
    // created. Don't transfer ownership to the caller, however, since it is
    // just creating a new view into the syntax tree.
    RawSyntax *Raw = Actions->getLibSyntaxNodeFor(Node.getData());
    return makeRootRef<SyntaxNode>(Raw, DataMem);
  }

  /// Return the libSyntax node stored in the given opaque \c Node.
  /// Assumes that \c Node is of type \c HiddenNode.
  TokenSyntax getLibSyntaxTokenFor(OpaqueSyntaxNode Node) {
    return getLibSyntaxNodeFor<TokenSyntax>(Node);
  }

  /// Return the libSyntax node stored in the given opaque \c Node.
  /// Assumes that \c Node is of type \c HiddenNode.
  template <typename SyntaxNode>
  SyntaxNode getLibSyntaxNodeFor(OpaqueSyntaxNode Node) {
    return makeRoot<SyntaxNode>(Actions->getLibSyntaxNodeFor(Node));
  }

  /// Return the libSyntax node stored in the given opaque \c Node.
  /// Assumes that \c Node is of type \c HiddenNode.
  template <typename SyntaxNode>
  SyntaxNode getLibSyntaxNodeRefFor(OpaqueSyntaxNode Node, SyntaxDataRef *DataMem) {
    return makeRootRef<SyntaxNode>(Actions->getLibSyntaxNodeFor(Node), DataMem);
  }
};
} // namespace swift

#endif // SWIFT_PARSE_LIBSYNTAXGENERATOR_H
