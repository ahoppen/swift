//===--- AbsoluteRawSyntax.h ------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SYNTAX_ABSOLUTERAWSYNTAX_H
#define SWIFT_SYNTAX_ABSOLUTERAWSYNTAX_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Syntax/RawSyntax.h"

namespace swift {
namespace syntax {

/// Type-safe wrapper around a \c size_t that describes a node's index in its
/// tree. This represents the index at which the node will be encountered during
/// a depth-first traversal of the tree.
class SyntaxIndexInTree {
  size_t IndexInTree;

  explicit SyntaxIndexInTree(size_t IndexInTree) : IndexInTree(IndexInTree) {}

public:
  static SyntaxIndexInTree zero() { return SyntaxIndexInTree(0); }

  /// Assuming that this index points to the start of \p Raw, advance it so that
  /// it points to the next sibling of \p Raw.
  SyntaxIndexInTree advancedBy(RawSyntax *Raw) const {
    auto NewIndexInTree = IndexInTree;
    if (Raw) {
      NewIndexInTree += Raw->getTotalNodes();
    }
    return SyntaxIndexInTree(NewIndexInTree);
  }

  /// Assuming that this index points to the next sibling of \p Raw, reverse it
  /// so that it points to the start of \p Raw.
  SyntaxIndexInTree reversedBy(RawSyntax *Raw) const {
    auto NewIndexInTree = IndexInTree;
    if (Raw) {
      NewIndexInTree -= Raw->getTotalNodes();
    }
    return SyntaxIndexInTree(NewIndexInTree);
  }

  /// Advance this index to point to its first immediate child.
  SyntaxIndexInTree advancedToFirstChild() const {
    auto NewIndexInTree = IndexInTree + 1;
    return SyntaxIndexInTree(NewIndexInTree);
  }

  bool operator==(SyntaxIndexInTree Other) const {
    return IndexInTree == Other.IndexInTree;
  }
};

/// A syntax identifier that globally identifies a \c Syntax node.
/// In contrast to \c NodeId of \c RawSyntax, this also takes into account the
/// node's position in the tree. For example, a source file may contain two
/// int literals with value 0. The corresponding \c RawSyntax nodes can share
/// the same \c NodeId. However, because the literals occur in different
/// locations of the syntax tree, their \c SyntaxIdentifiers are different.
class SyntaxIdentifier {
public:
  using RootIdType = size_t;

private:
  static std::atomic<RootIdType> NextUnusedRootId;

  /// An integer that identifies the tree in which the node represented by this
  /// identifier lives.
  RootIdType RootId;

  /// The position of the node represented by this identifier in the syntax
  /// tree.
  SyntaxIndexInTree IndexInTree;

public:
  SyntaxIdentifier(RootIdType RootId, SyntaxIndexInTree IndexInTree)
      : RootId(RootId), IndexInTree(IndexInTree) {
    assert(RootId < NextUnusedRootId && "New RootIds should only be created "
                                        "using the newRoot constructor.");
  }

  /// Create a SyntaxIdentifier that refers to the root of a new syntax tree.
  static SyntaxIdentifier newRoot() {
    return SyntaxIdentifier(NextUnusedRootId++, SyntaxIndexInTree::zero());
  }

  RootIdType getRootId() const { return RootId; }
  SyntaxIndexInTree getIndexInTree() const { return IndexInTree; }

  /// Assuming that this identifier points to the start of \p Raw, advance it so
  /// that it points to the next sibling of \p Raw.
  SyntaxIdentifier advancedBy(RawSyntax *Raw) const {
    auto NewIndexInTree = IndexInTree.advancedBy(Raw);
    return SyntaxIdentifier(RootId, NewIndexInTree);
  }

  /// Assuming that this identifier points to the next sibling of \p Raw,
  /// reverse it so that it points to the start of \p Raw.
  SyntaxIdentifier reversedBy(RawSyntax *Raw) const {
    auto NewIndexInTree = IndexInTree.reversedBy(Raw);
    return SyntaxIdentifier(RootId, NewIndexInTree);
  }

  /// Get the identifier of the first immediate child.
  SyntaxIdentifier advancedToFirstChild() const {
    auto NewIndexInTree = IndexInTree.advancedToFirstChild();
    return SyntaxIdentifier(RootId, NewIndexInTree);
  }

  bool operator==(SyntaxIdentifier Other) const {
    return RootId == Other.RootId && IndexInTree == Other.IndexInTree;
  }
};

/// Represents a node's position in a syntax tree, described by its overal
/// textual offset and the position within its parent.
class AbsoluteSyntaxPosition {
public:
  using OffsetType = uint32_t;
  using IndexInParentType = uint32_t;

private:
  /// The text offset where this node starts within its syntax tree, counted in
  /// UTF-8 bytes.
  OffsetType Offset;

  /// The node's index within its parent, i.e.
  /// `node.parent.childAt(IndexInParent) = node`.
  IndexInParentType IndexInParent;

public:
  AbsoluteSyntaxPosition(OffsetType Offset, IndexInParentType IndexInParent)
      : Offset(Offset), IndexInParent(IndexInParent) {}

  /// Create a new \c AbsoluteSyntaxPosition that refers to the root of a syntax
  /// tree.
  static AbsoluteSyntaxPosition forRoot() {
    return AbsoluteSyntaxPosition(0, 0);
  }

  OffsetType getOffset() const { return Offset; }
  IndexInParentType getIndexInParent() const { return IndexInParent; }

  /// Assuming that this position points to the start of \p Raw, advance it so
  /// that it points to the next sibling of \p Raw.
  AbsoluteSyntaxPosition advancedBy(RawSyntax *Raw) const {
    OffsetType NewOffset = Offset;
    if (Raw) {
      NewOffset += Raw->getTextLength();
    }
    IndexInParentType NewIndexInParent = IndexInParent + 1;
    return AbsoluteSyntaxPosition(NewOffset, NewIndexInParent);
  }

  /// Assuming that this position points to the next sibling of \p Raw, reverse
  /// it so that it points to the start of \p Raw.
  AbsoluteSyntaxPosition reversedBy(RawSyntax *Raw) const {
    OffsetType NewOffset = Offset;
    if (Raw) {
      NewOffset -= Raw->getTextLength();
    }
    IndexInParentType NewIndexInParent = IndexInParent - 1;
    return AbsoluteSyntaxPosition(NewOffset, NewIndexInParent);
  }

  /// Get the position of the node's first immediate child.
  AbsoluteSyntaxPosition advancedToFirstChild() const {
    return AbsoluteSyntaxPosition(Offset, 0);
  }
};

/// A type-safe wrapper that describes a node's textual position within a source
/// file, represented by its UTF-8 byte offset from the start.
class AbsoluteOffsetPosition {
  AbsoluteSyntaxPosition::OffsetType Offset;

public:
  explicit AbsoluteOffsetPosition(AbsoluteSyntaxPosition::OffsetType Offset)
      : Offset(Offset) {}
  AbsoluteOffsetPosition(AbsoluteSyntaxPosition Position)
      : Offset(Position.getOffset()) {}

  AbsoluteSyntaxPosition::OffsetType getOffset() const { return Offset; }

  /// Return a position that has been advanced by \p Advance UTF-8 bytes.s
  AbsoluteOffsetPosition advancedBy(int Advance) {
    return AbsoluteOffsetPosition(Offset + Advance);
  }
};

/// Various information that enrich a \c RawSyntax node with information on how
/// it's located within the syntax tree.
class AbsoluteSyntaxInfo {
  AbsoluteSyntaxPosition Position;
  SyntaxIdentifier NodeId;

public:
  AbsoluteSyntaxInfo(AbsoluteSyntaxPosition Position, SyntaxIdentifier NodeId)
      : Position(Position), NodeId(NodeId) {}

  static AbsoluteSyntaxInfo forRoot() {
    return AbsoluteSyntaxInfo(AbsoluteSyntaxPosition::forRoot(),
                              SyntaxIdentifier::newRoot());
  }

  AbsoluteSyntaxPosition getPosition() const { return Position; }
  SyntaxIdentifier getNodeId() const { return NodeId; }

  /// Assuming that this info points to the start of \p Raw, advance it so
  /// that it points to the next sibling of \p Raw.
  AbsoluteSyntaxInfo advancedBy(RawSyntax *Raw) const {
    auto NewNodeId = NodeId.advancedBy(Raw);
    auto NewPosition = Position.advancedBy(Raw);
    return AbsoluteSyntaxInfo(NewPosition, NewNodeId);
  }

  /// Assuming that this info points to the next sibling of \p Raw, reverse
  /// it so that it points to the start of \p Raw.
  AbsoluteSyntaxInfo reversedBy(RawSyntax *Raw) const {
    auto NewNodeId = NodeId.reversedBy(Raw);
    auto NewPosition = Position.reversedBy(Raw);
    return AbsoluteSyntaxInfo(NewPosition, NewNodeId);
  }

  /// Get the information of the node's first immediate child.
  AbsoluteSyntaxInfo advancedToFirstChild() const {
    auto NewNodeId = NodeId.advancedToFirstChild();
    auto NewPosition = Position.advancedToFirstChild();
    return AbsoluteSyntaxInfo(NewPosition, NewNodeId);
  }
};

/// A \c RawSyntax node that is enrichted with information of its position
/// within the syntax tree it lives in.
///
/// A \c AbsoluteRawSyntaxRef can store its \c RawSyntax either
/// reference-counted or as a plain pointer, if it can be guaranteed that \c
/// RawSyntax stays alive. If performance is not a critical concern and
/// memory-safety plays a prevailing role, use \c AbsoluteRawSyntax instead,
/// which guarantees that the \c RawSyntax is always stored ref-counted.
///
/// By having \c AbsoluteRawSyntax as a subclass of \c AbsoluteRawSyntaxRef
/// we can efficiently demote a reference-counted \c AbsoluteRawSyntax to a
/// potentially unowned \c AbsoluteRawSyntaxRef.
class AbsoluteRawSyntax {
  /// The reference to the \c RawSyntax can either be stored reference-counted
  /// (for safe access) or unowned (for faster access if it can be guaranteed
  /// that the raw syntax stays alive). Only one of the two following fields can
  /// be non-null at a time.
  RawSyntax *Raw;

  const AbsoluteSyntaxInfo Info;

public:
  AbsoluteRawSyntax(std::nullptr_t) : Raw(), Info(
    AbsoluteSyntaxPosition(0, 0),
    SyntaxIdentifier(0, SyntaxIndexInTree::zero())
  ) {}
public:
  AbsoluteRawSyntax(AbsoluteRawSyntax &&Other) = default;
  AbsoluteRawSyntax(const AbsoluteRawSyntax &Other)
      : Raw(Other.Raw), Info(Other.Info) {
  }
  
  /// Create a reference-counted \c AbsoluteRawSyntaxRef.
  /// \p Raw must not be \c nullptr.
  AbsoluteRawSyntax(RawSyntax *Raw, AbsoluteSyntaxInfo Info)
      : Raw(Raw), Info(Info) {
    assert(Raw != nullptr && "AbsoluteRawSyntax must have a RawSyntax");
  }
  
  /// Construct a \c AbsoluteRawSyntax for a \c RawSyntax node that represents
  /// the syntax tree's root.
  static AbsoluteRawSyntax forRoot(RawSyntax *Raw) {
    return AbsoluteRawSyntax(Raw, AbsoluteSyntaxInfo::forRoot());
  }

  /// Get a reference to the \c RawSyntax of this node.
  RawSyntax *getRaw() const {
    return Raw;
  }

  AbsoluteSyntaxInfo getInfo() const { return Info; }

  /// Get the position at which the leading triva of this node starts.
  AbsoluteSyntaxPosition getPosition() const { return Info.getPosition(); };

  SyntaxIdentifier getNodeId() const { return Info.getNodeId(); };

  AbsoluteSyntaxPosition::IndexInParentType getIndexInParent() const {
    return getPosition().getIndexInParent();
  }

  size_t getNumChildren() const { return getRaw()->getLayout().size(); }

  /// Get the child at \p Index.
  Optional<AbsoluteRawSyntax>
  getChild(AbsoluteSyntaxPosition::IndexInParentType Index) const {
    auto Raw = getRaw();
    auto RawChild = Raw->getChild(Index);
    if (!RawChild) {
      return None;
    }

    AbsoluteSyntaxPosition Position = getPosition().advancedToFirstChild();
    SyntaxIdentifier NodeId = getNodeId().advancedToFirstChild();

    for (size_t I = 0; I < Index; ++I) {
      Position = Position.advancedBy(Raw->getChild(I));
      NodeId = NodeId.advancedBy(Raw->getChild(I));
    }

    AbsoluteSyntaxInfo Info(Position, NodeId);
    return AbsoluteRawSyntax(RawChild, Info);
  }

  AbsoluteRawSyntax
  getPresentChild(AbsoluteSyntaxPosition::IndexInParentType Index) const {
    auto RawChild = getRaw()->getChild(Index);

    AbsoluteSyntaxPosition Position = getPosition().advancedToFirstChild();
    SyntaxIdentifier NodeId = getNodeId().advancedToFirstChild();

    for (size_t I = 0; I < Index; ++I) {
      Position = Position.advancedBy(getRaw()->getChild(I));
      NodeId = NodeId.advancedBy(getRaw()->getChild(I));
    }

    AbsoluteSyntaxInfo Info(Position, NodeId);
    return AbsoluteRawSyntax(RawChild, Info);
  }

  /// Get the first non-missing token node in this tree. Return \c None if
  /// this node does not contain non-missing tokens.
  Optional<AbsoluteRawSyntax> getFirstToken() const;

  /// Get the last non-missing token node in this tree. Return \c None if
  /// this node does not contain non-missing tokens.
  Optional<AbsoluteRawSyntax> getLastToken() const;
  
  /// Construct a new \c AbsoluteRawSyntax node that has the same info as the
  /// current one, but
  ///  - the \p NewRaw as the backing storage
  ///  - the \p NewRootId as the RootId
  AbsoluteRawSyntax
  replacingSelf(RawSyntax *NewRaw,
                SyntaxIdentifier::RootIdType NewRootId) const {
    SyntaxIdentifier NewNodeId(NewRootId, Info.getNodeId().getIndexInTree());
    AbsoluteSyntaxInfo NewInfo(Info.getPosition(), NewNodeId);
    return AbsoluteRawSyntax(NewRaw, NewInfo);
  }
};

} // end namespace syntax
} // end namespace swift

namespace llvm {
raw_ostream &operator<<(raw_ostream &OS,
                        swift::syntax::AbsoluteOffsetPosition Pos);
} // end namespace llvm

#endif // SWIFT_SYNTAX_ABSOLUTERAWSYNTAX_H
