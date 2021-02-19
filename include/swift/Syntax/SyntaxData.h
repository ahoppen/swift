//===--- SyntaxData.h - Swift Syntax Data Interface -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the SyntaxData interface, the type for the instance
// data for Syntax nodes.
//
// Effectively, these provide two main things to a Syntax node - parental
// relationships and caching for its children.
//
// A SyntaxData contains at least a strong reference to the RawSyntax,
// from which most information comes, and additionally a weak reference to
// its parent and the "index" at which it occurs in its parent. These were
// originally intended to have the important public APIs for structured
// editing but now contain no significant or public API; for those, see the
// Syntax type. These are purely to contain data, hence the name.
//
// Conceptually, SyntaxData add the characteristic of specific identity in a
// piece of Swift source code. While the RawSyntax for the integer literal
// token '1' can be reused anywhere a '1' occurs and has identical formatting,
// a SyntaxData represents *a* specific '1' at a particular location in
// Swift source.
//
// These are effectively internal implementation. For all public APIs, look
// for the type without "Data" in its name. For example, a StructDeclSyntaxData
// expresses its API through the wrapping StructDeclSyntax type.
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_SYNTAX_SYNTAXDATA_H
#define SWIFT_SYNTAX_SYNTAXDATA_H

#include "swift/Basic/Debug.h"
#include "swift/Syntax/AbsoluteRawSyntax.h"
#include "swift/Syntax/AtomicCache.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "llvm/ADT/DenseMap.h"

#include <atomic>

namespace swift {
namespace syntax {

/// A reference counted box that can contain any type.
template <typename T>
class RefCountedBox final
    : public llvm::ThreadSafeRefCountedBase<RefCountedBox<T>> {
public:
  const T Data;

  RefCountedBox(const T Data) : Data(Data) {}

  static RC<RefCountedBox<T>> make(const T &Data) {
    return RC<RefCountedBox<T>>{new RefCountedBox(Data)};
  }
};


class SyntaxDataRefBase {
  template<typename T>
  friend class SyntaxOptional;
protected:
  AbsoluteRawSyntax AbsoluteRaw;

  /// The parent can be stored either ref-counted or unsafe by a direct pointer.
  /// Either of those must always be \c nullptr. If both are \c nullptr, then
  /// the node does not have a parent.
  llvm::PointerIntPair<SyntaxDataRefBase *, 1, /*IsOwned*/bool> Parent;
  RC<SyntaxArena> Arena;

private:
  SyntaxDataRefBase(std::nullptr_t) : AbsoluteRaw(nullptr), Parent(nullptr, false), Arena(nullptr) {}
  
  bool isNull() const {
    return AbsoluteRaw.getRaw() == nullptr;
  }
  
protected:
  
  SyntaxDataRefBase(const AbsoluteRawSyntax &AbsoluteRaw, llvm::PointerIntPair<SyntaxDataRefBase *, 1, /*IsOwned*/bool> Parent, const RC<SyntaxArena> Arena) : AbsoluteRaw(AbsoluteRaw), Parent(Parent), Arena(Arena) {}
  
  SyntaxDataRefBase(const SyntaxDataRefBase &Other) : AbsoluteRaw(Other.AbsoluteRaw), Parent(
     (Other.Parent.getInt() && Other.Parent.getPointer()) ? new SyntaxDataRefBase(*Other.Parent.getPointer()) : Other.Parent.getPointer(),
     Other.Parent.getInt()
                                                                                     ), Arena(Other.Arena) {
  }
  SyntaxDataRefBase(SyntaxDataRefBase &&Other) : AbsoluteRaw(std::move(Other.AbsoluteRaw)), Parent(std::move(Other.Parent)), Arena(std::move(Other.Arena)) {
    Other.Parent.setPointer(nullptr);
    Other.Arena = nullptr;
  }
};

template<typename T>
class SyntaxOptional {
public:
  SyntaxDataRefBase Storage;
  
  SyntaxOptional() : Storage(nullptr) {}
  SyntaxOptional(llvm::NoneType) : Storage(nullptr) {}
  
  SyntaxOptional(const T &y) : Storage(y) {}
  SyntaxOptional(const SyntaxOptional &O) = default;

  SyntaxOptional(T &&y) : Storage(std::move(y)) {}
  SyntaxOptional(SyntaxOptional &&O) = default;

  SyntaxOptional &operator=(T &&y) {
    Storage = std::move(y);
    return *this;
  }
  SyntaxOptional &operator=(SyntaxOptional &&O) = default;

//  /// Create a new object by constructing it in place with the given arguments.
//  template <typename... ArgTypes> void emplace(ArgTypes &&... Args) {
//    Storage.emplace(std::forward<ArgTypes>(Args)...);
//  }

  static inline SyntaxOptional create(const T *y) {
    return y ? SyntaxOptional(*y) : SyntaxOptional();
  }

  SyntaxOptional &operator=(const T &y) {
    Storage = y;
    return *this;
  }
  SyntaxOptional &operator=(const SyntaxOptional &O) = default;

  void reset() { Storage = SyntaxDataRefBase(nullptr); }

  const T *getPointer() const { return static_cast<const T *>(&Storage); }
  T *getPointer() { return static_cast<T *>(&Storage); }
  const T &getValue() const LLVM_LVALUE_FUNCTION { return static_cast<const T &>(Storage); }
  T &getValue() LLVM_LVALUE_FUNCTION { return static_cast<T &>(Storage); }

  explicit operator bool() const { return hasValue(); }
  bool hasValue() const { return !Storage.isNull(); }
  const T *operator->() const { return getPointer(); }
  T *operator->() { return getPointer(); }
  const T &operator*() const LLVM_LVALUE_FUNCTION { return getValue(); }
  T &operator*() LLVM_LVALUE_FUNCTION { return getValue(); }
//
//  template <typename U>
//  constexpr T getValueOr(U &&value) const LLVM_LVALUE_FUNCTION {
//    return hasValue() ? getValue() : std::forward<U>(value);
//  }
//
//  /// Apply a function to the value if present; otherwise return None.
//  template <class Function>
//  auto map(const Function &F) const LLVM_LVALUE_FUNCTION
//      -> Optional<decltype(F(getValue()))> {
//    if (*this) return F(getValue());
//    return None;
//  }
//
#if LLVM_HAS_RVALUE_REFERENCE_THIS
  T &&getValue() && { return static_cast<T &&>(std::move(Storage)); }
  T &&operator*() && { return static_cast<T &&>(std::move(Storage)); }

//  template <typename U>
//  T getValueOr(U &&value) && {
//    return hasValue() ? std::move(getValue()) : std::forward<U>(value);
//  }
//
//  /// Apply a function to the value if present; otherwise return None.
//  template <class Function>
//  auto map(const Function &F) &&
//      -> Optional<decltype(F(std::move(*this).getValue()))> {
//    if (*this) return F(std::move(*this).getValue());
//    return None;
//  }
#endif
};

template<typename T>
bool operator==(const SyntaxOptional<T> &X, NoneType) {
  return !X;
}

template<typename T>
bool operator==(NoneType, const SyntaxOptional<T> &X) {
  return X == None;
}

template<typename T>
bool operator!=(const SyntaxOptional<T> &X, NoneType) {
  return !(X == None);
}

template<typename T>
bool operator!=(NoneType, const SyntaxOptional<T> &X) {
  return X != None;
}

/// The class for holding parented syntax.
///
/// This structure should not contain significant public
/// API or internal modification API.
///
/// It is essentially a wrapper around \c AbsoluteRawSyntax that also keeps
/// track of the parent.
///
/// The parent can either be stored ref-counted for memory-safety access or as
/// a plain pointer if it can be guaranteed that the parent will always outlive
/// the child.
/// If memory-safety should be guranteed, use \c SyntaxData, which gurantees
/// that the parent is always stored ref-counted.
///
/// Having \c SyntaxData be a sublcass of \c SyntaxDataRef means that we can
/// write algorithms that operate on \c SyntaxDataRef. When invoking those with
/// a \c SyntaxData node, we can efficiently demote the \c SyntaxData node to a
/// \c SyntaxDataRef.
///
/// We also uphold the following invariant: If a node's parent is ref-counted,
/// then all of its parent's parents are also ref-counted. This means that we
/// can address a subtree of a ref-counted syntax tree in a fast, but unsafe
/// unowned way, but we can never address a subtree of an unowned tree as
/// ref-counted.
class SyntaxDataRef: public SyntaxDataRefBase {
  friend class SyntaxData;

  /// Create an unowned \c SyntaxDataRef.
  /// \p AbsoluteRaw must not be reference-counted.
  SyntaxDataRef(const AbsoluteRawSyntax &AbsoluteRaw, SyntaxDataRef *Parent, bool IsParentOwned)
  : SyntaxDataRefBase(AbsoluteRaw, {Parent, IsParentOwned}
    , Parent ? nullptr : AbsoluteRaw.getRaw()->getArena()) {
  }
  SyntaxDataRef(AbsoluteRawSyntax &&AbsoluteRaw, SyntaxDataRef *Parent, bool IsParentOwned)
  : SyntaxDataRefBase(std::move(AbsoluteRaw), {Parent, IsParentOwned}
    , Parent ? nullptr : AbsoluteRaw.getRaw()->getArena()) {
  }

public:
  SyntaxDataRef(const SyntaxDataRef &Other) : SyntaxDataRefBase(Other) {
  }
  SyntaxDataRef(SyntaxDataRef &&Other) : SyntaxDataRefBase(std::move(Other)) {
  }
  
  ~SyntaxDataRef() {
    if (Parent.getInt() && Parent.getPointer()) {
      delete Parent.getPointer();
    }
  }
  
  // MARK: - Retrieving underlying storage

  const AbsoluteRawSyntax &getAbsoluteRaw() const { return AbsoluteRaw; }

  /// Returns the raw syntax node for this syntax node.
  const RawSyntax *getRawRef() const { return getAbsoluteRaw().getRaw(); }

  // MARK: - Retrieving related nodes

  SyntaxOptional<SyntaxDataRef> getParentRef() const {
    if (Parent.getPointer()) {
      return *static_cast<SyntaxDataRef *>(Parent.getPointer());
    } else {
      return None;
    }
  }

  /// Returns true if this syntax node has a parent.
  bool hasParent() const { return getParentRef().hasValue(); }

  /// Returns the number of children this SyntaxData has.
  size_t getNumChildren() const { return getRawRef()->getLayout().size(); }

  /// Gets the child at the index specified by the provided cursor.
  template <typename CursorType>
  SyntaxOptional<SyntaxDataRef> getChildRef(CursorType Cursor) const {
    return getChildRef(
        (AbsoluteSyntaxPosition::IndexInParentType)cursorIndex(Cursor));
  }

  /// Gets the child at the specified \p Index.
  SyntaxOptional<SyntaxDataRef>
  getChildRef(AbsoluteSyntaxPosition::IndexInParentType Index) const {
    auto AbsoluteRaw = getAbsoluteRaw().getChild(Index);
    if (AbsoluteRaw) {
      return SyntaxDataRef(*AbsoluteRaw, /*Parent=*/const_cast<SyntaxDataRef *>(this), /*IsParentOwned=*/false);
    } else {
      return None;
    }
  }

  /// Gets the child at the index specified by the provided cursor, assuming
  /// that the child exists.
  template <typename CursorType>
  SyntaxDataRef getPresentChildRef(CursorType Cursor) const {
    return getPresentChildRef(
        (AbsoluteSyntaxPosition::IndexInParentType)cursorIndex(Cursor));
  }

  /// Gets the child at the specified \p Index, assuming that the child exists.
  SyntaxDataRef
  getPresentChildRef(AbsoluteSyntaxPosition::IndexInParentType Index) const {
    auto AbsoluteRaw = getAbsoluteRaw().getPresentChild(Index);
    return SyntaxDataRef(std::move(AbsoluteRaw), /*Parent=*/const_cast<SyntaxDataRef *>(this), /*IsParentOwned=*/false);
  }

  /// Returns the child index of this node in its parent, if it has a parent,
  /// otherwise 0.
  AbsoluteSyntaxPosition::IndexInParentType getIndexInParent() const {
    return getAbsoluteRaw().getPosition().getIndexInParent();
  }
  
  /// Get the node immediately before this current node that does contain a
  /// non-missing token. Return \c None if we cannot find such node.
  SyntaxOptional<SyntaxDataRef> getPreviousNodeRef() const;

  /// Get the node immediately after this current node that does contain a
  /// non-missing token. Return \c None if we cannot find such node.
  SyntaxOptional<SyntaxDataRef> getNextNodeRef() const;

  // MARK: - Retrieving source locations

  /// Get the offset at which the leading trivia of this node starts.
  AbsoluteOffsetPosition getAbsolutePositionBeforeLeadingTrivia() const;

  /// Get the offset at which the content of this node (excluding leading
  /// trivia) starts.
  AbsoluteOffsetPosition getAbsolutePositionAfterLeadingTrivia() const;

  /// Get the offset at which the content (excluding trailing trivia) of this
  /// node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionBeforeTrailingTrivia() const;

  /// Get the offset at chiwh the trailing trivia of this node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionAfterTrailingTrivia() const;

  // MARK: - Getting the node's kind

  /// Returns the kind of syntax node this is.
  SyntaxKind getKind() const { return getRawRef()->getKind(); }

  /// Returns true if the data node represents type syntax.
  bool isType() const { return getRawRef()->isType(); }

  /// Returns true if the data node represents statement syntax.
  bool isStmt() const { return getRawRef()->isStmt(); }

  /// Returns true if the data node represents declaration syntax.
  bool isDecl() const { return getRawRef()->isDecl(); }

  /// Returns true if the data node represents expression syntax.
  bool isExpr() const { return getRawRef()->isExpr(); }

  /// Returns true if the data node represents pattern syntax.
  bool isPattern() const { return getRawRef()->isPattern(); }

  /// Returns true if this syntax is of some "unknown" kind.
  bool isUnknown() const { return getRawRef()->isUnknown(); }

  // MARK: - Miscellaneous

  /// Dump a debug description of the syntax data for debugging to
  /// standard error.
  void dump(llvm::raw_ostream &OS) const;

  SWIFT_DEBUG_DUMP;
};

class SyntaxData : public SyntaxDataRef {

  SyntaxData(AbsoluteRawSyntax AbsoluteRaw, const SyntaxData &Parent)
    : SyntaxDataRef(AbsoluteRaw, new SyntaxData(Parent), /*IsParentOwned=*/true) {
  }

public:
  // MARK: - Creating new SyntaxData

  SyntaxData(const SyntaxData &Other) : SyntaxDataRef(Other) {}
  SyntaxData(SyntaxData &&Other) : SyntaxDataRef(std::move(Other)) {}
  
  /// Create a \c SyntaxData for a tree's root (i.e. a node without a parent).
  SyntaxData(const AbsoluteRawSyntax &AbsoluteRaw, std::nullptr_t Parent)
      : SyntaxDataRef(AbsoluteRaw, nullptr, /*IsParentOwned=*/false) {}
  SyntaxData(AbsoluteRawSyntax &&AbsoluteRaw, std::nullptr_t Parent)
      : SyntaxDataRef(std::move(AbsoluteRaw), nullptr, /*IsParentOwned=*/false) {}

  /// Cast a \c SyntaxDataRef to a \c SyntaxData. This requires that \c Ref is
  /// known to be reference counted.
  explicit SyntaxData(const SyntaxDataRef &Ref) : SyntaxDataRef(Ref) {
  }

  // MARK: - Retrieving underlying storage
  
  /// Returns the raw syntax node for this syntax node.
  RawSyntax *getRaw() const { return getAbsoluteRaw().getRaw(); }

  // MARK: - Retrieving related nodes

  /// Return the parent syntax if there is one.
  SyntaxOptional<SyntaxData> getParent() const {
    if (auto ParentRef = getParentRef()) {
      return SyntaxData(*ParentRef);
    } else {
      return None;
    }
  }

  /// Gets the child at the index specified by the provided cursor.
  template <typename CursorType>
  SyntaxOptional<SyntaxData> getChild(CursorType Cursor) const {
    return getChild(
        (AbsoluteSyntaxPosition::IndexInParentType)cursorIndex(Cursor));
  }

  /// Gets the child at the specified \p Index.
  SyntaxOptional<SyntaxData>
  getChild(AbsoluteSyntaxPosition::IndexInParentType Index) const;

  /// Get the node immediately before this current node that contains a
  /// non-missing token. Return \c None if we cannot find such node.
  SyntaxOptional<SyntaxData> getPreviousNode() const;

  /// Get the node immediately after this current node that contains a
  /// non-missing token. Return \c None if we cannot find such node.
  SyntaxOptional<SyntaxData> getNextNode() const;

  /// Get the first non-missing token node in this tree. Return \c None if
  /// this node does not contain non-missing tokens.
  SyntaxOptional<SyntaxData> getFirstToken() const;

  /// Get the last non-missing token node in this tree. Return \c None if
  /// this node does not contain non-missing tokens.
  SyntaxOptional<SyntaxData> getLastToken() const;

  // MARK: - Modifying node

  /// With a new \c RawSyntax node, create a new node from this one and
  /// recursively rebuild the parental chain up to the root.
  SyntaxData replacingSelf(RawSyntax *NewRaw) const;

  /// Replace a child in the raw syntax and recursively rebuild the
  /// parental chain up to the root.
  template <typename CursorType>
  SyntaxData replacingChild(RawSyntax *RawChild,
                            CursorType ChildCursor) const {
    auto NewRaw = getRaw()->replacingChild(ChildCursor, RawChild);
    return replacingSelf(NewRaw);
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAXDATA_H
