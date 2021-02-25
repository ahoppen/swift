//===--- SyntaxCollection.h -------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SYNTAX_SYNTAXCOLLECTION_H
#define SWIFT_SYNTAX_SYNTAXCOLLECTION_H

#include "swift/Syntax/Syntax.h"

#include <iterator>

namespace swift {
namespace syntax {

template <SyntaxKind CollectionKind, typename Element>
class SyntaxCollectionRef;

template <SyntaxKind CollectionKind, typename Element>
struct SyntaxCollectionRefIterator {
  const SyntaxCollectionRef<CollectionKind, Element> &Collection;
  size_t Index;

  OwnedSyntaxRef<Element> operator*() {
    return Collection.getChild(Index);
  }

  SyntaxCollectionRefIterator<CollectionKind, Element> &operator++() {
    ++Index;
    return *this;
  }

  bool operator==(
      const SyntaxCollectionRefIterator<CollectionKind, Element> &Other) {
    return Collection.hasSameIdentityAs(Other.Collection) &&
           Index == Other.Index;
  }

  bool operator!=(
      const SyntaxCollectionRefIterator<CollectionKind, Element> &Other) {
    return !operator==(Other);
  }
};

/// A generic unbounded collection of \c SyntaxRef nodes.
template <SyntaxKind CollectionKind, typename Element>
class SyntaxCollectionRef : public SyntaxRef {
  friend class Syntax;

public:
  SyntaxCollectionRef(SyntaxDataRef *Data) : SyntaxRef(Data) {}

  /// Returns true if the collection is empty.
  bool empty() const { return size() == 0; }

  /// Returns the number of elements in the collection.
  size_t size() const { return getRaw()->getLayout().size(); }

  SyntaxCollectionRefIterator<CollectionKind, Element> begin() const {
    return SyntaxCollectionRefIterator<CollectionKind, Element>{
        /*Collection=*/*this,
        /*Index=*/0,
    };
  }

  SyntaxCollectionRefIterator<CollectionKind, Element> end() const {
    return SyntaxCollectionRefIterator<CollectionKind, Element>{
        /*Collection=*/*this,
        /*Index=*/getRaw()->getLayout().size(),
    };
  }
  
  /// Return the element at the given Index.
  ///
  /// Precondition: Index < size()
  /// Precondition: !empty()
  OwnedSyntaxRef<Element> getChild(const size_t Index) const {
    assert(Index < size());
    OwnedSyntaxRef<Element> Result;
    getDataRef().getChildRef(Index, Result.getDataPtr());
    return Result;
  }

  static bool kindof(SyntaxKind Kind) { return Kind == CollectionKind; }

  static bool classof(const SyntaxRef *S) { return kindof(S->getKind()); }
};

template <SyntaxKind CollectionKind, typename Element>
class SyntaxCollection;

template <SyntaxKind CollectionKind, typename Element>
struct SyntaxCollectionIterator {
  const SyntaxCollection<CollectionKind, Element> &Collection;
  size_t Index;

  Element operator*() {
    return Collection[Index];
  }

  SyntaxCollectionIterator<CollectionKind, Element> &operator++() {
    ++Index;
    return *this;
  }

  bool
  operator==(const SyntaxCollectionIterator<CollectionKind, Element> &Other) {
    return Collection.hasSameIdentityAs(Other.Collection) &&
           Index == Other.Index;
  }

  bool
  operator!=(const SyntaxCollectionIterator<CollectionKind, Element> &Other) {
    return !operator==(Other);
  }
};

/// A generic unbounded collection of syntax nodes
template <SyntaxKind CollectionKind, typename Element>
class SyntaxCollection : public Syntax {
  friend struct SyntaxFactory;
  friend class Syntax;

private:
  static SyntaxData makeData(std::initializer_list<Element> &Elements) {
    std::vector<RawSyntax *> List;
    List.reserve(Elements.size());
    for (auto &Elt : Elements)
      List.push_back(Elt.getRaw());
    auto Raw = RawSyntax::makeAndCalcLength(CollectionKind, List,
                                            SourcePresence::Present);
    return SyntaxData(AbsoluteRawSyntax::forRoot(Raw), /*Parent=*/nullptr);
  }

public:
  SyntaxCollection(const RC<SyntaxData> &Data) : Syntax(Data) {}
  
  SyntaxCollection(std::initializer_list<Element> list):
    SyntaxCollection(SyntaxCollection::makeData(list)) {}

  /// Returns true if the collection is empty.
  bool empty() const {
    return size() == 0;
  }

  /// Returns the number of elements in the collection.
  size_t size() const {
    return getRaw()->getLayout().size();
  }

  SyntaxCollectionIterator<CollectionKind, Element> begin() const {
    return SyntaxCollectionIterator<CollectionKind, Element>{
        /*Collection=*/*this,
        /*Index=*/0,
    };
  }

  SyntaxCollectionIterator<CollectionKind, Element> end() const {
    return SyntaxCollectionIterator<CollectionKind, Element>{
        /*Collection=*/*this,
        /*Index=*/getRaw()->getLayout().size(),
    };
  }

  /// Return the element at the given Index.
  ///
  /// Precondition: Index < size()
  /// Precondition: !empty()
  Element operator[](const size_t Index) const {
    assert(Index < size());
    return Element(*getData().getChild(Index));
  }

  /// Return a new collection with the given element added to the end.
  SyntaxCollection<CollectionKind, Element>
  appending(Element E) const {
    auto OldLayout = getRaw()->getLayout();
    std::vector<RawSyntax *> NewLayout;
    NewLayout.reserve(OldLayout.size() + 1);
    std::copy(OldLayout.begin(), OldLayout.end(), back_inserter(NewLayout));
    NewLayout.push_back(E.getRaw());
    auto Raw = RawSyntax::makeAndCalcLength(CollectionKind, NewLayout, getRaw()->getPresence());
    return SyntaxCollection<CollectionKind, Element>(
        getData().replacingSelf(Raw));
  }

  /// Return a new collection with an element removed from the end.
  ///
  /// Precondition: !empty()
  SyntaxCollection<CollectionKind, Element> removingLast() const {
    assert(!empty());
    auto NewLayout = getRaw()->getLayout().drop_back();
    auto Raw = RawSyntax::makeAndCalcLength(CollectionKind, NewLayout, getRaw()->getPresence());
    return SyntaxCollection<CollectionKind, Element>(
        getData().replacingSelf(Raw));
  }

  /// Return a new collection with the given element appended to the front.
  SyntaxCollection<CollectionKind, Element>
  prepending(Element E) const {
    auto OldLayout = getRaw()->getLayout();
    std::vector<RawSyntax *> NewLayout = { E.getRaw() };
    std::copy(OldLayout.begin(), OldLayout.end(),
              std::back_inserter(NewLayout));
    auto Raw = RawSyntax::makeAndCalcLength(CollectionKind, NewLayout, getRaw()->getPresence());
    return SyntaxCollection<CollectionKind, Element>(
        getData().replacingSelf(Raw));
  }

  /// Return a new collection with an element removed from the end.
  ///
  /// Precondition: !empty()
  SyntaxCollection<CollectionKind, Element> removingFirst() const {
    assert(!empty());
    auto NewLayout = getRaw()->getLayout().drop_front();
    auto Raw = RawSyntax::makeAndCalcLength(CollectionKind, NewLayout, getRaw()->getPresence());
    return SyntaxCollection<CollectionKind, Element>(
        getData().replacingSelf(Raw));
  }

  /// Return a new collection with the Element inserted at index i.
  ///
  /// Precondition: i <= size()
  SyntaxCollection<CollectionKind, Element>
  inserting(size_t i, Element E) const {
    assert(i <= size());
    auto OldLayout = getRaw()->getLayout();
    std::vector<RawSyntax *> NewLayout;
    NewLayout.reserve(OldLayout.size() + 1);

    std::copy(OldLayout.begin(), OldLayout.begin() + i,
              std::back_inserter(NewLayout));
    NewLayout.push_back(E.getRaw());
    std::copy(OldLayout.begin() + i, OldLayout.end(),
              std::back_inserter(NewLayout));
    auto Raw = RawSyntax::makeAndCalcLength(CollectionKind, NewLayout, getRaw()->getPresence());
    return SyntaxCollection<CollectionKind, Element>(
        getData().replacingSelf(Raw));
  }

  /// Return a new collection with the element removed at index i.
  SyntaxCollection<CollectionKind, Element> removing(size_t i) const {
    assert(i <= size());
    std::vector<RawSyntax *> NewLayout = getRaw()->getLayout();
    auto iterator = NewLayout.begin();
    std::advance(iterator, i);
    NewLayout.erase(iterator);
    auto Raw = RawSyntax::makeAndCalcLength(CollectionKind, NewLayout, getRaw()->getPresence());
    return SyntaxCollection<CollectionKind, Element>(
        getData().replacingSelf(Raw));
  }

  /// Return an empty syntax collection of this type.
  SyntaxCollection<CollectionKind, Element> cleared() const {
    auto Raw = RawSyntax::makeAndCalcLength(CollectionKind, {}, getRaw()->getPresence());
    return SyntaxCollection<CollectionKind, Element>(
        getData().replacingSelf(Raw));
  }

  static bool kindof(SyntaxKind Kind) {
    return Kind == CollectionKind;
  }

  static bool classof(const Syntax *S) {
    return kindof(S->getKind());
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAXCOLLECTION_H
