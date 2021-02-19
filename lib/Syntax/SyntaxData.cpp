//===--- SyntaxData.cpp - Swift Syntax Data Implementation ----------------===//
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

#include "swift/Syntax/SyntaxData.h"

using namespace swift;
using namespace swift::syntax;

// MARK: - SyntaxDataRef

SyntaxOptional<SyntaxDataRef> SyntaxDataRef::getPreviousNodeRef() const {
  if (size_t N = getIndexInParent()) {
    if (hasParent()) {
      for (size_t I = N - 1;; --I) {
        if (auto C = getParentRef()->getChildRef(I)) {
          if (C->getRawRef()->isPresent() &&
              C->getAbsoluteRaw().getFirstToken() != None) {
            return C;
          }
        }
        if (I == 0) {
          break;
        }
      }
    }
  }
  return hasParent() ? getParentRef()->getPreviousNodeRef() : None;
}

SyntaxOptional<SyntaxDataRef> SyntaxDataRef::getNextNodeRef() const {
  if (hasParent()) {
    size_t NumChildren = getParentRef()->getNumChildren();
    for (size_t I = getIndexInParent() + 1; I != NumChildren; ++I) {
      if (auto C = getParentRef()->getChildRef(I)) {
        if (C->getRawRef()->isPresent() &&
            C->getAbsoluteRaw().getFirstToken() != None) {
          return C;
        }
      }
    }
    return getParentRef()->getNextNodeRef();
  }
  return None;
}

AbsoluteOffsetPosition
SyntaxDataRef::getAbsolutePositionBeforeLeadingTrivia() const {
  return AbsoluteRaw.getPosition();
}

AbsoluteOffsetPosition
SyntaxDataRef::getAbsolutePositionAfterLeadingTrivia() const {
  if (auto FirstToken = getAbsoluteRaw().getFirstToken()) {
    return getAbsolutePositionBeforeLeadingTrivia().advancedBy(
        FirstToken->getRaw()->getLeadingTriviaLength());
  } else {
    return getAbsolutePositionBeforeLeadingTrivia();
  }
}

AbsoluteOffsetPosition
SyntaxDataRef::getAbsoluteEndPositionBeforeTrailingTrivia() const {
  if (auto LastToken = getAbsoluteRaw().getLastToken()) {
    return getAbsoluteEndPositionAfterTrailingTrivia().advancedBy(
        -LastToken->getRaw()->getTrailingTriviaLength());
  } else {
    return getAbsoluteEndPositionAfterTrailingTrivia();
  }
}

AbsoluteOffsetPosition
SyntaxDataRef::getAbsoluteEndPositionAfterTrailingTrivia() const {
  return getAbsolutePositionBeforeLeadingTrivia().advancedBy(
      getRawRef()->getTextLength());
}

void SyntaxDataRef::dump(llvm::raw_ostream &OS) const {
  getRawRef()->dump(OS, 0);
  OS << '\n';
}

void SyntaxDataRef::dump() const { dump(llvm::errs()); }

// MARK: - SyntaxData

SyntaxOptional<SyntaxData>
SyntaxData::getChild(AbsoluteSyntaxPosition::IndexInParentType Index) const {
  auto AbsoluteRaw = getAbsoluteRaw().getChild(Index);
  if (AbsoluteRaw) {
    return SyntaxData(std::move(*AbsoluteRaw), /*Parent=*/*this);
  } else {
    return None;
  }
}

SyntaxOptional<SyntaxData> SyntaxData::getPreviousNode() const {
  if (size_t N = getIndexInParent()) {
    if (hasParent()) {
      for (size_t I = N - 1; ; --I) {
        if (auto C = getParent()->getChild(I)) {
          if (C->getRaw()->isPresent() && C->getFirstToken() != None) {
            return C;
          }
        }
        if (I == 0) {
          break;
        }
      }
    }
  }
  return hasParent() ? getParent()->getPreviousNode() : None;
}

SyntaxOptional<SyntaxData> SyntaxData::getNextNode() const {
  if (hasParent()) {
    size_t NumChildren = getParent()->getNumChildren();
    for (size_t I = getIndexInParent() + 1; I != NumChildren; ++I) {
      if (auto C = getParent()->getChild(I)) {
        if (C->getRaw()->isPresent() && C->getFirstToken() != None) {
          return C;
        }
      }
    }
    return getParent()->getNextNode();
  }
  return None;
}

SyntaxOptional<SyntaxData> SyntaxData::getFirstToken() const {
  /// getFirstToken and getLastToken cannot be implemented on SyntaxDataRef
  /// because we might need to traverse through multiple nodes to reach the
  /// first token. When returning this token, the parent nodes are being
  /// discarded and thus its parent pointer would point to invalid memory.
  if (getRawRef()->isToken() && !getRawRef()->isMissing()) {
    return *this;
  }

  for (size_t I = 0, E = getNumChildren(); I < E; ++I) {
    if (auto Child = getChild(I)) {
      if (Child->getRawRef()->isMissing()) {
        continue;
      }

      if (auto Token = Child->getFirstToken()) {
        return Token;
      }
    }
  }
  return None;
}

SyntaxOptional<SyntaxData> SyntaxData::getLastToken() const {
  // Also see comment in getFirstToken.
  if (getRawRef()->isToken() && !getRawRef()->isMissing()) {
    return *this;
  }

  if (getNumChildren() == 0) {
    return None;
  }
  for (int I = getNumChildren() - 1; I >= 0; --I) {
    if (auto Child = getChild(I)) {
      if (Child->getRawRef()->isMissing()) {
        continue;
      }

      if (auto Token = Child->getLastToken()) {
        return Token;
      }
    }
  }
  return None;
}

SyntaxData SyntaxData::replacingSelf(RawSyntax *NewRaw) const {
  if (hasParent()) {
    auto NewRoot = getParent()->replacingChild(NewRaw, getIndexInParent());
    auto NewSelf = getAbsoluteRaw().replacingSelf(
        NewRaw, NewRoot.AbsoluteRaw.getNodeId().getRootId());
    return SyntaxData(NewSelf, NewRoot);
  } else {
    auto NewSelf = AbsoluteRawSyntax::forRoot(NewRaw);
    return SyntaxData(NewSelf, /*Parent=*/nullptr);
  }
}
