//===--- AbsoluteRawSyntax.cpp ----------------------------------*- C++ -*-===//
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

#include "swift/Syntax/AbsoluteRawSyntax.h"

using namespace swift;
using namespace swift::syntax;

std::atomic<SyntaxIdentifier::RootIdType> SyntaxIdentifier::NextUnusedRootId(0);

Optional<AbsoluteRawSyntax> AbsoluteRawSyntax::getFirstToken() const {
  if (getRaw()->isToken() && !getRaw()->isMissing()) {
    return *this;
  }

  size_t NumChildren = getNumChildren();
  for (size_t I = 0; I < NumChildren; ++I) {
    if (auto Child = getChild(I)) {
      if (Child->getRaw()->isMissing()) {
        continue;
      }

      if (auto Token = Child->getFirstToken()) {
        return Token;
      }
    }
  }
  return None;
}

Optional<AbsoluteRawSyntax> AbsoluteRawSyntax::getLastToken() const {
  if (getRaw()->isToken() && !getRaw()->isMissing()) {
    return *this;
  }

  size_t NumChildren = getNumChildren();
  if (NumChildren == 0) {
    return None;
  }
  for (int I = NumChildren - 1; I >= 0; --I) {
    if (auto Child = getChild(I)) {
      if (Child->getRaw()->isMissing()) {
        continue;
      }

      if (auto Token = Child->getLastToken()) {
        return Token;
      }
    }
  }
  return None;
}

raw_ostream &llvm::operator<<(raw_ostream &OS,
                              swift::syntax::AbsoluteOffsetPosition Pos) {
  OS << "Offset " << Pos.getOffset();
  return OS;
}
