//===--- Syntax.cpp - Swift Syntax Implementation -------------------------===//
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

#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/SyntaxVisitor.h"

using namespace swift;
using namespace swift::syntax;

Optional<TokenSyntax> Syntax::getFirstToken() const {
  if (auto Token = getData().getFirstToken()) {
    return TokenSyntax(Token);
  } else {
    return None;
  }
}

Optional<TokenSyntax> Syntax::getLastToken() const {
  if (auto Token = getData().getLastToken()) {
    return TokenSyntax(Token);
  } else {
    return None;
  }
}
