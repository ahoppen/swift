//===--- ParsedRawSyntaxNode.cpp - Parsed Raw Syntax Node -----------------===//
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

#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/SyntaxParsingContext.h"

using namespace swift;
using namespace swift::syntax;
using namespace llvm;

void ParsedRawSyntaxNode::dump() const {
  dump(llvm::errs(), /*Indent*/ 0);
  llvm::errs() << '\n';
}

void ParsedRawSyntaxNode::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  for (decltype(Indent) i = 0; i < Indent; ++i)
    OS << ' ';
  OS << '(';

  switch (DK) {
    case DataKind::Null:
      OS << "<NULL>";
      break;
    case DataKind::Recorded:
      dumpSyntaxKind(OS, getKind());
      OS << " [recorded] ";
      if (isToken()) {
        dumpTokenKind(OS, getTokenKind());
      } else {
        OS << "<layout>";
      }
      break;
    case DataKind::DeferredLayout:
      dumpSyntaxKind(OS, getKind());
      OS << " [deferred]";
      for (const auto &child : getDeferredChildren()) {
        OS << "\n";
        child.dump(OS, Indent + 2);
      }
      break;
    case DataKind::DeferredToken:
      dumpSyntaxKind(OS, getKind());
      OS << " [deferred] ";
      dumpTokenKind(OS, getTokenKind());
      break;
  }
  OS << ')';
}
