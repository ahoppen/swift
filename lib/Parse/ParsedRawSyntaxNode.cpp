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

ParsedRawSyntaxNode ParsedRawSyntaxNode::getDeferredChild(
    size_t ChildIndex, const SyntaxParsingContext *SyntaxContext) const {
  assert(isDeferredLayout());
  return SyntaxContext->getRecorder().getDeferredChild(*this, ChildIndex);
}

size_t ParsedRawSyntaxNode::getByteLength(SyntaxParseActions *Actions) const {
  return Actions->getByteLength(Data);
}

tok ParsedRawSyntaxNode::getTokenKind(SyntaxParsingContext *SyntaxContext) const {
  return SyntaxContext->getActions()->getTokenKind(Data);
}

syntax::SyntaxKind ParsedRawSyntaxNode::getKind(SyntaxParsingContext *SyntaxContext) const {
  return SyntaxContext->getActions()->getSyntaxKind(Data);
}

bool ParsedRawSyntaxNode::isMissing(SyntaxParseActions *Actions) const {
  return Actions->isMissing(Data);
}


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
      OS << " [recorded] ";
      break;
    case DataKind::DeferredLayout:
      OS << " [deferred]";
      OS << "<layout>";
      break;
    case DataKind::DeferredToken:
      OS << " [deferred] ";
      OS << "<token>";
      break;
    case DataKind::Destroyed:
      OS << " [destroyed] ";
  }
  OS << ')';
}
