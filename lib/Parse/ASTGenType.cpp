//===--- ASTGenType.cpp ---------------------------------------------------===//
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

#include "swift/AST/TypeRepr.h"
#include "swift/Parse/ASTGen.h"
#include "swift/Parse/CodeCompletionCallbacks.h"

using namespace swift;
using namespace swift::syntax;

//===--------------------------------------------------------------------===//
// MARK: - Public entry function

TypeRepr *ASTGen::generate(const TypeSyntaxRef &Type, SourceLoc TreeStartLoc,
                           SourceLoc PreviousTokLoc, Diag<> MissingTypeDiag) {
  this->TreeStartLoc = TreeStartLoc;
  this->PreviousTokLoc = PreviousTokLoc;

  return generate(Type, MissingTypeDiag);
}

//===--------------------------------------------------------------------===//
// MARK: - Generate functions

TypeRepr *ASTGen::generate(const TypeSyntaxRef &Type, Diag<> MissingTypeDiag) {
  auto typeLoc = getLeadingTriviaLoc(Type);

  // Check if we have recorded a type that hasn't been migrated to
  // libSyntax-parsing yet at this location
  if (hasType(typeLoc)) {
    return takeType(typeLoc);
  }

  // Otherwise, generate the AST node for the type.
  switch (Type.getKind()) {
  case SyntaxKind::ArrayType:
    return generate(Type.castTo<ArrayTypeSyntaxRef>());
  case SyntaxKind::DictionaryType:
    return generate(Type.castTo<DictionaryTypeSyntaxRef>());
  default:
    llvm_unreachable("ASTGen hasn't been tought how to generate this type");
  }
}

TypeRepr *ASTGen::generate(const ArrayTypeSyntaxRef &Type) {
  TypeRepr *ElementType = generate(Type.getElementType());
  return new (Context) ArrayTypeRepr(ElementType, getASTRange(Type));
}

TypeRepr *ASTGen::generate(const DictionaryTypeSyntaxRef &Type) {
  SourceLoc ColonLoc = getLoc(Type.getColon());

  TypeRepr *KeyType = generate(Type.getKeyType());
  TypeRepr *ValueType = generate(Type.getValueType());
  auto Range = getASTRange(Type);

  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

//===--------------------------------------------------------------------===//
// MARK: - Tracking non-migrated types

void ASTGen::addType(TypeRepr *T, const SourceLoc &Loc) {
  if (T == nullptr) {
    T = new (Context) ErrorTypeRepr(Loc);
  }
  assert(!hasType(Loc));
  Types[Loc] = T;
}

bool ASTGen::hasType(const SourceLoc &Loc) const {
  return Types.find(Loc) != Types.end();
}

TypeRepr *ASTGen::takeType(const SourceLoc &Loc) {
  auto I = Types.find(Loc);
  assert(I != Types.end());
  auto type = I->second;
  Types.erase(I);
  return type;
}
