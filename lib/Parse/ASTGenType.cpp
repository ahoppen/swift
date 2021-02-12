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
  case SyntaxKind::CodeCompletionType:
    return generate(Type.castTo<CodeCompletionTypeSyntaxRef>());
  case SyntaxKind::DictionaryType:
    return generate(Type.castTo<DictionaryTypeSyntaxRef>());
  case SyntaxKind::MemberTypeIdentifier:
    return generate(Type.castTo<MemberTypeIdentifierSyntaxRef>());
  case SyntaxKind::SimpleTypeIdentifier:
    return generate(Type.castTo<SimpleTypeIdentifierSyntaxRef>());
  case SyntaxKind::UnknownType:
    return generate(Type.castTo<UnknownTypeSyntaxRef>(), MissingTypeDiag);
  default:
    llvm_unreachable("ASTGen hasn't been tought how to generate this type");
  }
}

TypeRepr *ASTGen::generate(const ArrayTypeSyntaxRef &Type) {
  TypeRepr *ElementType = generate(Type.getElementType());
  return new (Context) ArrayTypeRepr(ElementType, getASTRange(Type));
}

TypeRepr *ASTGen::generate(const CodeCompletionTypeSyntaxRef &Type) {
  auto base = Type.getBase();
  if (!base) {
    if (CodeCompletion) {
      CodeCompletion->completeTypeSimpleBeginning();
    }
    return new (Context) ErrorTypeRepr(getASTRange(Type));
  }

  if (auto *parsedTyR = generate(*base)) {
    if (CodeCompletion) {
      CodeCompletion->setParsedTypeLoc(parsedTyR);
      if (Type.getPeriod()) {
        CodeCompletion->completeTypeIdentifierWithDot();
      } else {
        CodeCompletion->completeTypeIdentifierWithoutDot();
      }
    }
    return parsedTyR;
  }

  return new (Context) ErrorTypeRepr(getASTRange(Type));
}

TypeRepr *ASTGen::generate(const DictionaryTypeSyntaxRef &Type) {
  SourceLoc ColonLoc = getLoc(Type.getColon());

  TypeRepr *KeyType = generate(Type.getKeyType());
  TypeRepr *ValueType = generate(Type.getValueType());
  auto Range = getASTRange(Type);

  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const MemberTypeIdentifierSyntaxRef &Type) {
  SmallVector<ComponentIdentTypeRepr *, 4> components;
  gatherTypeIdentifierComponents(Type, components);
  return IdentTypeRepr::create(Context, components);
}

TypeRepr *ASTGen::generate(const SimpleTypeIdentifierSyntaxRef &Type) {
  if (Type.getName().getTokenKind() == tok::kw_Any) {
    auto anyLoc = getLoc(Type.getName());
    return CompositionTypeRepr::createEmptyComposition(Context, anyLoc);
  }

  auto typeRepr = generateTypeIdentifier(Type);
  return IdentTypeRepr::create(Context, {typeRepr});
}

TypeRepr *ASTGen::generate(const UnknownTypeSyntaxRef &Type,
                           Diag<> MissingTypeDiag) {
  auto ChildrenCount = Type.getNumChildren();

  // Recovery failed. Emit diagnostics.
  Optional<Diag<>> diagName = diag::expected_type;
  SourceLoc diagLoc = getLeadingTriviaLoc(Type);
  if (ChildrenCount == 0) {
    diagName = MissingTypeDiag;
  }
  if (ChildrenCount == 1) {
    SyntaxRef child = *Type.getChildRef(0);
    if (auto token = child.getAs<TokenSyntaxRef>()) {
      if (token->getTokenKind() != tok::identifier &&
          token->getTokenKind() != tok::kw_Self) {
        diagName = diag::expected_identifier_for_type;
      }
    }
  }

  if (diagName) {
    auto diag = diagnose(diagLoc, *diagName);
    if (ChildrenCount == 0) {
      diag.fixItInsert(getLeadingTriviaLoc(Type), "<#type#> ");
    }
  }

  // generate child 'TypeSyntax' anyway to trigger the side effects e.g.
  // code-completion.
  for (size_t i = 0; i != ChildrenCount; ++i) {
    auto elem = *Type.getChildRef(i);
    // TODO: (syntax-parse): Once everything is migrated, we can remove the
    // check for TypeSyntax here and always call generate.
    if (auto ty = elem.getAs<TypeSyntaxRef>()) {
      (void)generate(*ty);
    }
  }

  return new (Context) ErrorTypeRepr(getASTRange(Type));
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

//===--------------------------------------------------------------------===//
// MARK: - Private helper functions

std::pair<SourceRange, SmallVector<TypeRepr *, 4>> ASTGen::generateGenericArgs(
    const GenericArgumentClauseSyntaxRef &ClauseSyntax) {
  SmallVector<TypeRepr *, 4> args;
  for (auto arg : ClauseSyntax.getArguments()) {
    auto typeRepr = generate(arg.getArgumentType());
    args.push_back(typeRepr);
  }

  auto range = getASTRange(ClauseSyntax);
  return std::make_pair(range, args);
}

template <typename T>
ComponentIdentTypeRepr *ASTGen::generateTypeIdentifier(const T &TypeSyntax) {
  auto declNameLoc = DeclNameLoc(getLoc(TypeSyntax.getName()));
  auto declNameRef = DeclNameRef(
      Context.getIdentifier(TypeSyntax.getName().getIdentifierText()));
  diagnoseDollarIdentifier(TypeSyntax.getName(), /*DiagnoseDollarPrefix=*/true);
  if (auto clause = TypeSyntax.getGenericArgumentClause()) {
    SourceRange range;
    SmallVector<TypeRepr *, 4> args;
    std::tie(range, args) = generateGenericArgs(*clause);
    if (!args.empty()) {
      return GenericIdentTypeRepr::create(Context, declNameLoc, declNameRef,
                                          args, range);
    }
  }
  return new (Context) SimpleIdentTypeRepr(declNameLoc, declNameRef);
}

void ASTGen::gatherTypeIdentifierComponents(
    const TypeSyntaxRef &Component,
    SmallVectorImpl<ComponentIdentTypeRepr *> &Components) {
  if (auto simpleIdentifier =
          Component.getAs<SimpleTypeIdentifierSyntaxRef>()) {
    auto componentType = generateTypeIdentifier(*simpleIdentifier);
    Components.push_back(componentType);
  } else if (auto memberIdentifier =
                 Component.getAs<MemberTypeIdentifierSyntaxRef>()) {
    gatherTypeIdentifierComponents(memberIdentifier->getBaseType(), Components);
    auto ComponentType = generateTypeIdentifier(*memberIdentifier);
    Components.push_back(ComponentType);
  } else {
    llvm_unreachable("unexpected type identifier component");
  }
}

