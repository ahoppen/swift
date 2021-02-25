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
  case SyntaxKind::AttributedType:
    return generate(Type.castTo<AttributedTypeSyntaxRef>());
  case SyntaxKind::CodeCompletionType:
    return generate(Type.castTo<CodeCompletionTypeSyntaxRef>());
  case SyntaxKind::CompositionTypeElementList:
    llvm_unreachable("Composition elements list is being generated from "
                     "within the CompositionType generate function.");
  case SyntaxKind::CompositionTypeElement:
    llvm_unreachable("Composition type elements are being generated from "
                     "within the CompositionType generate function.");
  case SyntaxKind::CompositionType:
    return generate(Type.castTo<CompositionTypeSyntaxRef>());
  case SyntaxKind::DictionaryType:
    return generate(Type.castTo<DictionaryTypeSyntaxRef>());
  case SyntaxKind::FunctionType:
    return generate(Type.castTo<FunctionTypeSyntaxRef>());
  case SyntaxKind::ImplicitlyUnwrappedOptionalType:
    return generate(Type.castTo<ImplicitlyUnwrappedOptionalTypeSyntaxRef>());
  case SyntaxKind::MemberTypeIdentifier:
    return generate(Type.castTo<MemberTypeIdentifierSyntaxRef>());
  case SyntaxKind::MetatypeType:
    return generate(Type.castTo<MetatypeTypeSyntaxRef>());
  case SyntaxKind::OptionalType:
    return generate(Type.castTo<OptionalTypeSyntaxRef>());
  case SyntaxKind::SimpleTypeIdentifier:
    return generate(Type.castTo<SimpleTypeIdentifierSyntaxRef>());
  case SyntaxKind::SomeType:
    return generate(Type.castTo<SomeTypeSyntaxRef>());
  case SyntaxKind::TupleType:
    return generate(Type.castTo<TupleTypeSyntaxRef>());
  case SyntaxKind::TupleTypeElement:
    llvm_unreachable("Tuple type elements are being generated from within the "
                     "TupleTypeSyntax generate function.");
  case SyntaxKind::UnknownType:
    return generate(Type.castTo<UnknownTypeSyntaxRef>(), MissingTypeDiag);
  default:
    llvm_unreachable("ASTGen hasn't been tought how to generate this type");
  }
  //  } else if (auto some = Type.getAs<SomeTypeSyntax>()) {
  //    return generate(*some, Loc);
  //  } else if (auto classRestriction =
  //  Type.getAs<ClassRestrictionTypeSyntax>()) {
  //    return generate(*classRestriction, Loc);
  //  } else if (auto SILBoxType = Type.getAs<SILBoxTypeSyntax>()) {
  //    return generate(*SILBoxType, Loc, IsSILFuncDecl);
  //  } else if (auto SILFunctionType = Type.getAs<SILFunctionTypeSyntax>()) {
  //    return generate(*SILFunctionType, Loc, IsSILFuncDecl);
  //  }

  // FIXME: (syntax-parse) Erase generic type parameters
  //  if (Tok.is(tok::arrow)) {
  //  } else if (auto firstGenerics = generics ? generics : patternGenerics) {
  //    // Only function types may be generic.
  //    auto brackets = firstGenerics->getSourceRange();
  //    diagnose(brackets.Start, diag::generic_non_function);
  //
  //    // Forget any generic parameters we saw in the type.
  //    class EraseTypeParamWalker : public ASTWalker {
  //    public:
  //      bool walkToTypeReprPre(TypeRepr *T) override {
  //        if (auto ident = dyn_cast<ComponentIdentTypeRepr>(T)) {
  //          if (auto decl = ident->getBoundDecl()) {
  //            if (auto genericParam = dyn_cast<GenericTypeParamDecl>(decl))
  //              ident->overwriteNameRef(genericParam->createNameRef());
  //          }
  //        }
  //        return true;
  //      }
  //
  //    } walker;
  //
  //    if (tyR)
  //      tyR->walk(walker);
  //  }
}

TypeRepr *ASTGen::generate(const ArrayTypeSyntaxRef &Type) {
  auto Element = Type.getElementType();
  TypeRepr *ElementType = generate(Element.getRef());
  return new (*Context) ArrayTypeRepr(ElementType, getASTRange(Type));
}

TypeRepr *ASTGen::generate(const AttributedTypeSyntaxRef &Type) {
  auto BaseType = Type.getBaseType();
  auto typeAST = generate(BaseType.getRef());

  if (auto attributes = Type.getAttributes()) {
    TypeAttributes attrs = generateTypeAttributes(attributes.getRef());
    if (!attrs.empty()) {
      typeAST = new (*Context) AttributedTypeRepr(attrs, typeAST);
    }
  }

  if (auto specifier = Type.getSpecifier()) {
    auto specifierLoc = getLoc(specifier.getRef());
    auto specifierText = specifier->getText();

    // don't apply multiple specifiers to a type: that's invalid and was already
    // reported in the parser, handle gracefully
    if (!isa<SpecifierTypeRepr>(typeAST)) {
      if (specifierText == "inout") {
        typeAST = new (*Context) InOutTypeRepr(typeAST, specifierLoc);
      } else if (specifierText == "__owned") {
        typeAST = new (*Context) OwnedTypeRepr(typeAST, specifierLoc);
      } else if (specifierText == "__shared") {
        typeAST = new (*Context) SharedTypeRepr(typeAST, specifierLoc);
      } else {
        llvm_unreachable(
            "The libSyntax tree guarantees there are no other specifiers");
      }
    }
  }

  return typeAST;
}

TypeRepr *ASTGen::generate(const CodeCompletionTypeSyntaxRef &Type) {
  auto base = Type.getBase();
  if (!base) {
    if (CodeCompletion) {
      CodeCompletion->completeTypeSimpleBeginning();
    }
    return new (*Context) ErrorTypeRepr(getASTRange(Type));
  }

  if (auto *parsedTyR = generate(base.getRef())) {
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

  return new (*Context) ErrorTypeRepr(getASTRange(Type));
}

TypeRepr *ASTGen::generate(const CompositionTypeSyntaxRef &Type) {
  auto elements = Type.getElements();
  assert(!elements->empty());

  SmallVector<TypeRepr *, 4> elementTypeReprs;
  for (auto element = elements->begin(); element != elements->end(); ++element) {
    auto elementType = (*element)->getType();
    TypeRepr *elementTypeRepr;
    if (auto someSyntax = elementType->getAs<SomeTypeSyntaxRef>()) {
      // Some types inside type compositions are not valid. Diagnose and ignore
      // the 'some'.
      auto diag = diagnose(someSyntax->getSomeSpecifier().getRef(),
                           diag::opaque_mid_composition);
      diag.fixItRemove(getLoc(someSyntax->getSomeSpecifier().getRef()));

      // Don't suggest adding 'some' before the composition if there is already
      // a 'some' before.
      bool isAlreadySomeType =
          Type.getParentRef() && Type.getParentRef()->is<SomeTypeSyntaxRef>();
      if (!isAlreadySomeType) {
        diag.fixItInsert(getContentStartLoc(Type), "some ");
      }

      elementTypeRepr = generate(someSyntax->getBaseType().getRef());
    } else {
      elementTypeRepr = generate(elementType.getRef());
    }

    if (elementTypeRepr) {
      elementTypeReprs.push_back(elementTypeRepr);
    }
  }

  SourceLoc firstTypeLoc;
  if (!elements->empty()) {
    auto FirstChild = elements->getChild(0);
    firstTypeLoc = getContentStartLoc(FirstChild.getRef());
  }
  return CompositionTypeRepr::create(*Context, elementTypeReprs, firstTypeLoc,
                                     getASTRange(Type));
}

TypeRepr *ASTGen::generate(const DictionaryTypeSyntaxRef &Type) {
  SourceLoc ColonLoc = getLoc(Type.getColon().getRef());

  TypeRepr *KeyType = generate(Type.getKeyType().getRef());
  TypeRepr *ValueType = generate(Type.getValueType().getRef());
  auto Range = getASTRange(Type);

  return new (*Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const FunctionTypeSyntaxRef &FuncSyntax) {
  TupleTypeRepr *argumentTypes = argumentTypes =
      generateTuple(FuncSyntax.getArguments()->getElements().getRef(),
                    getASTRange(FuncSyntax), /*IsInFunctionSignature=*/true);

  if (!argumentTypes) {
    return new (*Context) ErrorTypeRepr(getASTRange(FuncSyntax));
  }

  SourceLoc asyncLoc, throwsLoc;
  std::tie(asyncLoc, throwsLoc) = generateEffectsSpecifiers(
      FuncSyntax.getEffectsSpecifiers().getRef(), /*RethrowsAllowed=*/false);

  auto arrowLoc = getLoc(FuncSyntax.getArrow().getRef());
  auto returnType = generate(FuncSyntax.getReturnType().getRef());
  if (!returnType) {
    return new (*Context) ErrorTypeRepr(getASTRange(FuncSyntax));
    ;
  }

  return new (*Context) FunctionTypeRepr(nullptr, argumentTypes, asyncLoc,
                                        throwsLoc, arrowLoc, returnType);
}

TypeRepr *
ASTGen::generate(const ImplicitlyUnwrappedOptionalTypeSyntaxRef &Type) {
  auto baseTypeRepr = generate(Type.getWrappedType().getRef());
  auto exclamationLoc = getLoc(Type.getExclamationMark().getRef());
  return new (*Context)
      ImplicitlyUnwrappedOptionalTypeRepr(baseTypeRepr, exclamationLoc);
}

TypeRepr *ASTGen::generate(const MemberTypeIdentifierSyntaxRef &Type) {
  SmallVector<ComponentIdentTypeRepr *, 4> components;
  gatherTypeIdentifierComponents(Type, components);
  return IdentTypeRepr::create(*Context, components);
}

TypeRepr *ASTGen::generate(const MetatypeTypeSyntaxRef &Type) {
  auto baseTypeRepr = generate(Type.getBaseType().getRef());
  auto metaLoc = getLoc(Type.getTypeOrProtocol().getRef());
  auto metaText = Type.getTypeOrProtocol()->getText();
  if (metaText == "Type") {
    return new (*Context) MetatypeTypeRepr(baseTypeRepr, metaLoc);
  } else if (metaText == "Protocol") {
    return new (*Context) ProtocolTypeRepr(baseTypeRepr, metaLoc);
  } else {
    llvm_unreachable("Meta part must be 'Type' or 'Protocol'");
  }
}

TypeRepr *ASTGen::generate(const OptionalTypeSyntaxRef &Type) {
  auto baseTypeRepr = generate(Type.getWrappedType().getRef());
  auto questionLoc = getLoc(Type.getQuestionMark().getRef());
  return new (*Context) OptionalTypeRepr(baseTypeRepr, questionLoc);
}

TypeRepr *ASTGen::generate(const SimpleTypeIdentifierSyntaxRef &Type) {
  auto Name = Type.getName();
  if (Name->getTokenKind() == tok::kw_Any) {
    auto anyLoc = getLoc(Name.getRef());
    return CompositionTypeRepr::createEmptyComposition(*Context, anyLoc);
  }

  auto typeRepr = generateTypeIdentifier(Type);
  return IdentTypeRepr::create(*Context, {typeRepr});
}

TypeRepr *ASTGen::generate(const SomeTypeSyntaxRef &Type) {
  auto someLoc = getLoc(Type.getSomeSpecifier().getRef());
  auto baseTypeRepr = generate(Type.getBaseType().getRef());
  return new (*Context) OpaqueReturnTypeRepr(someLoc, baseTypeRepr);
}

TypeRepr *ASTGen::generate(const TupleTypeSyntaxRef &Type) {
  return generateTuple(Type.getElements().getRef(), getASTRange(Type),
                       /*IsInFunctionSignature=*/false);
}

TypeRepr *ASTGen::generate(const UnknownTypeSyntaxRef &Type,
                           Diag<> MissingTypeDiag) {
  auto ChildrenCount = Type.getNumChildren();

  if (auto recovered = recoverOldStyleProtocolComposition(Type)) {
    return recovered;
  }

  // Recovery failed. Emit diagnostics.
  Optional<Diag<>> diagName = diag::expected_type;
  SourceLoc diagLoc = getLeadingTriviaLoc(Type);
  if (ChildrenCount == 0) {
    diagName = MissingTypeDiag;
  }
  if (ChildrenCount == 1) {
    auto child = Type.getChildRef(0);
    if (auto token = child->getAs<TokenSyntaxRef>()) {
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
    if (auto elem = Type.getChildRef(i)) {
      // TODO: (syntax-parse): Once everything is migrated, we can remove the
      // check for TypeSyntax here and always call generate.
      if (auto ty = elem->getAs<TypeSyntaxRef>()) {
        (void)generate(std::move(*ty));
      }
    }
  }

  return new (*Context) ErrorTypeRepr(getASTRange(Type));
}

//===--------------------------------------------------------------------===//
// MARK: - Tracking non-migrated types

void ASTGen::addType(TypeRepr *T, const SourceLoc &Loc) {
  if (T == nullptr) {
    T = new (*Context) ErrorTypeRepr(Loc);
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

std::pair<SourceLoc, SourceLoc> ASTGen::generateEffectsSpecifiers(
    const EffectsSpecifierListSyntaxRef &EffectsSpecifiers,
    bool RethrowsAllowed) {
  SourceLoc asyncLoc;
  SourceLoc throwsLoc;
  // When throwsLoc is valid, whether it points to a 'throws' or 'rethrows'
  // token
  bool isRethrows;

  for (auto specifier : EffectsSpecifiers) {
    auto tokenStore = specifier->getSpecifier();
    auto token = tokenStore.getRef();

    switch (token.getTokenKind()) {
    case tok::kw_throws:
    case tok::kw_rethrows:
      if (throwsLoc.isValid()) {
        diagnose(token, diag::duplicate_effects_specifier, token.getText())
            .highlight(throwsLoc)
            .fixItRemove(getLoc(token));
      } else if (token.getTokenKind() == tok::kw_rethrows && !RethrowsAllowed) {
        diagnose(token, diag::rethrowing_function_type)
            .fixItReplace(getLoc(token), "throws");
      } else {
        isRethrows = (token.getTokenKind() == tok::kw_rethrows);
        throwsLoc = getLoc(token);
      }
      break;
    default:
      if (token.getText() == "async") {
        if (asyncLoc.isValid()) {
          diagnose(token, diag::duplicate_effects_specifier, token.getText())
              .highlight(asyncLoc)
              .fixItRemove(getLoc(token));
        } else if (throwsLoc.isValid()) {
          // 'async' cannot be after 'throws'.
          diagnose(token, diag::async_after_throws, isRethrows)
              .fixItRemove(getLoc(token))
              .fixItInsert(throwsLoc, "async ");
        } else {
          // The async specifier is valid
          asyncLoc = getLoc(token);
        }
      } else {
        assert(false && "Unknown effects specifier");
      }
    }
  }

  return std::make_pair(asyncLoc, throwsLoc);
}

std::pair<SourceRange, SmallVector<TypeRepr *, 4>> ASTGen::generateGenericArgs(
    const GenericArgumentClauseSyntaxRef &ClauseSyntax) {
  SmallVector<TypeRepr *, 4> args;
  auto arguments = ClauseSyntax.getArguments();
  for (auto arg : arguments.getRef()) {
    auto typeRepr = generate(arg->getArgumentType().getRef());
    args.push_back(typeRepr);
  }

  auto range = getASTRange(ClauseSyntax);
  return std::make_pair(range, args);
}

TupleTypeRepr *
ASTGen::generateTuple(const TupleTypeElementListSyntaxRef &Elements,
                      const SourceRange &RangeWithParens,
                      bool IsInFunctionSignature) {
  SmallVector<TupleTypeReprElement, 4> tupleElements;

  SourceLoc ellipsisLoc;
  unsigned ellipsisIdx;

  for (unsigned i = 0; i < Elements.size(); i++) {
    auto elementStore = Elements.getChild(i);
    auto element = elementStore.getRef();

    // If we are in a function type, complain about elements that have a first
    // name that is not '_'.
    // If we are not in a function type, complain about elements with a second
    // name.
    if (IsInFunctionSignature) {
      if (auto nameToken = element.getName()) {
        if (nameToken->getText() != "_") {
          auto nameIdentifier = Context->getIdentifier(nameToken->getText());
          auto diag = diagnose(nameToken.getRef(), diag::function_type_argument_label,
                               nameIdentifier);
          if (auto secondName = element.getSecondName()) {
            if (secondName->getText() == "_") {
              // The names are of the form `first '_' ':' type`
              // Offer to remove both names.
              diag.fixItRemoveChars(getLoc(nameToken.getRef()),
                                    getLeadingTriviaLoc(element.getType().getRef()));
            } else {
              // The names are of the form `first second ':' type`
              // The first name is invalid. Replace it by '_'.
              diag.fixItReplace(getLoc(nameToken.getRef()), "_");
            }
          } else {
            // If we only have a first name (i.e. name ':' type) suggest
            // changing it to `'_' name ':' type`
            diag.fixItInsert(getLoc(nameToken.getRef()), "_ ");
          }
        }
      }
    } else {
      // We're in a proper tuple
      if (element.getName() && element.getSecondName()) {
        // True tuples can't have two labels
        auto nameToken = element.getName();

        auto diag = diagnose(nameToken.getRef(), diag::tuple_type_multiple_labels);
        if (nameToken->getText() == "_") {
          // If the first name is '_', remove both names.
          diag.fixItRemoveChars(getLoc(nameToken.getRef()),
                                getLeadingTriviaLoc(element.getType().getRef()));
        } else {
          // Otherwise, remove the second name
          diag.fixItRemove(getLoc(element.getSecondName().getRef()));
        }
      }
    }

    TupleTypeReprElement elementAST;
    elementAST.Type = generate(element.getType().getRef());

    if (auto name = element.getName()) {
      elementAST.NameLoc = getLoc(name.getRef());
      elementAST.Name = name->getText() == "_"
                            ? Identifier()
                            : Context->getIdentifier(name->getIdentifierText());
    }
    if (auto secondName = element.getSecondName()) {
      elementAST.SecondNameLoc = getLoc(secondName.getRef());
      elementAST.SecondName =
          secondName->getText() == "_"
              ? Identifier()
              : Context->getIdentifier(secondName->getIdentifierText());
      if (elementAST.Name.empty()) {
        // If the first name is empty (i.e. was an underscore), use it as the
        // underscore location and use the second (non-underscore) name as the
        // first name.
        elementAST.UnderscoreLoc = elementAST.NameLoc;
        elementAST.Name = elementAST.SecondName;
        elementAST.NameLoc = elementAST.SecondNameLoc;
      }
    }
    if (auto colon = element.getColon()) {
      elementAST.ColonLoc = getLoc(colon.getRef());
    }

    if (auto inOut = element.getInOut()) {
      auto inOutLoc = getLoc(inOut.getRef());
      if (isa<InOutTypeRepr>(elementAST.Type)) {
        // If the parsed type is already attributed, suggest removing
        // `inout`.
        diagnose(inOutLoc, diag::parameter_specifier_repeated)
            .fixItRemove(inOutLoc);
      } else {
        // Otherwise suggest moving it before the type.
        diagnose(inOutLoc, diag::parameter_specifier_as_attr_disallowed,
                 "inout")
            .fixItRemove(inOutLoc)
            .fixItInsert(getContentStartLoc(element.getType().getRef()), "inout ");
      }

      // If the type is not already attributed, apply the inout attribute to
      // recover
      if (!isa<InOutTypeRepr>(elementAST.Type)) {
        auto inOutLoc = getLoc(inOut.getRef());
        elementAST.Type =
            new (*Context) InOutTypeRepr(elementAST.Type, inOutLoc);
      }
    }

    if (auto ellipsis = element.getEllipsis()) {
      if (ellipsisLoc.isInvalid()) {
        // Seeing the first ellipsis
        ellipsisLoc = getLoc(ellipsis.getRef());
        ellipsisIdx = i;
      } else {
        // We have already seen an ellipsis. Diagnose.
        diagnose(ellipsis.getRef(), diag::multiple_ellipsis_in_tuple)
            .highlight(ellipsisLoc) // ellipsisLoc is the previous ellipsis
            .fixItRemove(getLoc(ellipsis.getRef()));
      }
    }

    if (element.getInitializer()) {
      // Initializers aren't valid in function types.
      auto init = element.getInitializer();
      diagnose(init.getRef(), diag::tuple_type_init)
          .fixItRemoveChars(getRangeWithTrivia(init.getRef()));
    }

    if (auto comma = element.getTrailingComma()) {
      elementAST.TrailingCommaLoc = getLoc(comma.getRef());
    }
    tupleElements.push_back(elementAST);
  }
  if (ellipsisLoc.isInvalid()) {
    // If we don't have an ellipsis the ellipsis index must point after the last
    // element to indicate this.
    ellipsisIdx = tupleElements.size();
  }

  return TupleTypeRepr::create(*Context, tupleElements, RangeWithParens,
                               ellipsisLoc, ellipsisIdx);
}

TypeAttributes
ASTGen::generateTypeAttributes(const AttributeListSyntaxRef &Syntax) {
  TypeAttributes attrs;

  for (auto elem : Syntax) {
    // We don't have custom type attributes, only custom decl attributes.
    if (!elem->is<AttributeSyntaxRef>()) {
      assert(false && "We don't have custom type attributes");
      continue;
    }
    auto attrSyntax = elem->castTo<AttributeSyntaxRef>();

    auto attrName = attrSyntax.getAttributeName()->getText();

    // If we haven't recorded the location of the first '@' yet, do so now.
    auto atLoc = getLoc(attrSyntax.getAtSignToken().getRef());
    if (attrs.AtLoc.isInvalid()) {
      attrs.AtLoc = atLoc;
    }

    auto attr = TypeAttributes::getAttrKindFromString(attrName);
    if (attr == TAK_Count) {
      auto declAttrID = DeclAttribute::getAttrKindFromString(attrName);
      if (declAttrID == DAK_Count) {
        // Not a decl or type attribute.
        diagnose(attrSyntax.getAttributeName().getRef(), diag::unknown_attribute,
                 attrName);
      } else {
        diagnose(attrSyntax.getAttributeName().getRef(),
                 diag::decl_attribute_applied_to_type);
      }
      continue;
    }

    if (attrs.has(attr)) {
      diagnose(attrSyntax.getAtSignToken().getRef(), diag::duplicate_attribute,
               /*isModifier=*/false);
      continue;
    }

    // In the following we have two methods of ignoring an attribute:
    // Either 'continue' to continue the loop and don't add the attribute to
    // attrs or 'break' to break the switch (possibly skipping custom handling
    // logic) but still adding it to attrs afterwards.
    switch (attr) {
    case TAK_out:
    case TAK_in:
    case TAK_owned:
    case TAK_unowned_inner_pointer:
    case TAK_guaranteed:
    case TAK_autoreleased:
    case TAK_callee_owned:
    case TAK_callee_guaranteed:
    case TAK_objc_metatype:
      if (!isInSILMode()) {
        diagnose(atLoc, diag::only_allowed_in_sil, attrName);
        continue;
      }
      break;
    case TAK_sil_weak:
    case TAK_sil_unowned:
      if (!isInSILMode()) {
        diagnose(atLoc, diag::only_allowed_in_sil, attrName);
        continue;
      }
      if (attrs.hasOwnership()) {
        diagnose(attrSyntax.getAtSignToken().getRef(), diag::duplicate_attribute,
                 /*isModifier=*/false);
      }
      break;
    case TAK_inout:
      if (!isInSILMode()) {
        diagnose(atLoc, diag::inout_not_attribute);
      }
      break;
    case TAK_opened: {
      if (!isInSILMode()) {
        diagnose(atLoc, diag::only_allowed_in_sil, "opened");
        continue;
      }
      // @opened("01234567-89ab-cdef-0123-111111111111")
      auto arg = attrSyntax.getArgument();
      if (!arg) {
        diagnose(atLoc, diag::opened_attribute_id_value);
        continue;
      }

      if (!arg->is<TokenSyntaxRef>() ||
          arg->castTo2<TokenSyntaxRef>().getTokenKind() != tok::string_literal) {
        diagnose(arg.getRef(), diag::opened_attribute_id_value);
        break;
      }
      auto tokText = arg->castTo2<TokenSyntaxRef>().getText();
      // Remove quotes from the string literal.
      auto literalText = tokText.slice(1, tokText.size() - 1);
      if (auto openedID = UUID::fromString(literalText.str().c_str())) {
        attrs.OpenedID = openedID;
      } else {
        diagnose(arg.getRef(), diag::opened_attribute_id_value);
      }
      break;
    }
    case TAK_differentiable: {
      DifferentiabilityKind diffKind = DifferentiabilityKind::Normal;

      auto arg = attrSyntax.getArgument();
      if (arg && arg->is<DifferentiableAttributeArgumentsSyntaxRef>()) {
        auto diffKindSyntax =
            arg->castTo2<DifferentiableAttributeArgumentsSyntaxRef>()
                .getDiffKind();
        auto diffKindText = diffKindSyntax->getIdentifierText();
        diffKind =
            llvm::StringSwitch<DifferentiabilityKind>(diffKindText)
                .Case("reverse", DifferentiabilityKind::Reverse)
                .Cases("wrt", "withRespectTo", DifferentiabilityKind::Normal)
                .Case("_linear", DifferentiabilityKind::Linear)
                .Case("_forward", DifferentiabilityKind::Forward)
                .Default(DifferentiabilityKind::NonDifferentiable);

        switch (diffKind) {
        // Reject unsupported differentiability kinds.
        case DifferentiabilityKind::Forward:
          diagnose(arg.getRef(), diag::attr_differentiable_kind_not_supported,
                   diffKindText)
              .fixItReplaceChars(getRangeWithoutTrivia(arg.getRef()), "reverse");
          continue;
        case DifferentiabilityKind::NonDifferentiable:
          diagnose(arg.getRef(), diag::attr_differentiable_unknown_kind, diffKindText)
              .fixItReplaceChars(getRangeWithoutTrivia(arg.getRef()), "reverse");
          continue;
        default:
          break;
        }
      }

      if (diffKind == DifferentiabilityKind::Normal) {
        // TODO: Change this to an error when clients have migrated to
        // 'reverse'.
        auto diag =
            diagnose(attrSyntax, diag::attr_differentiable_expected_reverse);

        if (arg && arg->is<DifferentiableAttributeArgumentsSyntaxRef>()) {
          auto diffArgSyntax =
              arg->castTo2<DifferentiableAttributeArgumentsSyntaxRef>();
          if (diffArgSyntax.getDiffKindComma()) {
            diag.fixItInsert(getContentStartLoc(diffArgSyntax), "reverse");
          } else {
            diag.fixItInsert(getContentStartLoc(diffArgSyntax), "reverse, ");
          }
        } else {
          diag.fixItInsert(getRangeWithoutTrivia(attrSyntax).getEnd(),
                           "(reverse)");
        }
        diffKind = DifferentiabilityKind::Reverse;
      }

      attrs.differentiabilityKind = diffKind;

      break;
    }
    case TAK_convention: {
      // @convention(block)
      // @convention(witness_method: ProtocolName)
      // @convention(c, cType: "void *(void)")
      TypeAttributes::Convention convention;

      auto arg = attrSyntax.getArgument();
      if (!arg) {
        diagnose(atLoc, diag::convention_attribute_expected_name);
        continue;
      }

      if (auto conventionNameTok = arg->getAs<TokenSyntaxRef>()) {
        auto conventionName = conventionNameTok->getIdentifierText();
        // Make an identifier for the convention name and use its string so the
        // name stays alive even after the syntax tree has been destructed.
        convention.Name = Context->getIdentifier(conventionName).str();
      } else if (auto conventionAttributeArgs =
                     arg->getAs<CTypeConventionAttributeArgumentsSyntaxRef>()) {
        auto conventionName =
            conventionAttributeArgs->getConvention()->getIdentifierText();
        conventionName = Context->getIdentifier(conventionName).str();
        convention.Name = conventionName;
        if (auto cType = conventionAttributeArgs->getCType()) {
          // If the attribute doesn't have a cType, this has already been
          // diagnosed in the parser
          auto cTypeToken = cType.getRef();
          if (cTypeToken.getTokenKind() != tok::string_literal) {
            diagnose(cTypeToken,
                     diag::convention_attribute_ctype_expected_string);
            continue;
          }
          auto cTypeTokenText = cTypeToken.getText();
          auto cTypeString = cTypeTokenText.slice(1, cTypeTokenText.size() - 1);
          cTypeString = Context->getIdentifier(cTypeString).str();
          auto cTypeStringLoc = getLoc(cTypeToken);
          convention.ClangType = {cTypeString, cTypeStringLoc};
        }
      } else if (auto witness =
                     arg->getAs<NamedAttributeStringArgumentSyntaxRef>()) {
        if (witness->getNameTok()->getIdentifierText() != "witness_method") {
          diagnose(witness->getNameTok().getRef(),
                   diag::convention_attribute_unkown_name);
          continue;
        }
        if (!witness->getStringOrDeclname()->is<DeclNameSyntaxRef>()) {
          diagnose(witness->getStringOrDeclname().getRef(),
                   diag::convention_attribute_witness_method_expected_protocol);
          continue;
        }
        auto protocolName =
            witness->getStringOrDeclname()->castTo2<DeclNameSyntaxRef>();
        convention.Name = "witness_method";
        convention.WitnessMethodProtocol = generateDeclNameRef(protocolName);
      } else {
        // Unknown attribute. Diagnose and ignore.
        diagnose(witness->getNameTok().getRef(), diag::convention_attribute_unkown_name);
        continue;
      }
      attrs.ConventionArguments = convention;
      break;
    }
    case TAK__opaqueReturnTypeOf: {
      auto arg = attrSyntax.getArgument();
      // @_opaqueReturnTypeOf("$sMangledName", 0)
      if (!arg) {
        diagnose(attrSyntax.getAttributeName().getRef(),
                 diag::attr_expected_string_literal, attrName);
        continue;
      }
      if (!arg->is<OpaqueReturnTypeOfAttributeArgumentsSyntaxRef>()) {
        diagnose(attrSyntax.getAttributeName().getRef(),
                 diag::attr_expected_string_literal, attrName);
        continue;
      }

      auto opaqueArg =
          arg->castTo2<OpaqueReturnTypeOfAttributeArgumentsSyntaxRef>();

      auto manglingTok = opaqueArg.getMangledName();
      auto indexTok = opaqueArg.getIndex();

      auto tokText = manglingTok->getText();
      auto mangling = tokText.slice(1, tokText.size() - 1);
      mangling = Context->getIdentifier(mangling).str();
      unsigned index;
      if (indexTok->getText().getAsInteger(10, index)) {
        diagnose(indexTok.getRef(), diag::attr_expected_int_literal, attrName);
        continue;
      }
      attrs.setOpaqueReturnTypeOf(mangling, index);
      break;
    }
    default: // No special handling for this attribute
      break;
    }

    attrs.setAttr(attr, atLoc);
  }

  return attrs;
}


template <typename T>
ComponentIdentTypeRepr *ASTGen::generateTypeIdentifier(const T &TypeSyntax) {
  auto name = TypeSyntax.getName();
  auto declNameLoc = DeclNameLoc(getLoc(name.getRef()));
  auto declNameRef = DeclNameRef(
      Context->getIdentifier(name->getIdentifierText()));
  diagnoseDollarIdentifier(name.getRef(), /*DiagnoseDollarPrefix=*/true);
  if (auto clause = TypeSyntax.getGenericArgumentClause()) {
    SourceRange range;
    SmallVector<TypeRepr *, 4> args;
    std::tie(range, args) = generateGenericArgs(clause.getRef());
    if (!args.empty()) {
      return GenericIdentTypeRepr::create(*Context, declNameLoc, declNameRef,
                                          args, range);
    }
  }
  return new (*Context) SimpleIdentTypeRepr(declNameLoc, declNameRef);
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
    gatherTypeIdentifierComponents(memberIdentifier->getBaseType().getRef(), Components);
    auto ComponentType = generateTypeIdentifier(*memberIdentifier);
    Components.push_back(ComponentType);
  } else {
    llvm_unreachable("unexpected type identifier component");
  }
}

TypeRepr *
ASTGen::recoverOldStyleProtocolComposition(const UnknownTypeSyntaxRef &Type) {
  auto ChildrenCount = Type.getNumChildren();

  // Can't be old-style protocol composition because we need at least
  // 'protocol' '<'
  if (ChildrenCount < 2) {
    return nullptr;
  }

  auto keyword = Type.getChildRef(0)->getAs<TokenSyntaxRef>();
  if (!keyword || keyword->getText() != "protocol") {
    return nullptr;
  }
  auto lAngle = Type.getChildRef(1)->getAs<TokenSyntaxRef>();
  if (!lAngle || lAngle->getTokenKind() != tok::l_angle) {
    return nullptr;
  }

  // The unknown type starts with 'protocol<'. We are recovering and old style
  // protocol composition.

  // Generate the composed protocols for the final type representation.
  // Gather the protocol names for the fixit.
  SmallVector<TypeRepr *, 4> protocols;
  SmallVector<StringRef, 2> protocolNames;

  for (unsigned i = 2; i < Type.getNumChildren(); i++) {
    // Only consider types. Skip over commas.
    if (auto elem = Type.getChildRef(i)->getAs<TypeSyntaxRef>()) {
      auto range = getRangeWithoutTrivia(*elem);
      if (auto proto = generate(std::move(*elem))) {
        protocols.push_back(proto);
      }
      if (range.getByteLength() > 0) {
        // If the protocol name has a source representation, add it to the
        // protocol names
        protocolNames.push_back(SourceMgr.extractText(range));
      }
    }
  }

  // Compute the fixit replacement string.
  SmallString<32> replacement;
  if (protocolNames.empty()) {
    replacement = "Any";
  } else {
    auto firstProtocol = true;
    for (auto protocolName : protocolNames) {
      if (!firstProtocol) {
        replacement += " & ";
      }
      firstProtocol = false;
      replacement += protocolName;
    }
  }

  // If the parent node is also a type syntax, we are performing more operations
  // on this type. Wrap it in parens.
  auto Parent = Type.getParentRef();
  if (Parent && Parent->is<TypeSyntaxRef>()) {
    replacement.insert(replacement.begin(), '(');
    replacement += ")";
  }

  Diag<> message;
  switch (protocolNames.size()) {
  case 0:
    message = diag::deprecated_any_composition;
    break;
  case 1:
    message = diag::deprecated_protocol_composition_single;
    break;
  default:
    message = diag::deprecated_protocol_composition;
    break;
  }

  diagnose(getLoc(*keyword), message)
      .highlightChars(getRangeWithoutTrivia(Type))
      .fixItReplaceChars(getRangeWithoutTrivia(Type), replacement);

  return CompositionTypeRepr::create(*Context, protocols, getLoc(*keyword),
                                     getASTRange(Type));
}
