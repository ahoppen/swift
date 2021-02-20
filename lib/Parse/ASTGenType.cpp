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
  SyntaxDataRef Data[1];
  TypeRepr *ElementType = generate(Type.getElementType(Data));
  return new (*Context) ArrayTypeRepr(ElementType, getASTRange(Type));
}

TypeRepr *ASTGen::generate(const AttributedTypeSyntaxRef &Type) {
  SyntaxDataRef Data[1];
  auto typeAST = generate(Type.getBaseType(Data));

  if (auto attributes = Type.getAttributes(Data)) {
    TypeAttributes attrs = generateTypeAttributes(*attributes);
    if (!attrs.empty()) {
      typeAST = new (*Context) AttributedTypeRepr(attrs, typeAST);
    }
  }

  if (auto specifier = Type.getSpecifier(Data)) {
    auto specifierLoc = getLoc(*specifier);
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
  SyntaxDataRef BaseData[1];
  auto base = Type.getBase(BaseData);
  if (!base) {
    if (CodeCompletion) {
      CodeCompletion->completeTypeSimpleBeginning();
    }
    return new (*Context) ErrorTypeRepr(getASTRange(Type));
  }

  if (auto *parsedTyR = generate(std::move(*base))) {
    if (CodeCompletion) {
      CodeCompletion->setParsedTypeLoc(parsedTyR);
      SyntaxDataRef PeriodData[1];
      if (Type.getPeriod(PeriodData)) {
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
  SyntaxDataRef ElementsData[1];
  auto elements = Type.getElements(ElementsData);
  assert(!elements.empty());

  SmallVector<TypeRepr *, 4> elementTypeReprs;
  for (auto element = elements.begin(); element != elements.end(); ++element) {
    SyntaxDataRef TypeData[1];
    auto elementType = (*element).getType(TypeData);
    TypeRepr *elementTypeRepr;
    if (auto someSyntax = elementType.getAs<SomeTypeSyntaxRef>()) {
      SyntaxDataRef SomeSpecifierData[1];
      // Some types inside type compositions are not valid. Diagnose and ignore
      // the 'some'.
      auto diag = diagnose(someSyntax->getSomeSpecifier(SomeSpecifierData),
                           diag::opaque_mid_composition);
      diag.fixItRemove(getLoc(someSyntax->getSomeSpecifier(SomeSpecifierData)));

      // Don't suggest adding 'some' before the composition if there is already
      // a 'some' before.
      bool isAlreadySomeType =
          Type.getParentRef() && Type.getParentRef()->is<SomeTypeSyntaxRef>();
      if (!isAlreadySomeType) {
        diag.fixItInsert(getContentStartLoc(Type), "some ");
      }

      SyntaxDataRef BaseTypeData[1];
      elementTypeRepr = generate(someSyntax->getBaseType(BaseTypeData));
    } else {
      elementTypeRepr = generate(std::move(elementType));
    }

    if (elementTypeRepr) {
      elementTypeReprs.push_back(elementTypeRepr);
    }
  }

  SourceLoc firstTypeLoc;
  if (!elements.empty()) {
    SyntaxDataRef Data[1];
    firstTypeLoc = getContentStartLoc(elements.getChild(0, Data));
  }
  return CompositionTypeRepr::create(*Context, elementTypeReprs, firstTypeLoc,
                                     getASTRange(Type));
}

TypeRepr *ASTGen::generate(const DictionaryTypeSyntaxRef &Type) {
  SyntaxDataRef Data[1];
  SourceLoc ColonLoc = getLoc(Type.getColon(Data));

  TypeRepr *KeyType = generate(Type.getKeyType(Data));
  TypeRepr *ValueType = generate(Type.getValueType(Data));
  auto Range = getASTRange(Type);

  return new (*Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const FunctionTypeSyntaxRef &FuncSyntax) {
  SyntaxDataRef ArgsData[1];
  SyntaxDataRef ElementsData[1];
  TupleTypeRepr *argumentTypes = argumentTypes =
      generateTuple(FuncSyntax.getArguments(ArgsData).getElements(ElementsData),
                    getASTRange(FuncSyntax), /*IsInFunctionSignature=*/true);

  if (!argumentTypes) {
    return new (*Context) ErrorTypeRepr(getASTRange(FuncSyntax));
  }

  SyntaxDataRef Data[1];
  SourceLoc asyncLoc, throwsLoc;
  std::tie(asyncLoc, throwsLoc) = generateEffectsSpecifiers(
      FuncSyntax.getEffectsSpecifiers(Data), /*RethrowsAllowed=*/false);

  auto arrowLoc = getLoc(FuncSyntax.getArrow(Data));
  auto returnType = generate(FuncSyntax.getReturnType(Data));
  if (!returnType) {
    return new (*Context) ErrorTypeRepr(getASTRange(FuncSyntax));
    ;
  }

  return new (*Context) FunctionTypeRepr(nullptr, argumentTypes, asyncLoc,
                                        throwsLoc, arrowLoc, returnType);
}

TypeRepr *
ASTGen::generate(const ImplicitlyUnwrappedOptionalTypeSyntaxRef &Type) {
  SyntaxDataRef Data[1];
  auto baseTypeRepr = generate(Type.getWrappedType(Data));
  auto exclamationLoc = getLoc(Type.getExclamationMark(Data));
  return new (*Context)
      ImplicitlyUnwrappedOptionalTypeRepr(baseTypeRepr, exclamationLoc);
}

TypeRepr *ASTGen::generate(const MemberTypeIdentifierSyntaxRef &Type) {
  SmallVector<ComponentIdentTypeRepr *, 4> components;
  gatherTypeIdentifierComponents(Type, components);
  return IdentTypeRepr::create(*Context, components);
}

TypeRepr *ASTGen::generate(const MetatypeTypeSyntaxRef &Type) {
  SyntaxDataRef Data[1];
  auto baseTypeRepr = generate(Type.getBaseType(Data));
  auto metaLoc = getLoc(Type.getTypeOrProtocol(Data));
  auto metaText = Type.getTypeOrProtocol(Data).getText();
  if (metaText == "Type") {
    return new (*Context) MetatypeTypeRepr(baseTypeRepr, metaLoc);
  } else if (metaText == "Protocol") {
    return new (*Context) ProtocolTypeRepr(baseTypeRepr, metaLoc);
  } else {
    llvm_unreachable("Meta part must be 'Type' or 'Protocol'");
  }
}

TypeRepr *ASTGen::generate(const OptionalTypeSyntaxRef &Type) {
  SyntaxDataRef Data[1];
  auto baseTypeRepr = generate(Type.getWrappedType(Data));
  auto questionLoc = getLoc(Type.getQuestionMark(Data));
  return new (*Context) OptionalTypeRepr(baseTypeRepr, questionLoc);
}

TypeRepr *ASTGen::generate(const SimpleTypeIdentifierSyntaxRef &Type) {
  SyntaxDataRef Data[1];
  if (Type.getName(Data).getTokenKind() == tok::kw_Any) {
    auto anyLoc = getLoc(Type.getName(Data));
    return CompositionTypeRepr::createEmptyComposition(*Context, anyLoc);
  }

  auto typeRepr = generateTypeIdentifier(Type);
  return IdentTypeRepr::create(*Context, {typeRepr});
}

TypeRepr *ASTGen::generate(const SomeTypeSyntaxRef &Type) {
  SyntaxDataRef Data[1];
  auto someLoc = getLoc(Type.getSomeSpecifier(Data));
  auto baseTypeRepr = generate(Type.getBaseType(Data));
  return new (*Context) OpaqueReturnTypeRepr(someLoc, baseTypeRepr);
}

TypeRepr *ASTGen::generate(const TupleTypeSyntaxRef &Type) {
  SyntaxDataRef Data[1];
  return generateTuple(Type.getElements(Data), getASTRange(Type),
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
    SyntaxDataRef Data[1];
    SyntaxRef child = *Type.getChildRef(0, Data);
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
    SyntaxDataRef Data[1];
    if (auto elem = Type.getChildRef(i, Data)) {
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
    SyntaxDataRef Data[1];
    auto token = specifier.getSpecifier(Data);

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
  SyntaxDataRef Data[1];
  SmallVector<TypeRepr *, 4> args;
  for (auto arg : ClauseSyntax.getArguments(Data)) {
    SyntaxDataRef Data2[1];
    auto typeRepr = generate(arg.getArgumentType(Data2));
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
    SyntaxDataRef Data10[1];
    auto element = Elements.getChild(i, Data10);

    // If we are in a function type, complain about elements that have a first
    // name that is not '_'.
    // If we are not in a function type, complain about elements with a second
    // name.
    if (IsInFunctionSignature) {
      SyntaxDataRef Data[1];
      if (element.getName(Data)) {
        SyntaxDataRef Data2[1];
        auto nameToken = *element.getName(Data2);
        if (nameToken.getText() != "_") {
          auto nameIdentifier = Context->getIdentifier(nameToken.getText());
          auto diag = diagnose(nameToken, diag::function_type_argument_label,
                               nameIdentifier);
          SyntaxDataRef Data3[1];
          if (element.getSecondName(Data3)) {
            if (element.getSecondName(Data3)->getText() == "_") {
              // The names are of the form `first '_' ':' type`
              // Offer to remove both names.
              SyntaxDataRef Data4[1];
              diag.fixItRemoveChars(getLoc(nameToken),
                                    getLeadingTriviaLoc(element.getType(Data4)));
            } else {
              // The names are of the form `first second ':' type`
              // The first name is invalid. Replace it by '_'.
              diag.fixItReplace(getLoc(nameToken), "_");
            }
          } else {
            // If we only have a first name (i.e. name ':' type) suggest
            // changing it to `'_' name ':' type`
            diag.fixItInsert(getLoc(nameToken), "_ ");
          }
        }
      }
    } else {
      SyntaxDataRef Data[1];
      SyntaxDataRef Data2[1];
      // We're in a proper tuple
      if (element.getName(Data) && element.getSecondName(Data2)) {
        SyntaxDataRef Data3[1];
        // True tuples can't have two labels
        auto nameToken = *element.getName(Data3);

        auto diag = diagnose(nameToken, diag::tuple_type_multiple_labels);
        if (nameToken.getText() == "_") {
          // If the first name is '_', remove both names.
          SyntaxDataRef Data4[1];
          diag.fixItRemoveChars(getLoc(nameToken),
                                getLeadingTriviaLoc(element.getType(Data4)));
        } else {
          SyntaxDataRef Data4[1];
          // Otherwise, remove the second name
          diag.fixItRemove(getLoc(*element.getSecondName(Data4)));
        }
      }
    }

    TupleTypeReprElement elementAST;
    SyntaxDataRef Data[1];
    elementAST.Type = generate(element.getType(Data));

    if (auto name = element.getName(Data)) {
      elementAST.NameLoc = getLoc(*name);
      elementAST.Name = name->getText() == "_"
                            ? Identifier()
                            : Context->getIdentifier(name->getIdentifierText());
    }
    if (auto secondName = element.getSecondName(Data)) {
      elementAST.SecondNameLoc = getLoc(*secondName);
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
    if (auto colon = element.getColon(Data)) {
      elementAST.ColonLoc = getLoc(*colon);
    }

    if (auto inOut = element.getInOut(Data)) {
      auto inOutLoc = getLoc(*inOut);
      if (isa<InOutTypeRepr>(elementAST.Type)) {
        // If the parsed type is already attributed, suggest removing
        // `inout`.
        diagnose(inOutLoc, diag::parameter_specifier_repeated)
            .fixItRemove(inOutLoc);
      } else {
        SyntaxDataRef Data2[1];
        // Otherwise suggest moving it before the type.
        diagnose(inOutLoc, diag::parameter_specifier_as_attr_disallowed,
                 "inout")
            .fixItRemove(inOutLoc)
            .fixItInsert(getContentStartLoc(element.getType(Data2)), "inout ");
      }

      // If the type is not already attributed, apply the inout attribute to
      // recover
      if (!isa<InOutTypeRepr>(elementAST.Type)) {
        auto inOutLoc = getLoc(*inOut);
        elementAST.Type =
            new (*Context) InOutTypeRepr(elementAST.Type, inOutLoc);
      }
    }

    if (auto ellipsis = element.getEllipsis(Data)) {
      if (ellipsisLoc.isInvalid()) {
        // Seeing the first ellipsis
        ellipsisLoc = getLoc(*ellipsis);
        ellipsisIdx = i;
      } else {
        // We have already seen an ellipsis. Diagnose.
        diagnose(*ellipsis, diag::multiple_ellipsis_in_tuple)
            .highlight(ellipsisLoc) // ellipsisLoc is the previous ellipsis
            .fixItRemove(getLoc(*ellipsis));
      }
    }

    if (element.getInitializer(Data)) {
      SyntaxDataRef Data2[1];
      // Initializers aren't valid in function types.
      auto init = *element.getInitializer(Data2);
      diagnose(init, diag::tuple_type_init)
          .fixItRemoveChars(getRangeWithTrivia(init));
    }

    if (auto comma = element.getTrailingComma(Data)) {
      elementAST.TrailingCommaLoc = getLoc(*comma);
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
    if (!elem.is<AttributeSyntaxRef>()) {
      assert(false && "We don't have custom type attributes");
      continue;
    }
    auto attrSyntax = std::move(elem).castTo<AttributeSyntaxRef>();

    SyntaxDataRef Data[1];
    auto attrName = attrSyntax.getAttributeName(Data).getText();

    // If we haven't recorded the location of the first '@' yet, do so now.
    auto atLoc = getLoc(attrSyntax.getAtSignToken(Data));
    if (attrs.AtLoc.isInvalid()) {
      attrs.AtLoc = atLoc;
    }

    auto attr = TypeAttributes::getAttrKindFromString(attrName);
    if (attr == TAK_Count) {
      auto declAttrID = DeclAttribute::getAttrKindFromString(attrName);
      if (declAttrID == DAK_Count) {
        // Not a decl or type attribute.
        diagnose(attrSyntax.getAttributeName(Data), diag::unknown_attribute,
                 attrName);
      } else {
        diagnose(attrSyntax.getAttributeName(Data),
                 diag::decl_attribute_applied_to_type);
      }
      continue;
    }

    if (attrs.has(attr)) {
      diagnose(attrSyntax.getAtSignToken(Data), diag::duplicate_attribute,
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
        diagnose(attrSyntax.getAtSignToken(Data), diag::duplicate_attribute,
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
      auto arg = attrSyntax.getArgument(Data);
      if (!arg) {
        diagnose(atLoc, diag::opened_attribute_id_value);
        continue;
      }

      if (!arg->is<TokenSyntaxRef>() ||
          arg->castTo2<TokenSyntaxRef>().getTokenKind() != tok::string_literal) {
        diagnose(*arg, diag::opened_attribute_id_value);
        break;
      }
      auto tokText = arg->castTo2<TokenSyntaxRef>().getText();
      // Remove quotes from the string literal.
      auto literalText = tokText.slice(1, tokText.size() - 1);
      if (auto openedID = UUID::fromString(literalText.str().c_str())) {
        attrs.OpenedID = openedID;
      } else {
        diagnose(*arg, diag::opened_attribute_id_value);
      }
      break;
    }
    case TAK_differentiable: {
      DifferentiabilityKind diffKind = DifferentiabilityKind::Normal;

      auto arg = attrSyntax.getArgument(Data);
      if (arg && arg->is<DifferentiableAttributeArgumentsSyntaxRef>()) {
        SyntaxDataRef Data2[1];
        auto diffKindSyntax =
            arg->castTo2<DifferentiableAttributeArgumentsSyntaxRef>()
                .getDiffKind(Data2);
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
          diagnose(*arg, diag::attr_differentiable_kind_not_supported,
                   diffKindText)
              .fixItReplaceChars(getRangeWithoutTrivia(*arg), "reverse");
          continue;
        case DifferentiabilityKind::NonDifferentiable:
          diagnose(*arg, diag::attr_differentiable_unknown_kind, diffKindText)
              .fixItReplaceChars(getRangeWithoutTrivia(*arg), "reverse");
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
          if (diffArgSyntax.getDiffKindComma(Data)) {
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

      auto arg = attrSyntax.getArgument(Data);
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
        SyntaxDataRef Data2[1];
        auto conventionName =
            conventionAttributeArgs->getConvention(Data2).getIdentifierText();
        conventionName = Context->getIdentifier(conventionName).str();
        convention.Name = conventionName;
        SyntaxDataRef Data3[1];
        if (auto cType = conventionAttributeArgs->getCType(Data3)) {
          // If the attribute doesn't have a cType, this has already been
          // diagnosed in the parser
          auto cTypeToken = *cType;
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
        SyntaxDataRef Data2[1];
        if (witness->getNameTok(Data2).getIdentifierText() != "witness_method") {
          diagnose(witness->getNameTok(Data2),
                   diag::convention_attribute_unkown_name);
          continue;
        }
        if (!witness->getStringOrDeclname(Data2).is<DeclNameSyntaxRef>()) {
          diagnose(witness->getStringOrDeclname(Data2),
                   diag::convention_attribute_witness_method_expected_protocol);
          continue;
        }
        auto protocolName =
            witness->getStringOrDeclname(Data2).castTo2<DeclNameSyntaxRef>();
        convention.Name = "witness_method";
        convention.WitnessMethodProtocol = generateDeclNameRef(protocolName);
      } else {
        SyntaxDataRef Data2[1];
        // Unknown attribute. Diagnose and ignore.
        diagnose(witness->getNameTok(Data2), diag::convention_attribute_unkown_name);
        continue;
      }
      attrs.ConventionArguments = convention;
      break;
    }
    case TAK__opaqueReturnTypeOf: {
      SyntaxDataRef Data2[1];
      auto arg = attrSyntax.getArgument(Data2);
      // @_opaqueReturnTypeOf("$sMangledName", 0)
      if (!arg) {
        SyntaxDataRef Data3[1];
        diagnose(attrSyntax.getAttributeName(Data3),
                 diag::attr_expected_string_literal, attrName);
        continue;
      }
      if (!arg->is<OpaqueReturnTypeOfAttributeArgumentsSyntaxRef>()) {
        SyntaxDataRef Data5[1];
        diagnose(attrSyntax.getAttributeName(Data5),
                 diag::attr_expected_string_literal, attrName);
        continue;
      }

      auto opaqueArg =
          arg->castTo2<OpaqueReturnTypeOfAttributeArgumentsSyntaxRef>();

      SyntaxDataRef Data3[1];
      auto manglingTok = opaqueArg.getMangledName(Data3);
      SyntaxDataRef Data4[1];
      auto indexTok = opaqueArg.getIndex(Data4);

      auto tokText = manglingTok.getText();
      auto mangling = tokText.slice(1, tokText.size() - 1);
      mangling = Context->getIdentifier(mangling).str();
      unsigned index;
      if (indexTok.getText().getAsInteger(10, index)) {
        diagnose(indexTok, diag::attr_expected_int_literal, attrName);
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
  SyntaxDataRef Data[1];
  auto name = TypeSyntax.getName(Data);
  auto declNameLoc = DeclNameLoc(getLoc(name));
  auto declNameRef = DeclNameRef(
      Context->getIdentifier(name.getIdentifierText()));
  diagnoseDollarIdentifier(name, /*DiagnoseDollarPrefix=*/true);
  SyntaxDataRef Data2[1];
  if (auto clause = TypeSyntax.getGenericArgumentClause(Data2)) {
    SourceRange range;
    SmallVector<TypeRepr *, 4> args;
    std::tie(range, args) = generateGenericArgs(*clause);
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
    SyntaxDataRef Data[1];
    gatherTypeIdentifierComponents(memberIdentifier->getBaseType(Data), Components);
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

  SyntaxDataRef Data[1];
  auto keyword = Type.getChildRef(0, Data)->getAs<TokenSyntaxRef>();
  if (!keyword || keyword->getText() != "protocol") {
    return nullptr;
  }
  SyntaxDataRef Data2[1];
  auto lAngle = Type.getChildRef(1, Data)->getAs<TokenSyntaxRef>();
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
    SyntaxDataRef Data3[1];
    if (auto elem = Type.getChildRef(i, Data3)->getAs<TypeSyntaxRef>()) {
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
