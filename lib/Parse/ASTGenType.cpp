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
  case SyntaxKind::DictionaryType:
    return generate(Type.castTo<DictionaryTypeSyntaxRef>());
  case SyntaxKind::MemberTypeIdentifier:
    return generate(Type.castTo<MemberTypeIdentifierSyntaxRef>());
  case SyntaxKind::SimpleTypeIdentifier:
    return generate(Type.castTo<SimpleTypeIdentifierSyntaxRef>());
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
}

TypeRepr *ASTGen::generate(const ArrayTypeSyntaxRef &Type) {
  TypeRepr *ElementType = generate(Type.getElementType());
  return new (Context) ArrayTypeRepr(ElementType, getASTRange(Type));
}

TypeRepr *ASTGen::generate(const AttributedTypeSyntaxRef &Type) {
  auto typeAST = generate(Type.getBaseType());

  if (auto attributes = Type.getAttributes()) {
    TypeAttributes attrs = generateTypeAttributes(*attributes);
    if (!attrs.empty()) {
      typeAST = new (Context) AttributedTypeRepr(attrs, typeAST);
    }
  }

  if (auto specifier = Type.getSpecifier()) {
    auto specifierLoc = getLoc(*specifier);
    auto specifierText = specifier->getText();

    // don't apply multiple specifiers to a type: that's invalid and was already
    // reported in the parser, handle gracefully
    if (!isa<SpecifierTypeRepr>(typeAST)) {
      if (specifierText == "inout") {
        typeAST = new (Context) InOutTypeRepr(typeAST, specifierLoc);
      } else if (specifierText == "__owned") {
        typeAST = new (Context) OwnedTypeRepr(typeAST, specifierLoc);
      } else if (specifierText == "__shared") {
        typeAST = new (Context) SharedTypeRepr(typeAST, specifierLoc);
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

TypeRepr *ASTGen::generate(const TupleTypeSyntaxRef &Type) {
  return generateTuple(Type.getElements(), getASTRange(Type),
                       /*IsInFunctionSignature=*/false);
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

TupleTypeRepr *
ASTGen::generateTuple(const TupleTypeElementListSyntaxRef &Elements,
                      const SourceRange &RangeWithParens,
                      bool IsInFunctionSignature) {
  // TODO: Remove once generating function types has been migrated.
  IsInFunctionSignature = this->IsInFunctionType;
  SmallVector<TupleTypeReprElement, 4> tupleElements;

  SourceLoc ellipsisLoc;
  unsigned ellipsisIdx;

  for (unsigned i = 0; i < Elements.size(); i++) {
    auto element = Elements[i];

    // If we are in a function type, complain about elements that have a first
    // name that is not '_'.
    // If we are not in a function type, complain about elements with a second
    // name.
    if (IsInFunctionSignature) {
      if (element.getName()) {
        auto nameToken = *element.getName();
        if (nameToken.getText() != "_") {
          auto nameIdentifier = Context.getIdentifier(nameToken.getText());
          auto diag = diagnose(nameToken, diag::function_type_argument_label,
                               nameIdentifier);
          if (element.getSecondName()) {
            if (element.getSecondName()->getText() == "_") {
              // The names are of the form `first '_' ':' type`
              // Offer to remove both names.
              diag.fixItRemoveChars(getLoc(nameToken),
                                    getLeadingTriviaLoc(element.getType()));
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
      // We're in a proper tuple
      if (element.getName() && element.getSecondName()) {
        // True tuples can't have two labels
        auto nameToken = *element.getName();

        auto diag = diagnose(nameToken, diag::tuple_type_multiple_labels);
        if (nameToken.getText() == "_") {
          // If the first name is '_', remove both names.
          diag.fixItRemoveChars(getLoc(nameToken),
                                getLeadingTriviaLoc(element.getType()));
        } else {
          // Otherwise, remove the second name
          diag.fixItRemove(getLoc(*element.getSecondName()));
        }
      }
    }

    TupleTypeReprElement elementAST;
    elementAST.Type = generate(element.getType());

    if (auto name = element.getName()) {
      elementAST.NameLoc = getLoc(*name);
      elementAST.Name = name->getText() == "_"
                            ? Identifier()
                            : Context.getIdentifier(name->getIdentifierText());
    }
    if (auto secondName = element.getSecondName()) {
      elementAST.SecondNameLoc = getLoc(*secondName);
      elementAST.SecondName =
          secondName->getText() == "_"
              ? Identifier()
              : Context.getIdentifier(secondName->getIdentifierText());
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
      elementAST.ColonLoc = getLoc(*colon);
    }

    if (auto inOut = element.getInOut()) {
      auto inOutLoc = getLoc(*inOut);
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
            .fixItInsert(getContentStartLoc(element.getType()), "inout ");
      }

      // If the type is not already attributed, apply the inout attribute to
      // recover
      if (!isa<InOutTypeRepr>(elementAST.Type)) {
        auto inOutLoc = getLoc(*inOut);
        elementAST.Type =
            new (Context) InOutTypeRepr(elementAST.Type, inOutLoc);
      }
    }

    if (auto ellipsis = element.getEllipsis()) {
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

    if (element.getInitializer()) {
      // Initializers aren't valid in function types.
      auto init = *element.getInitializer();
      diagnose(init, diag::tuple_type_init)
          .fixItRemoveChars(getRangeWithTrivia(init));
    }

    if (auto comma = element.getTrailingComma()) {
      elementAST.TrailingCommaLoc = getLoc(*comma);
    }
    tupleElements.push_back(elementAST);
  }
  if (ellipsisLoc.isInvalid()) {
    // If we don't have an ellipsis the ellipsis index must point after the last
    // element to indicate this.
    ellipsisIdx = tupleElements.size();
  }

  return TupleTypeRepr::create(Context, tupleElements, RangeWithParens,
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
    auto attrSyntax = elem.castTo<AttributeSyntaxRef>();

    auto attrName = attrSyntax.getAttributeName().getText();

    // If we haven't recorded the location of the first '@' yet, do so now.
    auto atLoc = getLoc(attrSyntax.getAtSignToken());
    if (attrs.AtLoc.isInvalid()) {
      attrs.AtLoc = atLoc;
    }

    auto attr = TypeAttributes::getAttrKindFromString(attrName);
    if (attr == TAK_Count) {
      auto declAttrID = DeclAttribute::getAttrKindFromString(attrName);
      if (declAttrID == DAK_Count) {
        // Not a decl or type attribute.
        diagnose(attrSyntax.getAttributeName(), diag::unknown_attribute,
                 attrName);
      } else {
        diagnose(attrSyntax.getAttributeName(),
                 diag::decl_attribute_applied_to_type);
      }
      continue;
    }

    if (attrs.has(attr)) {
      diagnose(attrSyntax.getAtSignToken(), diag::duplicate_attribute,
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
        diagnose(attrSyntax.getAtSignToken(), diag::duplicate_attribute,
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
          arg->castTo<TokenSyntaxRef>().getTokenKind() != tok::string_literal) {
        diagnose(*arg, diag::opened_attribute_id_value);
        break;
      }
      auto tokText = arg->castTo<TokenSyntaxRef>().getText();
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

      auto arg = attrSyntax.getArgument();
      if (arg && arg->is<DifferentiableAttributeArgumentsSyntaxRef>()) {
        auto diffKindSyntax =
            arg->castTo<DifferentiableAttributeArgumentsSyntaxRef>()
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
              arg->castTo<DifferentiableAttributeArgumentsSyntaxRef>();
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
        convention.Name = Context.getIdentifier(conventionName).str();
      } else if (auto conventionAttributeArgs =
                     arg->getAs<CTypeConventionAttributeArgumentsSyntaxRef>()) {
        auto conventionName =
            conventionAttributeArgs->getConvention().getIdentifierText();
        conventionName = Context.getIdentifier(conventionName).str();
        convention.Name = conventionName;
        if (auto cType = conventionAttributeArgs->getCType()) {
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
          cTypeString = Context.getIdentifier(cTypeString).str();
          auto cTypeStringLoc = getLoc(cTypeToken);
          convention.ClangType = {cTypeString, cTypeStringLoc};
        }
      } else if (auto witness =
                     arg->getAs<NamedAttributeStringArgumentSyntaxRef>()) {
        if (witness->getNameTok().getIdentifierText() != "witness_method") {
          diagnose(witness->getNameTok(),
                   diag::convention_attribute_unkown_name);
          continue;
        }
        if (!witness->getStringOrDeclname().is<DeclNameSyntaxRef>()) {
          diagnose(witness->getStringOrDeclname(),
                   diag::convention_attribute_witness_method_expected_protocol);
          continue;
        }
        auto protocolName =
            witness->getStringOrDeclname().castTo<DeclNameSyntaxRef>();
        convention.Name = "witness_method";
        convention.WitnessMethodProtocol = generateDeclNameRef(protocolName);
      } else {
        // Unknown attribute. Diagnose and ignore.
        diagnose(witness->getNameTok(), diag::convention_attribute_unkown_name);
        continue;
      }
      attrs.ConventionArguments = convention;
      break;
    }
    case TAK__opaqueReturnTypeOf: {
      auto arg = attrSyntax.getArgument();
      // @_opaqueReturnTypeOf("$sMangledName", 0)
      if (!arg) {
        diagnose(attrSyntax.getAttributeName(),
                 diag::attr_expected_string_literal, attrName);
        continue;
      }
      if (!arg->is<OpaqueReturnTypeOfAttributeArgumentsSyntaxRef>()) {
        diagnose(attrSyntax.getAttributeName(),
                 diag::attr_expected_string_literal, attrName);
        continue;
      }

      auto opaqueArg =
          arg->castTo<OpaqueReturnTypeOfAttributeArgumentsSyntaxRef>();

      auto manglingTok = opaqueArg.getMangledName();
      auto indexTok = opaqueArg.getIndex();

      auto tokText = manglingTok.getText();
      auto mangling = tokText.slice(1, tokText.size() - 1);
      mangling = Context.getIdentifier(mangling).str();
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

