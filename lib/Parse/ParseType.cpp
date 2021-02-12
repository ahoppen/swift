//===--- ParseType.cpp - Swift Language Parser for Types ------------------===//
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
//
// Type Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "ParseList.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/ParsedSyntaxBuilders.h"
#include "swift/Parse/ParsedSyntaxRecorder.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace swift::syntax;

TypeRepr *Parser::applyAttributeToType(TypeRepr *ty,
                                       const TypeAttributes &attrs,
                                       ParamDecl::Specifier specifier,
                                       SourceLoc specifierLoc) {
  // Apply those attributes that do apply.
  if (!attrs.empty()) {
    ty = new (Context) AttributedTypeRepr(attrs, ty);
  }

  // Apply 'inout' or '__shared' or '__owned'
  if (specifierLoc.isValid()) {
    switch (specifier) {
    case ParamDecl::Specifier::Owned:
      ty = new (Context) OwnedTypeRepr(ty, specifierLoc);
      break;
    case ParamDecl::Specifier::InOut:
      ty = new (Context) InOutTypeRepr(ty, specifierLoc);
      break;
    case ParamDecl::Specifier::Shared:
      ty = new (Context) SharedTypeRepr(ty, specifierLoc);
      break;
    case ParamDecl::Specifier::Default:
      break;
    }
  }

  return ty;
}

LayoutConstraint Parser::parseLayoutConstraint(Identifier LayoutConstraintID) {
  LayoutConstraint layoutConstraint =
      getLayoutConstraint(LayoutConstraintID, Context);
  assert(layoutConstraint->isKnownLayout() &&
         "Expected layout constraint definition");

  if (!layoutConstraint->isTrivial())
    return layoutConstraint;

  SourceLoc LParenLoc;
  if (!consumeIf(tok::l_paren, LParenLoc)) {
    // It is a trivial without any size constraints.
    return LayoutConstraint::getLayoutConstraint(LayoutConstraintKind::Trivial,
                                                 Context);
  }

  int size = 0;
  int alignment = 0;

  auto ParseTrivialLayoutConstraintBody = [&] () -> bool {
    // Parse the size and alignment.
    if (Tok.is(tok::integer_literal)) {
      if (Tok.getText().getAsInteger(10, size)) {
        diagnose(Tok.getLoc(), diag::layout_size_should_be_positive);
        return true;
      }
      consumeToken();
      if (consumeIf(tok::comma)) {
        // parse alignment.
        if (Tok.is(tok::integer_literal)) {
          if (Tok.getText().getAsInteger(10, alignment)) {
            diagnose(Tok.getLoc(), diag::layout_alignment_should_be_positive);
            return true;
          }
          consumeToken();
        } else {
          diagnose(Tok.getLoc(), diag::layout_alignment_should_be_positive);
          return true;
        }
      }
    } else {
      diagnose(Tok.getLoc(), diag::layout_size_should_be_positive);
      return true;
    }
    return false;
  };

  if (ParseTrivialLayoutConstraintBody()) {
    // There was an error during parsing.
    skipUntil(tok::r_paren);
    consumeIf(tok::r_paren);
    return LayoutConstraint::getUnknownLayout();
  }

  if (!consumeIf(tok::r_paren)) {
    // Expected a closing r_paren.
    diagnose(Tok.getLoc(), diag::expected_rparen_layout_constraint);
    consumeToken();
    return LayoutConstraint::getUnknownLayout();
  }

  if (size < 0) {
    diagnose(Tok.getLoc(), diag::layout_size_should_be_positive);
    return LayoutConstraint::getUnknownLayout();
  }

  if (alignment < 0) {
    diagnose(Tok.getLoc(), diag::layout_alignment_should_be_positive);
    return LayoutConstraint::getUnknownLayout();
  }

  // Otherwise it is a trivial layout constraint with
  // provided size and alignment.
  return LayoutConstraint::getLayoutConstraint(layoutConstraint->getKind(), size,
                                               alignment, Context);
}

/// parseTypeSimple
///   type-simple:
///     type-identifier
///     type-tuple
///     type-composition-deprecated
///     'Any'
///     type-simple '.Type'
///     type-simple '.Protocol'
///     type-simple '?'
///     type-simple '!'
///     type-collection
///     type-array
ParserResult<TypeRepr> Parser::parseTypeSimple(Diag<> MessageID) {
  EnableSyntaxParsingRAII enableSyntaxParsing(*this);

  SourceLoc previousTokLoc = PreviousLoc;
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeSimpleSyntax(MessageID);
  auto status = result.getStatus();
  SyntaxContext->addSyntax(result.take());
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  
  Optional<GeneratingFunctionTypeRAII> InFunctionType;
  bool isFunctionType =
      Tok.isAny(tok::arrow, tok::kw_throws, tok::kw_rethrows) ||
      Tok.isContextualKeyword("async");
  if (isFunctionType) {
    InFunctionType.emplace(ASTGenerator);
  }
  
  auto typeRepr =
      ASTGenerator.generate(collectionType, typeLoc, previousTokLoc, MessageID);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);

}

ParserResult<TypeRepr> Parser::parseType() {
  return parseType(diag::expected_type);
}

ParserResult<TypeRepr> Parser::parseSILBoxType(GenericParamList *generics,
                                               const TypeAttributes &attrs) {
  auto LBraceLoc = consumeToken(tok::l_brace);
  
  SmallVector<SILBoxTypeRepr::Field, 4> Fields;
  if (!Tok.is(tok::r_brace)) {
    for (;;) {
      bool Mutable;
      if (Tok.is(tok::kw_var)) {
        Mutable = true;
      } else if (Tok.is(tok::kw_let)) {
        Mutable = false;
      } else {
        diagnose(Tok, diag::sil_box_expected_var_or_let);
        return makeParserError();
      }
      SourceLoc VarOrLetLoc = consumeToken();
      
      auto fieldTy = parseType();
      if (!fieldTy.getPtrOrNull())
        return makeParserError();
      Fields.push_back({VarOrLetLoc, Mutable, fieldTy.get()});
      
      if (!consumeIf(tok::comma))
        break;
    }
  }
  
  if (!Tok.is(tok::r_brace)) {
    diagnose(Tok, diag::sil_box_expected_r_brace);
    return makeParserError();
  }
  
  auto RBraceLoc = consumeToken(tok::r_brace);

  SourceLoc LAngleLoc, RAngleLoc;
  SmallVector<TypeRepr*, 4> Args;
  if (startsWithLess(Tok)) {
    LAngleLoc = consumeStartingLess();
    for (;;) {
      auto argTy = parseType();
      if (!argTy.getPtrOrNull())
        return makeParserError();
      Args.push_back(argTy.get());
      if (!consumeIf(tok::comma))
        break;
    }
    if (!startsWithGreater(Tok)) {
      diagnose(Tok, diag::sil_box_expected_r_angle);
      return makeParserError();
    }
    
    RAngleLoc = consumeStartingGreater();
  }
  
  auto repr = SILBoxTypeRepr::create(Context, generics,
                                     LBraceLoc, Fields, RBraceLoc,
                                     LAngleLoc, Args, RAngleLoc);
  return makeParserResult(applyAttributeToType(repr, attrs,
                                               ParamDecl::Specifier::Owned,
                                               SourceLoc()));
}


/// parseType
///   type:
///     attribute-list type-composition
///     attribute-list type-function
///
///   type-function:
///     type-composition 'async'? 'throws'? '->' type
///
ParserResult<TypeRepr> Parser::parseType(Diag<> MessageID,
                                         bool IsSILFuncDecl) {
  // Start a context for creating type syntax.
  SyntaxParsingContext TypeParsingContext(SyntaxContext,
                                          SyntaxContextKind::Type);
  // Parse attributes.
  ParamDecl::Specifier specifier;
  SourceLoc specifierLoc;
  TypeAttributes attrs;
  parseTypeAttributeList(specifier, specifierLoc, attrs);

  // Parse generic parameters in SIL mode.
  GenericParamList *generics = nullptr;
  SourceLoc substitutedLoc;
  GenericParamList *patternGenerics = nullptr;
  if (isInSILMode()) {
    generics = maybeParseGenericParams().getPtrOrNull();
    
    if (Tok.is(tok::at_sign) && peekToken().getText() == "substituted") {
      consumeToken(tok::at_sign);
      substitutedLoc = consumeToken(tok::identifier);
      patternGenerics = maybeParseGenericParams().getPtrOrNull();
      if (!patternGenerics) {
        diagnose(Tok.getLoc(), diag::sil_function_subst_expected_generics);
      }
    }
  }
  
  // In SIL mode, parse box types { ... }.
  if (isInSILMode() && Tok.is(tok::l_brace)) {
    if (patternGenerics) {
      diagnose(Tok.getLoc(), diag::sil_function_subst_expected_function);
    }
    return parseSILBoxType(generics, attrs);
  }

  ParserResult<TypeRepr> ty = parseTypeSimpleOrComposition(MessageID);
  if (ty.isNull())
    return ty;
  auto tyR = ty.get();
  auto status = ParserStatus(ty);

  // Parse effects specifiers.
  // Don't consume them, if there's no following '->', so we can emit a more
  // useful diagnostic when parsing a function decl.
  SourceLoc asyncLoc;
  SourceLoc throwsLoc;
  if (isAtFunctionTypeArrow()) {
    status |= parseEffectsSpecifiers(SourceLoc(), asyncLoc, throwsLoc,
                                     /*rethrows=*/nullptr);
  }

  // Handle type-function if we have an arrow.
  if (Tok.is(tok::arrow)) {
    SourceLoc arrowLoc = consumeToken();

    // Handle async/throws in the wrong place.
    parseEffectsSpecifiers(arrowLoc, asyncLoc, throwsLoc, /*rethrows=*/nullptr);

    ParserResult<TypeRepr> SecondHalf =
        parseType(diag::expected_type_function_result);
    if (SecondHalf.isNull()) {
      status.setIsParseError();
      return status;
    }
    status |= SecondHalf;

    if (SyntaxContext->isEnabled()) {
      ParsedFunctionTypeSyntaxBuilder Builder(*SyntaxContext);
      Builder.useReturnType(std::move(*SyntaxContext->popIf<ParsedTypeSyntax>()));
      Builder.useArrow(SyntaxContext->popToken());
      if (throwsLoc.isValid())
        Builder.useThrowsOrRethrowsKeyword(SyntaxContext->popToken());
      if (asyncLoc.isValid())
        Builder.useAsyncKeyword(SyntaxContext->popToken());

      auto InputNode(std::move(*SyntaxContext->popIf<ParsedTypeSyntax>()));
      if (InputNode.is<ParsedTupleTypeSyntax>()) {
        auto TupleTypeNode = std::move(InputNode).castTo<ParsedTupleTypeSyntax>();
        // Decompose TupleTypeSyntax and repack into FunctionType.
        auto LeftParen = TupleTypeNode.getDeferredLeftParen(SyntaxContext);
        auto Arguments = TupleTypeNode.getDeferredElements(SyntaxContext);
        auto RightParen = TupleTypeNode.getDeferredRightParen(SyntaxContext);
        Builder
          .useLeftParen(std::move(LeftParen))
          .useArguments(std::move(Arguments))
          .useRightParen(std::move(RightParen));
      } else {
        Builder.addArgumentsMember(ParsedSyntaxRecorder::makeTupleTypeElement(
            std::move(InputNode), /*TrailingComma=*/None, *SyntaxContext));
      }
      SyntaxContext->addSyntax(Builder.build());
    }

    TupleTypeRepr *argsTyR = nullptr;
    if (auto *TTArgs = dyn_cast<TupleTypeRepr>(tyR)) {
      argsTyR = TTArgs;
    } else {
      bool isVoid = false;
      if (const auto Void = dyn_cast<SimpleIdentTypeRepr>(tyR)) {
        if (Void->getNameRef().isSimpleName(Context.Id_Void)) {
          isVoid = true;
        }
      }

      if (isVoid) {
        diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
          .fixItReplace(tyR->getStartLoc(), "()");
        argsTyR = TupleTypeRepr::createEmpty(Context, tyR->getSourceRange());
      } else {
        diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
          .highlight(tyR->getSourceRange())
          .fixItInsert(tyR->getStartLoc(), "(")
          .fixItInsertAfter(tyR->getEndLoc(), ")");
        argsTyR = TupleTypeRepr::create(Context, {tyR},
                                        tyR->getSourceRange());
      }
    }
    
    // Parse substitutions for substituted SIL types.
    MutableArrayRef<TypeRepr *> invocationSubsTypes;
    MutableArrayRef<TypeRepr *> patternSubsTypes;
    if (isInSILMode()) {
      auto parseSubstitutions =
          [&](MutableArrayRef<TypeRepr*> &subs) -> Optional<bool> {
        if (!consumeIf(tok::kw_for)) return None;

        if (!startsWithLess(Tok)) {
          diagnose(Tok, diag::sil_function_subst_expected_l_angle);
          return false;
        }

        consumeStartingLess();

        SmallVector<TypeRepr*, 4> SubsTypesVec;
        for (;;) {
          auto argTy = parseType();
          if (!argTy.getPtrOrNull())
            return false;
          SubsTypesVec.push_back(argTy.get());
          if (!consumeIf(tok::comma))
            break;
        }
        if (!startsWithGreater(Tok)) {
          diagnose(Tok, diag::sil_function_subst_expected_r_angle);
          return false;
        }
        consumeStartingGreater();

        subs = Context.AllocateCopy(SubsTypesVec);
        return true;
      };

      // Parse pattern substitutions.  These must exist if we had pattern
      // generics above.
      if (patternGenerics) {
        auto result = parseSubstitutions(patternSubsTypes);
        if (!result || patternSubsTypes.empty()) {
          diagnose(Tok, diag::sil_function_subst_expected_subs);
          patternGenerics = nullptr;
        } else if (!*result) {
          return makeParserError();
        }
      }

      if (generics) {
        if (auto result = parseSubstitutions(invocationSubsTypes))
          if (!*result) return makeParserError();
      }

      if (Tok.is(tok::kw_for)) {
        diagnose(Tok, diag::sil_function_subs_without_generics);
        return makeParserError();
      }
    }

    tyR = new (Context) FunctionTypeRepr(generics, argsTyR, asyncLoc, throwsLoc,
                                         arrowLoc, SecondHalf.get(),
                                         patternGenerics, patternSubsTypes,
                                         invocationSubsTypes);
  } else if (auto firstGenerics = generics ? generics : patternGenerics) {
    // Only function types may be generic.
    auto brackets = firstGenerics->getSourceRange();
    diagnose(brackets.Start, diag::generic_non_function);

    // Forget any generic parameters we saw in the type.
    class EraseTypeParamWalker : public ASTWalker {
    public:
      bool walkToTypeReprPre(TypeRepr *T) override {
        if (auto ident = dyn_cast<ComponentIdentTypeRepr>(T)) {
          if (auto decl = ident->getBoundDecl()) {
            if (auto genericParam = dyn_cast<GenericTypeParamDecl>(decl))
              ident->overwriteNameRef(genericParam->createNameRef());
          }
        }
        return true;
      }

    } walker;

    if (tyR)
      tyR->walk(walker);
  }
  if (specifierLoc.isValid() || !attrs.empty())
    SyntaxContext->setCreateSyntax(SyntaxKind::AttributedType);

  return makeParserResult(status, applyAttributeToType(tyR, attrs, specifier,
                                                       specifierLoc));
}

ParserResult<TypeRepr> Parser::parseDeclResultType(Diag<> MessageID) {
  if (Tok.is(tok::code_complete)) {
    if (CodeCompletion)
      CodeCompletion->completeTypeDeclResultBeginning();
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionStatus();
  }

  auto result = parseType(MessageID);

  if (!result.isParseErrorOrHasCompletion()) {
    if (Tok.is(tok::r_square)) {
      auto diag = diagnose(Tok, diag::extra_rbracket);
      diag.fixItInsert(result.get()->getStartLoc(), getTokenText(tok::l_square));
      consumeToken();
      return makeParserErrorResult(new (Context)
                                       ErrorTypeRepr(getTypeErrorLoc()));
    }

    if (Tok.is(tok::colon)) {
      auto colonTok = consumeToken();
      auto secondType = parseType(diag::expected_dictionary_value_type);

      auto diag = diagnose(colonTok, diag::extra_colon);
      diag.fixItInsert(result.get()->getStartLoc(), getTokenText(tok::l_square));
      if (!secondType.isParseErrorOrHasCompletion()) {
        if (Tok.is(tok::r_square)) {
          consumeToken();
        } else {
          diag.fixItInsertAfter(secondType.get()->getEndLoc(), getTokenText(tok::r_square));
        }
      }
      return makeParserErrorResult(new (Context)
                                       ErrorTypeRepr(getTypeErrorLoc()));
    }
  }
  return result;
}

SourceLoc Parser::getTypeErrorLoc() const {
  // Use the same location as a missing close brace, etc.
  return getErrorOrMissingLoc();
}

ParserStatus Parser::parseGenericArguments(SmallVectorImpl<TypeRepr *> &Args,
                                           SourceLoc &LAngleLoc,
                                           SourceLoc &RAngleLoc) {
  SyntaxParsingContext GenericArgumentsContext(
      SyntaxContext, SyntaxKind::GenericArgumentClause);

  // Parse the opening '<'.
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  LAngleLoc = consumeStartingLess();

  {
    SyntaxParsingContext ListContext(SyntaxContext,
        SyntaxKind::GenericArgumentList);

    while (true) {
      SyntaxParsingContext ElementContext(SyntaxContext,
                                          SyntaxKind::GenericArgument);
      ParserResult<TypeRepr> Ty = parseType(diag::expected_type);
      if (Ty.isNull() || Ty.hasCodeCompletion()) {
        // Skip until we hit the '>'.
        RAngleLoc = skipUntilGreaterInTypeList();
        return ParserStatus(Ty);
      }

      Args.push_back(Ty.get());
      // Parse the comma, if the list continues.
      if (!consumeIf(tok::comma))
        break;
    }
  }

  if (!startsWithGreater(Tok)) {
    checkForInputIncomplete();
    diagnose(Tok, diag::expected_rangle_generic_arg_list);
    diagnose(LAngleLoc, diag::opening_angle);

    // Skip until we hit the '>'.
    RAngleLoc = skipUntilGreaterInTypeList();
    return makeParserError();
  } else {
    RAngleLoc = consumeStartingGreater();
  }

  return makeParserSuccess();
}

/// parseTypeIdentifier
///   
///   type-identifier:
///     identifier generic-args? ('.' identifier generic-args?)*
///
ParserResult<TypeRepr>
Parser::parseTypeIdentifier(bool isParsingQualifiedDeclBaseType) {
  EnableSyntaxParsingRAII enableSyntaxParsing(*this);

  SourceLoc previousTokLoc = PreviousLoc;
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeIdentifierSyntax(isParsingQualifiedDeclBaseType);
  auto status = result.getStatus();
  SyntaxContext->addSyntax(result.take());
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  auto typeRepr =
      ASTGenerator.generate(collectionType, typeLoc, previousTokLoc);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);
}

/// parseTypeSimpleOrComposition
///
///   type-composition:
///     'some'? type-simple
///     type-composition '&' type-simple
ParserResult<TypeRepr>
Parser::parseTypeSimpleOrComposition(Diag<> MessageID) {
  EnableSyntaxParsingRAII enableSyntaxParsing(*this);

  SourceLoc previousTokLoc = PreviousLoc;
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeSimpleOrCompositionSyntax(MessageID);
  auto status = result.getStatus();
  SyntaxContext->addSyntax(result.take());
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  
  Optional<GeneratingFunctionTypeRAII> InFunctionType;
  bool isFunctionType =
      Tok.isAny(tok::arrow, tok::kw_throws, tok::kw_rethrows) ||
      Tok.isContextualKeyword("async");
  if (isFunctionType) {
    InFunctionType.emplace(ASTGenerator);
  }
  
  auto typeRepr =
      ASTGenerator.generate(collectionType, typeLoc, previousTokLoc, MessageID);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);
}

ParserResult<TypeRepr> Parser::parseAnyType() {
  EnableSyntaxParsingRAII enableSyntaxParsing(*this);
  
  SourceLoc previousTokLoc = PreviousLoc;
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeAnySyntax();
  auto status = result.getStatus();
  SyntaxContext->addSyntax(result.take());
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  auto typeRepr =
  ASTGenerator.generate(collectionType, typeLoc, previousTokLoc);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);
}

/// parseOldStyleProtocolComposition
///   type-composition-deprecated:
///     'protocol' '<' '>'
///     'protocol' '<' type-composition-list-deprecated '>'
///
///   type-composition-list-deprecated:
///     type-identifier
///     type-composition-list-deprecated ',' type-identifier
ParserResult<TypeRepr> Parser::parseOldStyleProtocolComposition() {
  EnableSyntaxParsingRAII enableSyntaxParsing(*this);

  SourceLoc previousTokLoc = PreviousLoc;
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeOldStyleProtocolCompositionSyntax();
  auto status = result.getStatus();
  SyntaxContext->addSyntax(result.take());
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  auto typeRepr =
      ASTGenerator.generate(collectionType, typeLoc, previousTokLoc);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);

}

/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier? identifier ':' type
///     type
ParserResult<TypeRepr> Parser::parseTypeTupleBody() {
  EnableSyntaxParsingRAII enableSyntaxParsing(*this);
  
  SourceLoc previousTokLoc = PreviousLoc;
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeTupleBodySyntax();
  auto status = result.getStatus();
  SyntaxContext->addSyntax(result.take());
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  
  bool isFunctionType =
      Tok.isAny(tok::arrow, tok::kw_throws, tok::kw_rethrows) ||
      Tok.isContextualKeyword("async");
  if (isFunctionType) {
    GeneratingFunctionTypeRAII InFunctionType(ASTGenerator);
    auto typeRepr =
        ASTGenerator.generate(collectionType, typeLoc, previousTokLoc);
    if (!typeRepr) {
      status.setIsParseError();
    }
    return makeParserResult(status, typeRepr);
  } else {
    auto typeRepr =
        ASTGenerator.generate(collectionType, typeLoc, previousTokLoc);
    if (!typeRepr) {
      status.setIsParseError();
    }
    return makeParserResult(status, typeRepr);
  }
}


/// parseTypeArray - Parse the type-array production, given that we
/// are looking at the initial l_square.  Note that this index
/// clause is actually the outermost (first-indexed) clause.
///
///   type-array:
///     type-simple
///     type-array '[' ']'
///     type-array '[' expr ']'
///
ParserResult<TypeRepr> Parser::parseTypeArray(TypeRepr *Base) {
  assert(Tok.isFollowingLSquare());
  Parser::StructureMarkerRAII ParsingArrayBound(*this, Tok);
  SourceLoc lsquareLoc = consumeToken();
  ArrayTypeRepr *ATR = nullptr;
  
  // Handle a postfix [] production, a common typo for a C-like array.

  // If we have something that might be an array size expression, parse it as
  // such, for better error recovery.
  if (Tok.isNot(tok::r_square)) {
    auto sizeEx = parseExprBasic(diag::expected_expr);
    if (sizeEx.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
  }
  
  SourceLoc rsquareLoc;
  if (parseMatchingToken(tok::r_square, rsquareLoc,
                         diag::expected_rbracket_array_type, lsquareLoc))
    return makeParserErrorResult(Base);

  // If we parsed something valid, diagnose it with a fixit to rewrite it to
  // Swift syntax.
  diagnose(lsquareLoc, diag::new_array_syntax)
    .fixItInsert(Base->getStartLoc(), "[")
    .fixItRemove(lsquareLoc);
  
  // Build a normal array slice type for recovery.
  ATR = new (Context) ArrayTypeRepr(Base,
                              SourceRange(Base->getStartLoc(), rsquareLoc));
  return makeParserResult(ATR);
}

ParserResult<TypeRepr> Parser::parseTypeCollection() {
  EnableSyntaxParsingRAII enableSyntaxParsing(*this);

  SourceLoc previousTokLoc = PreviousLoc;
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeCollectionSyntax();
  auto status = result.getStatus();
  SyntaxContext->addSyntax(result.take());
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  auto typeRepr =
      ASTGenerator.generate(collectionType, typeLoc, previousTokLoc);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);
}

bool Parser::isOptionalToken(const Token &T) const {
  // A postfix '?' by itself is obviously optional.
  if (T.is(tok::question_postfix))
    return true;
  // A postfix or bound infix operator token that begins with '?' can be
  // optional too.
  if (T.is(tok::oper_postfix) || T.is(tok::oper_binary_unspaced)) {
    // We'll munch off the '?', so long as it is left-bound with
    // the type (i.e., parsed as a postfix or unspaced binary operator).
    return T.getText().startswith("?");
  }

  return false;
}

bool Parser::isImplicitlyUnwrappedOptionalToken(const Token &T) const {
  // A postfix '!' by itself, or a '!' in SIL mode, is obviously implicitly
  // unwrapped optional.
  if (T.is(tok::exclaim_postfix) || T.is(tok::sil_exclamation))
    return true;
  // A postfix or bound infix operator token that begins with '!' can be
  // implicitly unwrapped optional too.
  if (T.is(tok::oper_postfix) || T.is(tok::oper_binary_unspaced)) {
    // We'll munch off the '!', so long as it is left-bound with
    // the type (i.e., parsed as a postfix or unspaced binary operator).
    return T.getText().startswith("!");
  }

  return false;
}

SourceLoc Parser::consumeOptionalToken() {
  assert(isOptionalToken(Tok) && "not a '?' token?!");
  return consumeStartingCharacterOfCurrentToken(tok::question_postfix);
}

SourceLoc Parser::consumeImplicitlyUnwrappedOptionalToken() {
  assert(isImplicitlyUnwrappedOptionalToken(Tok) && "not a '!' token?!");
  // If the text of the token is just '!', grab the next token.
  return consumeStartingCharacterOfCurrentToken(tok::exclaim_postfix);
}

/// Parse a single optional suffix, given that we are looking at the
/// question mark.
ParserResult<TypeRepr>
Parser::parseTypeOptional(TypeRepr *base) {
  SourceLoc questionLoc = consumeOptionalToken();
  auto TyR = new (Context) OptionalTypeRepr(base, questionLoc);
  SyntaxContext->createNodeInPlace(SyntaxKind::OptionalType);
  return makeParserResult(TyR);
}

/// Parse a single implicitly unwrapped optional suffix, given that we
/// are looking at the exclamation mark.
ParserResult<TypeRepr>
Parser::parseTypeImplicitlyUnwrappedOptional(TypeRepr *base) {
  SourceLoc exclamationLoc = consumeImplicitlyUnwrappedOptionalToken();
  auto TyR =
      new (Context) ImplicitlyUnwrappedOptionalTypeRepr(base, exclamationLoc);
  SyntaxContext->createNodeInPlace(SyntaxKind::ImplicitlyUnwrappedOptionalType);
  return makeParserResult(TyR);
}

//===----------------------------------------------------------------------===//
// Speculative type list parsing
//===----------------------------------------------------------------------===//

static bool isGenericTypeDisambiguatingToken(Parser &P) {
  auto &tok = P.Tok;
  switch (tok.getKind()) {
  default:
    return false;
  case tok::r_paren:
  case tok::r_square:
  case tok::l_brace:
  case tok::r_brace:
  case tok::period:
  case tok::period_prefix:
  case tok::comma:
  case tok::semi:
  case tok::eof:
  case tok::code_complete:
  case tok::exclaim_postfix:
  case tok::question_postfix:
  case tok::colon:
    return true;

  case tok::oper_binary_spaced:
    if (tok.getText() == "&")
      return true;

    LLVM_FALLTHROUGH;
  case tok::oper_binary_unspaced:
  case tok::oper_postfix:
    // These might be '?' or '!' type modifiers.
    return P.isOptionalToken(tok) || P.isImplicitlyUnwrappedOptionalToken(tok);

  case tok::l_paren:
  case tok::l_square:
    // These only apply to the generic type if they don't start a new line.
    return !tok.isAtStartOfLine();
  }
}

bool Parser::canParseAsGenericArgumentList() {
  if (!Tok.isAnyOperator() || !Tok.getText().equals("<"))
    return false;

  BacktrackingScope backtrack(*this);

  if (canParseGenericArguments())
    return isGenericTypeDisambiguatingToken(*this);

  return false;
}

bool Parser::canParseGenericArguments() {
  // Parse the opening '<'.
  if (!startsWithLess(Tok))
    return false;
  consumeStartingLess();
  
  do {
    if (!canParseType())
      return false;
    // Parse the comma, if the list continues.
  } while (consumeIf(tok::comma));
  
  if (!startsWithGreater(Tok)) {
    return false;
  } else {
    consumeStartingGreater();
    return true;
  }
}

bool Parser::canParseType() {
  // Accept 'inout' at for better recovery.
  consumeIf(tok::kw_inout);

  if (Tok.isContextualKeyword("some"))
    consumeToken();

  switch (Tok.getKind()) {
  case tok::kw_Self:
  case tok::kw_Any:
      if (!canParseTypeIdentifier())
        return false;
      break;
  case tok::kw_protocol: // Deprecated composition syntax
  case tok::identifier:
    if (!canParseTypeIdentifierOrTypeComposition())
      return false;
    break;
  case tok::l_paren: {
    consumeToken();
    if (!canParseTypeTupleBody())
      return false;
    break;
  }
  case tok::at_sign: {
    consumeToken();
    if (!canParseTypeAttribute())
      return false;
    return canParseType();
  }
  case tok::l_square:
    consumeToken();
    if (!canParseType())
      return false;
    if (consumeIf(tok::colon)) {
      if (!canParseType())
        return false;
    }
    if (!consumeIf(tok::r_square))
      return false;
    break;


  default:
    return false;
  }

  // '.Type', '.Protocol', '?', and '!' still leave us with type-simple.
  while (true) {
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        (peekToken().isContextualKeyword("Type")
         || peekToken().isContextualKeyword("Protocol"))) {
      consumeToken();
      consumeToken(tok::identifier);
      continue;
    }
    if (isOptionalToken(Tok)) {
      consumeOptionalToken();
      continue;
    }
    if (isImplicitlyUnwrappedOptionalToken(Tok)) {
      consumeImplicitlyUnwrappedOptionalToken();
      continue;
    }
    break;
  }

  if (!isAtFunctionTypeArrow())
    return true;

  // Handle type-function if we have an '->' with optional
  // 'async' and/or 'throws'.
  while (isEffectsSpecifier(Tok))
    consumeToken();

  if (!consumeIf(tok::arrow))
    return false;
  
  return canParseType();
}

bool Parser::canParseTypeIdentifierOrTypeComposition() {
  if (Tok.is(tok::kw_protocol))
    return canParseOldStyleProtocolComposition();
  
  while (true) {
    if (!canParseTypeIdentifier())
      return false;
    
    if (Tok.isContextualPunctuator("&")) {
      consumeToken();
      continue;
    } else {
      return true;
    }
  }
}

bool Parser::canParseSimpleTypeIdentifier() {
  // Parse an identifier.
  if (!Tok.isAny(tok::identifier, tok::kw_Self, tok::kw_Any))
    return false;
  consumeToken();

  // Parse an optional generic argument list.
  if (startsWithLess(Tok) && !canParseGenericArguments())
    return false;

  return true;
}

bool Parser::canParseTypeIdentifier() {
  while (true) {
    if (!canParseSimpleTypeIdentifier())
      return false;

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'Type' or 'Protocol'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        !peekToken().isContextualKeyword("Type") &&
        !peekToken().isContextualKeyword("Protocol")) {
      consumeToken();
    } else {
      return true;
    }
  }
}

bool Parser::canParseBaseTypeForQualifiedDeclName() {
  BacktrackingScope backtrack(*this);

  // Parse a simple type identifier.
  if (!canParseSimpleTypeIdentifier())
    return false;

  // Qualified name base types must be followed by a period.
  // If the next token starts with a period, return true.
  return startsWithSymbol(Tok, '.');
}

bool Parser::canParseOldStyleProtocolComposition() {
  consumeToken(tok::kw_protocol);
  
  // Check for the starting '<'.
  if (!startsWithLess(Tok)) {
    return false;
  }
  consumeStartingLess();
  
  // Check for empty protocol composition.
  if (startsWithGreater(Tok)) {
    consumeStartingGreater();
    return true;
  }
  
  // Parse the type-composition-list.
  do {
    if (!canParseTypeIdentifier()) {
      return false;
    }
  } while (consumeIf(tok::comma));
  
  // Check for the terminating '>'.
  if (!startsWithGreater(Tok)) {
    return false;
  }
  consumeStartingGreater();
  
  return true;
}

bool Parser::canParseTypeTupleBody() {
  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace) &&
      Tok.isNotEllipsis() && !isStartOfSwiftDecl()) {
    do {
      // The contextual inout marker is part of argument lists.
      consumeIf(tok::kw_inout);

      // If the tuple element starts with "ident :", then it is followed
      // by a type annotation.
      if (Tok.canBeArgumentLabel() && 
          (peekToken().is(tok::colon) || peekToken().canBeArgumentLabel())) {
        consumeToken();
        if (Tok.canBeArgumentLabel()) {
          consumeToken();
          if (!Tok.is(tok::colon)) return false;
        }
        consumeToken(tok::colon);

        // Parse a type.
        if (!canParseType())
          return false;

        // Parse default values. This aren't actually allowed, but we recover
        // better if we skip over them.
        if (consumeIf(tok::equal)) {
          while (Tok.isNot(tok::eof) && Tok.isNot(tok::r_paren) &&
                 Tok.isNot(tok::r_brace) && Tok.isNotEllipsis() &&
                 Tok.isNot(tok::comma) && !isStartOfSwiftDecl()) {
            skipSingle();
          }
        }

        continue;
      }
      
      // Otherwise, this has to be a type.
      if (!canParseType())
        return false;

      if (Tok.isEllipsis())
        consumeToken();

    } while (consumeIf(tok::comma));
  }
  
  return consumeIf(tok::r_paren);
}

bool Parser::isAtFunctionTypeArrow() {
  if (Tok.is(tok::arrow))
    return true;

  if (isEffectsSpecifier(Tok)) {
    if (peekToken().is(tok::arrow))
      return true;
    if (isEffectsSpecifier(peekToken())) {
      BacktrackingScope backtrack(*this);
      consumeToken();
      consumeToken();
      return isAtFunctionTypeArrow();
    }
    // Don't look for '->' in code completion. The user may write it later.
    if (peekToken().is(tok::code_complete) && !peekToken().isAtStartOfLine())
      return true;

    return false;
  }

  // Don't look for '->' in code completion. The user may write it later.
  if (Tok.is(tok::code_complete) && !Tok.isAtStartOfLine())
    return true;

  return false;
}

//===--------------------------------------------------------------------===//
// MARK: - Type Parsing using libSyntax

ParsedSyntaxResult<ParsedTypeSyntax> Parser::applyAttributeToTypeSyntax(
    ParsedTypeSyntax &&Type, Optional<ParsedTokenSyntax> &&Specifier,
    Optional<ParsedAttributeListSyntax> &&Attrs) {
  if (!Attrs && !Specifier) {
    // Fast path. No attributes or specifiers to add.
    return makeParsedResult(std::move(Type));
  }

  auto ty = ParsedSyntaxRecorder::makeAttributedType(
      std::move(Specifier), std::move(Attrs), std::move(Type), *SyntaxContext);
  return makeParsedResult(std::move(ty));
}

ParsedTokenSyntax Parser::consumeImplicitlyUnwrappedOptionalTokenSyntax() {
  assert(isImplicitlyUnwrappedOptionalToken(Tok) && "not a '!' token?!");
  // If the text of the token is just '!', grab the next token.
  return consumeStartingCharacterOfCurrentTokenSyntax(tok::exclaim_postfix);
}

ParsedTokenSyntax Parser::consumeOptionalTokenSyntax() {
  assert(isOptionalToken(Tok) && "not a '?' token?!");
  return consumeStartingCharacterOfCurrentTokenSyntax(tok::question_postfix);
}

ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeAnySyntax() {
  auto anyToken = consumeTokenSyntax(tok::kw_Any);
  auto typeSyntax = ParsedSyntaxRecorder::makeSimpleTypeIdentifier(
      std::move(anyToken), None, *SyntaxContext);
  return makeParsedResult(std::move(typeSyntax));
}

/// Parse a collection type.
///   type-simple:
///     '[' type ']'
///     '[' type ':' type ']'
ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeCollectionSyntax() {
  assert(Tok.is(tok::l_square));
  Parser::StructureMarkerRAII parsingCollection(*this, Tok);

  ParserStatus status;

  // Parse the leading '['.
  SourceLoc lsquareLoc = Tok.getLoc();
  ParsedTokenSyntax lsquare = consumeTokenSyntax(tok::l_square);

  // Parse the element type.
  ParsedSyntaxResult<ParsedTypeSyntax> firstTypeResult =
      parseTypeSyntax(diag::expected_element_type);
  status |= firstTypeResult.getStatus();
  ParsedTypeSyntax &&firstType = firstTypeResult.take();

  // If there is a ':', this is a dictionary type.
  Optional<ParsedTokenSyntax> colon;
  Optional<ParsedTypeSyntax> secondType;
  if (Tok.is(tok::colon)) {
    colon = consumeTokenSyntax(tok::colon);

    // Parse the second type.
    ParsedSyntaxResult<ParsedTypeSyntax> secondTypeResult =
        parseTypeSyntax(diag::expected_dictionary_value_type);
    status |= secondTypeResult.getStatus();
    secondType = secondTypeResult.take();
  }

  // Parse the closing ']'.
  auto rsquare = parseMatchingTokenSyntax(
      tok::r_square,
      colon.hasValue() ? diag::expected_rbracket_dictionary_type
                       : diag::expected_rbracket_array_type,
      lsquareLoc);
  status |= rsquare.getStatus();

  if (colon) {
    ParsedDictionaryTypeSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftSquareBracket(std::move(lsquare));
    builder.useKeyType(std::move(firstType));
    builder.useColon(std::move(*colon));
    builder.useValueType(std::move(*secondType));
    builder.useRightSquareBracket(rsquare.take());
    return makeParsedResult(builder.build(), status);
  } else {
    ParsedArrayTypeSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftSquareBracket(std::move(lsquare));
    builder.useElementType(std::move(firstType));
    builder.useRightSquareBracket(rsquare.take());
    return makeParsedResult(builder.build(), status);
  }
}

ParsedSyntaxResult<ParsedGenericArgumentClauseSyntax>
Parser::parseTypeGenericArgumentClauseSyntax() {
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  auto lAngleLoc = Tok.getLoc();
  ParsedGenericArgumentClauseSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  // Parse '<'.
  builder.useLeftAngleBracket(consumeStartingLessSyntax());

  bool hasNext = true;
  while (hasNext) {
    // Parse argument type.
    auto ty = parseTypeSyntax(diag::expected_type);
    status |= ty.getStatus();
    ParsedGenericArgumentSyntaxBuilder argBuilder(*SyntaxContext);
    argBuilder.useArgumentType(ty.take());

    // Parse trailing comma: ','.
    if (Tok.is(tok::comma)) {
      argBuilder.useTrailingComma(consumeTokenSyntax());
    } else {
      hasNext = false;
    }
    builder.addArgumentsMember(argBuilder.build());
  }

  // Parse '>'.
  if (startsWithGreater(Tok)) {
    builder.useRightAngleBracket(consumeStartingGreaterSyntax());
  } else {
    if (status.isSuccess()) {
      diagnose(Tok, diag::expected_rangle_generic_arg_list);
      diagnose(lAngleLoc, diag::opening_angle);
    }
    checkForInputIncomplete();
    status.setIsParseError();
    if (ignoreUntilGreaterInTypeList(/*ProtocolComposition=*/false)) {
      builder.useRightAngleBracket(consumeStartingGreaterSyntax());
    }
  }

  return makeParsedResult(builder.build(), status);
}

/// parseTypeIdentifierSyntax
///
///   type-identifier:
///     identifier generic-args? ('.' identifier generic-args?)*
///
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeIdentifierSyntax(bool isParsingQualifiedDeclBaseType) {
  // If parsing a qualified declaration name, return an error if the base type
  // cannot be parsed.
  if (isParsingQualifiedDeclBaseType &&
      !canParseBaseTypeForQualifiedDeclName()) {
    auto unknownTy = ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);
    return makeParsedError(std::move(unknownTy));
  }

  if (Tok.is(tok::kw_Any)) {
    return parseTypeAnySyntax();
  } else if (Tok.is(tok::code_complete)) {
    auto ccTok = consumeTokenSyntax(tok::code_complete);
    auto ccType = ParsedSyntaxRecorder::makeCodeCompletionType(
        /*Base=*/None, /*Period=*/None, std::move(ccTok), *SyntaxContext);
    return makeParsedCodeCompletion(std::move(ccType));
  } else if (Tok.isNot(tok::identifier, tok::kw_Self)) {
    // Assume that the keyword was supposed to be a type and form an unknown
    // type with it.
    // If there is a keyword at the start of a new line, we don't want to
    // skip it as a recovery but rather keep it.
    if (Tok.isKeyword() && !Tok.isAtStartOfLine()) {
      auto kwTok = consumeTokenSyntax();
      ParsedTypeSyntax type =
          ParsedSyntaxRecorder::makeUnknownType({&kwTok, 1}, *SyntaxContext);
      return makeParsedError(std::move(type));
    }

    auto unknownTy = ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);
    return makeParsedError<ParsedTypeSyntax>(std::move(unknownTy));
  }

  /// Parse a type component and its generic args into \p Identifier and \p
  /// GenericArgs. Return the parser status after parsing the type component.
  auto parseComponent =
      [&](Optional<ParsedTokenSyntax> &Identifier,
          Optional<ParsedGenericArgumentClauseSyntax> &GenericArgs)
      -> ParserStatus {
    // Parse the type identifier
    // FIXME: specialize diagnostic for 'Type': type cannot start with
    // 'metatype'
    // FIXME: offer a fixit: 'self' -> 'Self'
    // TODO: (syntax-parse) According to the previous implementation, We should
    // be parsing a DeclNameRef here
    Identifier =
        parseIdentifierSyntax(diag::expected_identifier_in_dotted_type);

    if (!Identifier) {
      return makeParserError();
    }

    // Parse generic arguments if there are any
    if (startsWithLess(Tok)) {
      auto genericArgsResult = parseTypeGenericArgumentClauseSyntax();
      GenericArgs = genericArgsResult.take();
      return genericArgsResult.getStatus();
    }

    return makeParserSuccess();
  };

  // Parse the base identifier.
  Optional<ParsedTokenSyntax> baseIdentifier;
  Optional<ParsedGenericArgumentClauseSyntax> baseGenericArgs;
  auto status = parseComponent(baseIdentifier, baseGenericArgs);
  // If this isn't an identifier we should have bailed out earlier
  assert(baseIdentifier);
  ParsedTypeSyntax typeSyntax = ParsedSyntaxRecorder::makeSimpleTypeIdentifier(
      std::move(*baseIdentifier), std::move(baseGenericArgs), *SyntaxContext);

  // Parse member identifiers.
  while (status.isSuccess() && Tok.isAny(tok::period, tok::period_prefix)) {
    if (peekToken().isContextualKeyword("Type") ||
        peekToken().isContextualKeyword("Protocol")) {
      break;
    }
    // If parsing a qualified declaration name, break before parsing the
    // period before the final declaration name component.
    if (isParsingQualifiedDeclBaseType) {
      // If qualified name base type cannot be parsed from the current
      // point (i.e. the next type identifier is not followed by a '.'),
      // then the next identifier is the final declaration name component.
      BacktrackingScope backtrack(*this);
      consumeStartingCharacterOfCurrentToken(tok::period);
      if (!canParseBaseTypeForQualifiedDeclName()) {
        break;
      }
    }

    // Parse '.'.
    auto period = consumeTokenSyntax();

    // Parse component;
    Optional<ParsedTokenSyntax> identifier;
    Optional<ParsedGenericArgumentClauseSyntax> genericArgs;
    auto status = parseComponent(identifier, genericArgs);
    if (identifier) {
      ParsedMemberTypeIdentifierSyntaxBuilder builder(*SyntaxContext);
      builder.useBaseType(std::move(typeSyntax));
      builder.usePeriod(std::move(period));
      builder.useName(std::move(*identifier));
      builder.useGenericArgumentClause(std::move(genericArgs));
      typeSyntax = builder.build();
      continue;
    }

    // If there was no identifer there shouldn't be genericArgs.
    assert(!genericArgs);

    if (Tok.is(tok::code_complete)) {
      auto ccTok = consumeTokenSyntax(tok::code_complete);
      auto ty = ParsedSyntaxRecorder::makeCodeCompletionType(
          std::move(typeSyntax), std::move(period), std::move(ccTok),
          *SyntaxContext);
      return makeParsedCodeCompletion(std::move(ty));
    }

    ParsedSyntax parts[] = {std::move(typeSyntax), std::move(period)};
    status.setIsParseError();
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownType({parts, 2}, *SyntaxContext),
        status);
  }

  if (status.isSuccess() && Tok.is(tok::code_complete) &&
      !Tok.isAtStartOfLine()) {
    auto ccTok = consumeTokenSyntax(tok::code_complete);
    auto ty = ParsedSyntaxRecorder::makeCodeCompletionType(
        std::move(typeSyntax), /*Period=*/None, std::move(ccTok),
        *SyntaxContext);
    return makeParsedCodeCompletion(std::move(ty));
  }

  return makeParsedResult(std::move(typeSyntax), status);
}

/// parseOldStyleProtocolCompositionSyntax
///   type-composition-deprecated:
///     'protocol' '<' '>'
///     'protocol' '<' type-composition-list-deprecated '>'
///
///   type-composition-list-deprecated:
///     type-identifier
///     type-composition-list-deprecated ',' type-identifier
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeOldStyleProtocolCompositionSyntax() {
  SmallVector<ParsedSyntax, 6> junk;

  junk.emplace_back(consumeTokenSyntax(tok::kw_protocol));
  auto lAngleLoc = Tok.getLoc();
  junk.emplace_back(consumeStartingLessSyntax());

  // Parse the type-composition-list.
  ParserStatus status;
  bool isEmpty = startsWithGreater(Tok);

  if (!isEmpty) {
    while (true) {
      auto typeResult = parseTypeIdentifierSyntax();
      status |= typeResult.getStatus();
      junk.emplace_back(typeResult.take());
      Optional<ParsedTokenSyntax> comma = consumeTokenSyntaxIf(tok::comma);
      if (comma) {
        junk.emplace_back(std::move(*comma));
      } else {
        break;
      }
    };
  }

  // Check for the terminating '>'.
  Optional<SourceLoc> rAngleLoc;
  if (startsWithGreater(Tok)) {
    rAngleLoc = Tok.getLoc();
    junk.emplace_back(consumeStartingGreaterSyntax());
  } else {
    // We did not find the terminating '>'.
    diagnose(Tok, diag::expected_rangle_protocol);
    diagnose(lAngleLoc, diag::opening_angle);
    status.setIsParseError();

    // Skip until we hit the '>'.
    ignoreUntilGreaterInTypeList(/*ProtocolComposition=*/true,
                                 /*Collect=*/&junk);
  }

  auto unknown = ParsedSyntaxRecorder::makeUnknownType(junk, *SyntaxContext);
  return makeParsedError(std::move(unknown));
}

/// parseTypeOldStyleArraySyntax - Parse the old style type-array production,
/// given that we are looking at the initial l_square.  Note that this index
/// clause is actually the outermost (first-indexed) clause.
///
///   type-array:
///     type-simple
///     type-array '[' ']'
///     type-array '[' int_literal ']'
///
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeOldStyleArraySyntax(ParsedTypeSyntax BaseSyntax,
                                     SourceLoc BaseLoc) {
  assert(Tok.isFollowingLSquare());
  Parser::StructureMarkerRAII ParsingArrayBound(*this, Tok);
  SourceLoc lsquareLoc = Tok.getLoc();
  ignoreToken(tok::l_square);

  // Ignore integer literal between '[' and ']'
  ignoreIf(tok::integer_literal);

  ParserStatus status;

  auto rSquareLoc = Tok.getLoc();
  auto rSquare = parseMatchingTokenSyntax(
      tok::r_square, diag::expected_rbracket_array_type, lsquareLoc);
  status |= rSquare.getStatus();

  if (status.isSuccess()) {
    // If we parsed a valid old style array (baseType '[' ']'), diagnose it
    // with a fixit to rewrite it to new Swift syntax.
    diagnose(lsquareLoc, diag::new_array_syntax)
        .fixItInsert(BaseLoc, "[")
        .fixItRemoveChars(lsquareLoc, rSquareLoc);
  }

  ParsedArrayTypeSyntaxBuilder builder(*SyntaxContext);
  builder.useElementType(std::move(BaseSyntax));
  builder.useRightSquareBracket(rSquare.take());
  return makeParsedResult(builder.build(), status);
}

/// parseTypeSimpleSyntax
///   type-simple:
///     type-identifier
///     type-tuple
///     type-composition-deprecated
///     'Any'
///     type-simple '.Type'
///     type-simple '.Protocol'
///     type-simple '?'
///     type-simple '!'
///     type-collection
///     type-array
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeSimpleSyntax(Diag<> MessageID) {
  if (Tok.is(tok::kw_inout) ||
      (Tok.is(tok::identifier) && (Tok.getRawText().equals("__shared") ||
                                   Tok.getRawText().equals("__owned")))) {
    // Type specifier should already be parsed before here. This only happens
    // for construct like 'P1 & inout P2'.
    diagnose(Tok.getLoc(), diag::attr_only_on_parameters, Tok.getRawText());
    ignoreToken();
  }

  SourceLoc typeLoc = leadingTriviaLoc();

  ParserStatus status;
  Optional<ParsedTypeSyntax> typeOptional; // stores the parsed type
  switch (Tok.getKind()) {
  case tok::kw_Self:
  case tok::kw_Any:
  case tok::identifier: {
    auto result = parseTypeIdentifierSyntax();
    status |= result.getStatus();
    typeOptional = result.take();
    break;
  }
  case tok::l_paren: {
    auto result = parseTypeTupleBodySyntax();
    status |= result.getStatus();
    typeOptional = result.take();
    break;
  }
  case tok::code_complete: {
    auto ccToken = consumeTokenSyntax(tok::code_complete);
    auto ccType = ParsedSyntaxRecorder::makeCodeCompletionType(
        None, None, std::move(ccToken), *SyntaxContext);
    return makeParsedCodeCompletion(std::move(ccType));
  }
  case tok::l_square: {
    auto result = parseTypeCollectionSyntax();
    status |= result.getStatus();
    typeOptional = result.take();
    break;
  }
  case tok::kw_protocol:
    if (startsWithLess(peekToken())) {
      auto result = parseTypeOldStyleProtocolCompositionSyntax();
      status |= result.getStatus();
      typeOptional = result.take();
      break;
    }
    LLVM_FALLTHROUGH;
  default: {
    if (Tok.isKeyword() && !Tok.isAtStartOfLine()) {
      auto token = consumeTokenSyntax();
      ParsedTypeSyntax ty =
          ParsedSyntaxRecorder::makeUnknownType({&token, 1}, *SyntaxContext);
      status.setIsParseError();
      return makeParsedResult(std::move(ty), status);
    }
    checkForInputIncomplete();
    ParsedTypeSyntax ty =
        ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);
    status.setIsParseError();
    return makeParsedResult<ParsedTypeSyntax>(std::move(ty), status);
  }
  }
  ParsedTypeSyntax &&type = std::move(*typeOptional);

  // '.Type', '.Protocol', '?', '!', and '[]' still leave us with type-simple.
  while (status.isSuccess()) {
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        (peekToken().isContextualKeyword("Type") ||
         peekToken().isContextualKeyword("Protocol"))) {
      auto period = consumeTokenSyntax();
      auto keyword = consumeTokenSyntax(tok::identifier);
      type = ParsedSyntaxRecorder::makeMetatypeType(
          std::move(type), std::move(period), std::move(keyword),
          *SyntaxContext);
    } else if (isOptionalToken(Tok) && !Tok.isAtStartOfLine()) {
      auto questionMark = consumeOptionalTokenSyntax();
      type = ParsedSyntaxRecorder::makeOptionalType(
          std::move(type), std::move(questionMark), *SyntaxContext);
    } else if (isImplicitlyUnwrappedOptionalToken(Tok) &&
               !Tok.isAtStartOfLine()) {
      auto exclamationMark = consumeImplicitlyUnwrappedOptionalTokenSyntax();
      type = ParsedSyntaxRecorder::makeImplicitlyUnwrappedOptionalType(
          std::move(type), std::move(exclamationMark), *SyntaxContext);
    } else if (Tok.is(tok::l_square) && !Tok.isAtStartOfLine()) {
      auto result = parseTypeOldStyleArraySyntax(std::move(type), typeLoc);
      status |= result.getStatus();
      type = result.take();
    } else {
      break;
    }
  }

  return makeParsedResult(std::move(type), status);
}

/// parseTypeSimpleOrCompositionSyntax
///
///   type-composition:
///     'some'? type-simple
///     type-composition '&' type-simple
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeSimpleOrCompositionSyntax(Diag<> MessageID) {
  // Check for the 'some' modifier.
  Optional<ParsedTokenSyntax> someSpecifier;
  if (Tok.isContextualKeyword("some")) {
    TokReceiver->registerTokenKindChange(Tok.getLoc(), tok::contextual_keyword);
    someSpecifier = consumeTokenSyntax();
  }

  ParserStatus status;

  // Parse the first type specifically so we can return a simple type if there
  // is no composition
  auto firstTypeResult = parseTypeSimpleSyntax(MessageID);
  status |= firstTypeResult.getStatus();

  if (!Tok.isContextualPunctuator("&")) {
    // We don't have a type composition
    if (someSpecifier) {
      auto someType = ParsedSyntaxRecorder::makeSomeType(
          std::move(*someSpecifier), firstTypeResult.take(), *SyntaxContext);
      return makeParsedResult(std::move(someType));
    } else {
      return firstTypeResult;
    }
  }

  // We have a type composition
  assert(Tok.isContextualPunctuator("&"));

  SmallVector<ParsedCompositionTypeElementSyntax, 4> elements;
  // Add the first type to elements
  {
    auto ampersand = consumeTokenSyntax();
    auto firstCompositionElement =
        ParsedSyntaxRecorder::makeCompositionTypeElement(
            firstTypeResult.take(), std::move(ampersand), *SyntaxContext);
    elements.push_back(std::move(firstCompositionElement));
  }

  // Parse for more elements in the composition
  while (true) {
    Optional<ParsedTokenSyntax> nextSome;
    // Diagnose invalid `some` after an ampersand.
    if (Tok.isContextualKeyword("some")) {
      nextSome = consumeTokenSyntax();
    }

    ParsedSyntaxResult<ParsedTypeSyntax> nextTypeResult =
        parseTypeSimpleSyntax(diag::expected_identifier_for_type);
    status |= nextTypeResult.getStatus();

    // Otherwise we can form another element for our type composition
    ParsedTypeSyntax &&nextType = nextTypeResult.take();
    if (nextSome) {
      nextType = ParsedSyntaxRecorder::makeSomeType(
          std::move(*nextSome), std::move(nextType), *SyntaxContext);
    }

    Optional<ParsedTokenSyntax> ampersand = None;
    bool hasAmpersand = false;
    if (Tok.isContextualPunctuator("&")) {
      ampersand = consumeTokenSyntax();
      hasAmpersand = true;
    }
    auto element = ParsedSyntaxRecorder::makeCompositionTypeElement(
        std::move(nextType), std::move(ampersand), *SyntaxContext);
    elements.push_back(std::move(element));
    if (!hasAmpersand) {
      break;
    }
  }

  // Build the final composition
  auto elementsList = ParsedSyntaxRecorder::makeCompositionTypeElementList(
      elements, *SyntaxContext);
  ParsedTypeSyntax resultSyntax = ParsedSyntaxRecorder::makeCompositionType(
      std::move(elementsList), *SyntaxContext);

  // If we saw a 'some' specifier in the beginning, apply it to the type.
  if (someSpecifier) {
    resultSyntax = ParsedSyntaxRecorder::makeSomeType(
        std::move(*someSpecifier), std::move(resultSyntax), *SyntaxContext);
  }

  return makeParsedResult(std::move(resultSyntax), status);
}

ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeSyntax(Diag<> MessageID, bool IsSILFuncDecl) {
  SourceLoc TypeLoc = leadingTriviaLoc();

  // Set up a SyntaxParsingContext that captures the libSyntax nodes generated
  // by the legacy AST parser.
  SyntaxParsingContext TypeParsingContext(SyntaxContext,
                                          SyntaxContextKind::Type);
  TypeParsingContext.setTransparent();
  ParserResult<TypeRepr> Result = parseType(MessageID, IsSILFuncDecl);

  // If parsing succeeded, we have a ParsedTypeSyntax in the TypeParsingContext.
  // Pop it and return it. The caller of this method will add it to its
  // SyntaxParsingContext manually.
  if (auto ParsedType = TypeParsingContext.popIf<ParsedTypeSyntax>()) {
    ASTGenerator.addType(Result.getPtrOrNull(), TypeLoc);
    return makeParsedResult(std::move(*ParsedType), Result.getStatus());
  } else {
    // Add an error type to ASTGen to tell it that this unknown type
    // has already been diagnosed by the parser.
    ASTGenerator.addType(new (Context) ErrorTypeRepr(TypeLoc), TypeLoc);
  }
  // Otherwise parsing failed. Return the parser status.
  auto unknownType = ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);
  return makeParsedError(std::move(unknownType));
}
/// parseTypeTupleBodySyntax
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier? identifier ':' type
///     type
ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeTupleBodySyntax() {
  Parser::StructureMarkerRAII ParsingTypeTuple(*this, Tok);
  // Force the context to create deferred nodes, as we might need to
  // de-structure the tuple type to create a function type.
  DeferringContextRAII Deferring(*SyntaxContext);

  if (ParsingTypeTuple.isFailed()) {
    auto unknownTy = ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);
    return makeParsedError(std::move(unknownTy));
  }

  ParsedTupleTypeSyntaxBuilder builder(*SyntaxContext);

  // Parse '('.
  auto lParenLoc = Tok.getLoc();
  builder.useLeftParen(consumeTokenSyntax(tok::l_paren));

  // Parse the elements.
  SmallVector<ParsedTupleTypeElementSyntax, 4> elements;
  // For each element contains nameLoc, secondNameLoc, typeLoc
  SmallVector<std::tuple<SourceLoc, SourceLoc, SourceLoc>, 4> elementsLoc;
  auto status = parseListSyntax(
      elements, /*AllowEmpty=*/true, /*AllowSepAfterLast=*/false,
      [&] { return Tok.is(tok::r_paren); },
      [&](ParsedTupleTypeElementSyntaxBuilder &ElemBuilder) {
        Optional<BacktrackingScope> backtracking;

        // 'inout' here can be be either obsoleted use of the marker in front of
        // an argument label or a valid type annotation in case there are no
        // argument labels. Until we are convinced of the opposite, we must
        // assume the former.
        SourceLoc inOutLoc;
        Optional<ParsedTokenSyntax> inOut;
        bool hasArgumentLabel = false;
        if (Tok.is(tok::kw_inout)) {
          inOutLoc = Tok.getLoc();
          inOut = consumeTokenSyntax(tok::kw_inout);
        }

        // If the label is "some", this could end up being an opaque type
        // description if there's `some <identifier>` without a following colon,
        // so we may need to be able to backtrack.
        if (Tok.getText().equals("some")) {
          backtracking.emplace(*this);
        }

        // If the tuple element starts with a potential argument label followed
        // by a ':' or another potential argument label, then the identifier is
        // an element tag, and it is followed by a type annotation.
        Optional<ParsedTokenSyntax> name;
        Optional<ParsedTokenSyntax> secondName;
        Optional<ParsedTokenSyntax> colon;
        SourceLoc nameLoc;
        SourceLoc secondNameLoc;
        // Argument labels can be either
        // name ':' or
        // name secondName ':'
        // Check if we are in either of these cases
        if (Tok.canBeArgumentLabel() &&
            (peekToken().is(tok::colon) || peekToken().canBeArgumentLabel())) {
          hasArgumentLabel = true;
          // Consume a name.
          nameLoc = Tok.getLoc();
          name = consumeArgumentLabelSyntax();

          // If there is a second name, consume it as well.
          if (Tok.canBeArgumentLabel()) {
            secondNameLoc = Tok.getLoc();
            secondName = consumeArgumentLabelSyntax();
          }

          // Consume the ':'.
          colon = consumeTokenSyntaxIf(tok::colon);
          if (colon) {
            // If we succeed, then we successfully parsed a label and there's
            // no need to backtrack.
            if (backtracking) {
              backtracking->cancelBacktrack();
            }
          }
        }

        if (!backtracking || !backtracking->willBacktrack()) {
          // If we are not backtracking, record the parsed tokens.
          ElemBuilder.useName(std::move(name));
          ElemBuilder.useSecondName(std::move(secondName));
          ElemBuilder.useColon(std::move(colon));
          if ((name || secondName) && !colon) {
            diagnose(Tok, diag::expected_parameter_colon);
          }
        } else {
          // We are backtracking. Discard any names that we have parsed.
          nameLoc = SourceLoc();
          secondNameLoc = SourceLoc();
          name.reset();
          secondName.reset();
          assert(!colon.hasValue());
        }
        backtracking.reset();

        // Parse the type.
        ParserStatus status;
        auto typeLoc = Tok.getLoc();
        auto typeResult = parseTypeSyntax(diag::expected_type);
        status |= typeResult.getStatus();
        ParsedTypeSyntax &&ty = typeResult.take();

        // Handle pre-parsed 'inout'.
        if (inOut) {
          if (hasArgumentLabel) {
            ElemBuilder.useInOut(std::move(*inOut));
          } else {
            // Apply 'inout' to the parsed type.
            auto attrTyRes = applyAttributeToTypeSyntax(std::move(ty),
                                                        std::move(inOut), None);
            status |= attrTyRes.getStatus();
            ty = attrTyRes.take();
            typeLoc = inOutLoc;
          }
        }
        ElemBuilder.useType(std::move(ty));
        elementsLoc.emplace_back(nameLoc, secondNameLoc, typeLoc);

        // Parse '...'.
        if (Tok.isEllipsis()) {
          Tok.setKind(tok::ellipsis);
          ElemBuilder.useEllipsis(consumeTokenSyntax(tok::ellipsis));
        }

        // If we find an initializer ('=' expr), this is not valid inside a type
        // tuple. Parse it into the libSyntax tree for better diagnostics. It
        // will be ignored in terms of the AST.
        if (Tok.is(tok::equal)) {
          ParsedInitializerClauseSyntaxBuilder initBuilder(*SyntaxContext);
          initBuilder.useEqual(consumeTokenSyntax(tok::equal));

          // The context into which the expression will be parsed until
          // expression parsing has been migrated to libSyntax. We don't need to
          // worry about feeding the expression to ASTGen since it is ignored by
          // the AST.
          SyntaxParsingContext tmpCtxt(SyntaxContext);
          tmpCtxt.setTransparent();

          (void)parseExpr(diag::expected_init_value);
          if (auto expr = SyntaxContext->popIf<ParsedExprSyntax>()) {
            initBuilder.useValue(std::move(*expr));
          } else {
            initBuilder.useValue(
                ParsedSyntaxRecorder::makeUnknownExpr({}, *SyntaxContext));
          }
          ElemBuilder.useInitializer(initBuilder.build());
          status.setIsParseError();
        }

        return status;
      });

  // Parse ')'.
  auto rParen = parseMatchingTokenSyntax(
      tok::r_paren, diag::expected_rparen_tuple_type_list, lParenLoc,
      /*SilenceDiag=*/status.isError());
  status |= rParen.getStatus();

  builder.useElements(
      ParsedSyntaxRecorder::makeTupleTypeElementList(elements, *SyntaxContext));
  builder.useRightParen(rParen.take());

  return makeParsedResult(builder.build(), status);
}
