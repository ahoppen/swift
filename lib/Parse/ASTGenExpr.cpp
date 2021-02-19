//===--- ASTGenExpr.cpp ---------------------------------------------------===//
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

#include "swift/Parse/ASTGen.h"

using namespace swift;
using namespace swift::syntax;

//===--------------------------------------------------------------------===//
// MARK: - Public entry function

Expr *ASTGen::generate(const ExprSyntax &Expr, SourceLoc TreeStartLoc) {
  this->TreeStartLoc = TreeStartLoc;

  return generate(Expr);
}

//===--------------------------------------------------------------------===//
// MARK: - Generate functions

Expr *ASTGen::generate(const ExprSyntax &Expr) {
  if (auto boolLitera = Expr.getAs<BooleanLiteralExprSyntax>()) {
    return generate(*boolLitera);
  } else if (auto floatLiteral = Expr.getAs<FloatLiteralExprSyntax>()) {
    return generate(*floatLiteral);
  } else if (auto intLiteral = Expr.getAs<IntegerLiteralExprSyntax>()) {
    return generate(*intLiteral);
  } else if (auto nilLiteral = Expr.getAs<NilLiteralExprSyntax>()) {
    return generate(*nilLiteral);
  } else if (auto poundColumn = Expr.getAs<PoundColumnExprSyntax>()) {
    return generate(*poundColumn);
  } else if (auto poundDso = Expr.getAs<PoundDsohandleExprSyntax>()) {
    return generate(*poundDso);
  } else if (auto poundFile = Expr.getAs<PoundFileExprSyntax>()) {
    return generate(*poundFile);
  } else if (auto poundFileId = Expr.getAs<PoundFileIDExprSyntax>()) {
    return generate(*poundFileId);
  } else if (auto poundFilePath = Expr.getAs<PoundFilePathExprSyntax>()) {
    return generate(*poundFilePath);
  } else if (auto poundLine = Expr.getAs<PoundLineExprSyntax>()) {
    return generate(*poundLine);
  } else if (auto poundFunction = Expr.getAs<PoundFunctionExprSyntax>()) {
    return generate(*poundFunction);
  } else if (auto unknownExpr = Expr.getAs<UnknownExprSyntax>()) {
    return generate(*unknownExpr);
  } else {
    llvm_unreachable("ASTGen hasn't been tought how to generate this expr");
  }
}

Expr *ASTGen::generate(const BooleanLiteralExprSyntax &Expr) {
  TokenSyntax Literal = Expr.getBooleanLiteral();
  assert(Literal.getTokenKind() == tok::kw_true ||
         Literal.getTokenKind() == tok::kw_false);
  bool Value = Literal.getTokenKind() == tok::kw_true;
  return new (*Context) BooleanLiteralExpr(Value, getLoc(Literal));
}

Expr *ASTGen::generate(const FloatLiteralExprSyntax &Expr) {
  TokenSyntax Digits = Expr.getFloatingDigits();
  StringRef Text = copyAndStripUnderscores(Digits.getText());
  return new (*Context) FloatLiteralExpr(Text, getLoc(Digits));
}

Expr *ASTGen::generate(const IntegerLiteralExprSyntax &Expr) {
  TokenSyntax Digits = Expr.getDigits();
  StringRef Text = copyAndStripUnderscores(Digits.getText());
  return new (*Context) IntegerLiteralExpr(Text, getLoc(Digits));
}

Expr *ASTGen::generate(const NilLiteralExprSyntax &Expr) {
  TokenSyntax Nil = Expr.getNilKeyword();
  return new (*Context) NilLiteralExpr(getLoc(Nil));
}

Expr *ASTGen::generate(const PoundColumnExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundColumn());
}

Expr *ASTGen::generate(const PoundDsohandleExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundDsohandle());
}

Expr *ASTGen::generate(const PoundFileExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFile());
}

Expr *ASTGen::generate(const PoundFileIDExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFileID());
}

Expr *ASTGen::generate(const PoundFilePathExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFilePath());
}

Expr *ASTGen::generate(const PoundFunctionExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFunction());
}

Expr *ASTGen::generate(const PoundLineExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundLine());
}

Expr *ASTGen::generate(const UnknownExprSyntax &Expr) {
  if (Expr.getNumChildren() == 1 && Expr.getChild(0)->isToken()) {
    TokenSyntax Token = *Expr.getChild(0)->getAs<TokenSyntax>();
    tok Kind = Token.getRaw()->getTokenKind();
    switch (Kind) {
    case tok::kw___FILE__:
    case tok::kw___LINE__:
    case tok::kw___COLUMN__:
    case tok::kw___FUNCTION__:
    case tok::kw___DSO_HANDLE__: {
      auto MagicKind = getMagicIdentifierLiteralKind(Kind);
      return new (*Context) MagicIdentifierLiteralExpr(MagicKind, getLoc(Token));
    }
    default:
      return nullptr;
    }
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
// MARK: - Private helpers

Expr *ASTGen::generateMagicIdentifierLiteralExpr(const TokenSyntax &Token) {
  auto Kind = getMagicIdentifierLiteralKind(Token.getTokenKind());
  return new (*Context) MagicIdentifierLiteralExpr(Kind, getLoc(Token));
}

/// Map magic literal tokens such as #file to their
/// MagicIdentifierLiteralExpr kind.
MagicIdentifierLiteralExpr::Kind
ASTGen::getMagicIdentifierLiteralKind(tok Kind) {
  switch (Kind) {
  case tok::pound_file:
    // TODO: Enable by default at the next source break. (SR-13199)
    return Context->LangOpts.EnableConcisePoundFile
               ? MagicIdentifierLiteralExpr::FileIDSpelledAsFile
               : MagicIdentifierLiteralExpr::FilePathSpelledAsFile;
#define MAGIC_IDENTIFIER_TOKEN(NAME, TOKEN)                                    \
  case tok::TOKEN:                                                             \
    return MagicIdentifierLiteralExpr::Kind::NAME;
#include "swift/AST/MagicIdentifierKinds.def"
  default:
    llvm_unreachable("not a magic literal");
  }
}
