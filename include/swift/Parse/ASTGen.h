//===--- ASTGen.h ---------------------------------------------------------===//
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

#ifndef SWIFT_PARSE_ASTGEN_H
#define SWIFT_PARSE_ASTGEN_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Expr.h"
#include "swift/Parse/Lexer.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

using namespace swift::syntax;

class CodeCompletionCallbacks;
class ComponentIdentTypeRepr;
class TupleTypeRepr;

/// Generates AST nodes from Syntax nodes.
class ASTGen {
  friend class GeneratingFunctionTypeRAII;

  ASTContext &Context;

  SourceManager &SourceMgr;

  CodeCompletionCallbacks *CodeCompletion;

  DiagnosticEngine &Diags;

  // TODO: (syntax-parse) We should define our own type at some point an not
  // rely on the lexer's mode enum.
  LexerMode Mode;

  // TODO: (syntax-parse) Remove when parsing of all types has been migrated to
  // libSyntax.
  /// When parsing a type that cannot be represented by libSyntax or generated
  /// by ASTGen yet, but is nested inside a node that is already parsed into
  /// libSyntax and subsequently ASTGen'ed, it will be added to this map.
  /// When requested to ASTGen this type, we can look it up using \c hasType
  /// and directly return it.
  /// Types are added to this map by the \c SourceLoc of their *leading trivia*.
  llvm::DenseMap<SourceLoc, TypeRepr *> Types;

  // TODO: (syntax-parse) Once migration is finished we should be able to
  // populate this from ASTGen's initializer.
  /// The SourceLoc at which the syntax tree that is currently being ASTGen'ed
  /// starts. Used to compute source locations of syntax nodes by adding their
  /// offset in the tree to this location.
  SourceLoc TreeStartLoc;

  // TODO: (syntax-parse): Once migration is finished, we can always compute
  // this by accessing the node's parent. Remove then.
  /// The location of the token preceeding the syntax tree currently being
  /// generated
  SourceLoc PreviousTokLoc;

public:
  ASTGen(ASTContext &Context, SourceManager &SourceMgr,
         CodeCompletionCallbacks *CodeCompletion, DiagnosticEngine &Diags,
         LexerMode Mode)
      : Context(Context), SourceMgr(SourceMgr), CodeCompletion(CodeCompletion),
        Diags(Diags), Mode(Mode) {}

  //===--------------------------------------------------------------------===//
  // MARK: - Expressions
public:
  /// The public entry function to generate an AST expression from the \p Expr
  /// libSyntax node.
  ///
  /// \p TreeStartLoc must point to the leading trivia of the first token in the
  /// tree in which \p Type resides. I.e. if \p Type resides in a tree
  /// representing the entire source file, \p TreeStartLoc must point to the
  /// first trivia in the file.
  Expr *generate(const ExprSyntax &Expr, SourceLoc TreeStartLoc);

private:
  Expr *generate(const ExprSyntax &Expr);

  Expr *generate(const BooleanLiteralExprSyntax &Expr);
  Expr *generate(const FloatLiteralExprSyntax &Expr);
  Expr *generate(const IntegerLiteralExprSyntax &Expr);
  Expr *generate(const NilLiteralExprSyntax &Expr);
  Expr *generate(const PoundColumnExprSyntax &Expr);
  Expr *generate(const PoundDsohandleExprSyntax &Expr);
  Expr *generate(const PoundFileExprSyntax &Expr);
  Expr *generate(const PoundFileIDExprSyntax &Expr);
  Expr *generate(const PoundFilePathExprSyntax &Expr);
  Expr *generate(const PoundLineExprSyntax &Expr);
  Expr *generate(const PoundFunctionExprSyntax &Expr);
  Expr *generate(const UnknownExprSyntax &Expr);

private:
  /// Map magic literal tokens such as #file to their MagicIdentifierLiteralExpr
  /// kind.
  MagicIdentifierLiteralExpr::Kind getMagicIdentifierLiteralKind(tok Kind);

  Expr *generateMagicIdentifierLiteralExpr(const TokenSyntax &Token);

  //===--------------------------------------------------------------------===//
  // MARK: - Types
public:
  /// The public entry function to generate an AST type from the \p Type
  /// libSyntax node.
  ///
  /// \p TreeStartLoc must point to the leading trivia of the first token in the
  /// tree in which \p Type resides. I.e. if \p Type resides in a tree
  /// representing the entire source file, \p TreeStartLoc must point to the
  /// first trivia in the file.
  /// If no type was provided \p MissingTypeDiag is emitted.
  TypeRepr *generate(const TypeSyntaxRef &Type, SourceLoc TreeStartLoc,
                     SourceLoc PreviousTokLoc,
                     Diag<> MissingTypeDiag = diag::expected_type);

private:
  TypeRepr *generate(const TypeSyntaxRef &Type,
                     Diag<> MissingTypeDiag = diag::expected_type);

  TypeRepr *generate(const ArrayTypeSyntaxRef &Type);
  TypeRepr *generate(const CodeCompletionTypeSyntaxRef &Type);
  TypeRepr *generate(const DictionaryTypeSyntaxRef &Type);
  TypeRepr *generate(const MemberTypeIdentifierSyntaxRef &Type);
  TypeRepr *generate(const SimpleTypeIdentifierSyntaxRef &Type);
  TypeRepr *generate(const UnknownTypeSyntaxRef &Type, Diag<> MissingTypeDiag);
public:
  /// Add a \c TypeRepr occurring at \p Loc whose parsing hasn't been migrated
  /// to libSyntaxParsing yet. It can later be retrieved from \c ASTGen using
  /// \c hasType and \c takeType.
  void addType(TypeRepr *Type, const SourceLoc &Loc);

  /// Check if a \c TypeRepr, whose parsing hasn't been migrated to libSyntax
  /// yet, has been added to \c Types at the given \p Loc.
  bool hasType(const SourceLoc &Loc) const;

  /// Given there is a \c TypeRepr, whose parsing hasn't been migrated to
  /// libSyntax yet, at the given \c Loc.
  TypeRepr *takeType(const SourceLoc &Loc);

  /// Generate the \c TypeReprs specified in the \c clauseSyntax.
  /// Returns a pair containing
  /// 1. The source range from the left angle bracket to the right-angle bracket
  /// 2. The generic arguments
  std::pair<SourceRange, SmallVector<TypeRepr *, 4>>
  generateGenericArgs(const GenericArgumentClauseSyntaxRef &ClauseSyntax);

  /// Generate a \c ComponentIdentTypeRepr from a \c SimpleTypeIdentifierSyntax
  /// or \c MemberTypeIdentifierSyntax. If \c TypeSyntax is a \c
  /// MemberTypeIdentifierSyntax this will *not* walk its children. Use \c
  /// gatherTypeIdentifierComponents to gather all components.
  template <typename T>
  ComponentIdentTypeRepr *generateTypeIdentifier(const T &TypeSyntax);

  /// Recursively walk the \c Component type syntax and gather all type
  /// components as \c TypeReprs in \c Components.
  void gatherTypeIdentifierComponents(
      const TypeSyntaxRef &Component,
      llvm::SmallVectorImpl<ComponentIdentTypeRepr *> &Components);

  //===--------------------------------------------------------------------===//
  // MARK: - Miscellaneous
public:
  /// Copy a numeric literal value into AST-owned memory, stripping underscores
  /// so the semantic part of the value can be parsed by APInt/APFloat parsers.
  static StringRef copyAndStripUnderscores(StringRef Orig, ASTContext &Context);

  bool isInSILMode() { return Mode == LexerMode::SIL; }

  void setCodeCompletionCallbacks(CodeCompletionCallbacks *Callbacks) {
    CodeCompletion = Callbacks;
  }

private:
  StringRef copyAndStripUnderscores(StringRef Orig);

  //===--------------------------------------------------------------------===//
  // MARK: - Diagnostics
private:
  InFlightDiagnostic diagnose(SourceLoc Loc, Diagnostic Diag) {
    return Diags.diagnose(Loc, Diag);
  }

  InFlightDiagnostic diagnose(const SyntaxRef &Node, Diagnostic Diag) {
    return diagnose(getContentStartLoc(Node), Diag);
  }

  template <typename... DiagArgTypes, typename... ArgTypes>
  InFlightDiagnostic diagnose(SourceLoc Loc, Diag<DiagArgTypes...> DiagID,
                              ArgTypes &&...Args) {
    return diagnose(Loc, Diagnostic(DiagID, std::forward<ArgTypes>(Args)...));
  }

  template <typename... DiagArgTypes, typename... ArgTypes>
  InFlightDiagnostic diagnose(const SyntaxRef &Node,
                              Diag<DiagArgTypes...> DiagID,
                              ArgTypes &&...Args) {
    return diagnose(getContentStartLoc(Node),
                    Diagnostic(DiagID, std::forward<ArgTypes>(Args)...));
  }

  /// If the token starts with a '$', diagnose it if not permitted in this mode.
  /// \param DiagnoseDollarPrefix Whether to diagnose dollar-prefixed
  /// identifiers in addition to a standalone '$'.
  void diagnoseDollarIdentifier(const TokenSyntaxRef &Token,
                                bool DiagnoseDollarPrefix);

  //===--------------------------------------------------------------------===//
  // MARK: - Locations
private:
  /// Return the \c SourceLoc that points to the first leading trivia of \p
  /// Node.
  ///
  /// IMPORTANT: This location cannot be safely used to describe source
  /// locations in the AST because they are required to point to the start of
  /// the token. Use \c getLoc instead to determine locations for the AST.
  SourceLoc getLeadingTriviaLoc(const SyntaxRef &Node);

  /// Return the \c SourceLoc after the leading trivia of \p Node.
  ///
  /// IMPORTANT: This location cannot be safely used to describe source
  /// locations in the AST because they are required to point to the start of
  /// the token. This is not guaranteed by this method if the first token in
  /// this node is missing. Use \c getLoc instead to determine locations for the
  /// AST.
  SourceLoc getContentStartLoc(const SyntaxRef &Node);

  /// Return the start location of this token. If the token is missing, return
  /// an invalid location. The result of this method can safely be used to
  /// describe source locations in the AST.
  SourceLoc getLoc(const TokenSyntaxRef &Token) {
    if (Token.isMissing()) {
      return SourceLoc();
    } else {
      return getLoc(Token.getAbsoluteRawRef());
    }
  }

  /// Return the start location of this token. If the token is missing, return
  /// an invalid location. The result of this method can safely be used to
  /// describe source locations in the AST.
  SourceLoc getLoc(const AbsoluteRawSyntaxRef &AbsoluteRaw) {
    if (AbsoluteRaw.getRawRef()->isMissing()) {
      return SourceLoc();
    } else {
      return TreeStartLoc.getAdvancedLoc(
          AbsoluteRaw.getPosition().getOffset() +
          AbsoluteRaw.getRawRef()->getLeadingTriviaLength());
    }
  }

  /// Form a \c SourceRange that contains all tokens in \p Node and is valid to
  /// be used in the AST.
  /// If \p Node does not contain any present tokens, the resulting source range
  /// will span the token *preceeding* \p Node. This is in accordance with the
  /// previous best practice of the AST.
  SourceRange getASTRange(const SyntaxRef &Node);

  /// Return the \c CharSourceRange occupied by \p Node, including leading and
  /// trailing trivia.
  CharSourceRange getRangeWithTrivia(const SyntaxRef &Node);

  /// Return the \c CharSourceRange occupied by \p Node, excluding leading or
  /// trailing trivia.
  CharSourceRange getRangeWithoutTrivia(const SyntaxRef &Node);
};

} // namespace swift

#endif // SWIFT_PARSE_ASTGEN_H

