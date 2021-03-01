//===--- HiddenLibSyntaxAction.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/HiddenLibSyntaxAction.h"

#include "swift/AST/ASTContext.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;
using namespace swift::syntax;
using namespace llvm;

OpaqueSyntaxNode HiddenLibSyntaxAction::recordMissingToken(tok tokenKind,
                                                           SourceLoc loc) {
  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    explicitActionNode = ExplicitAction->recordMissingToken(tokenKind, loc);
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    libSyntaxActionNode = LibSyntaxAction->recordMissingToken(tokenKind, loc);
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
}

Optional<syntax::SourceFileSyntax>
HiddenLibSyntaxAction::realizeSyntaxRoot(OpaqueSyntaxNode root,
                                         const SourceFile &SF) {
  auto node = static_cast<const HiddenNode *>(root);
  return LibSyntaxAction->realizeSyntaxRoot(getLibSyntaxNodeFor(node), SF);
}

DeferredNodeInfo HiddenLibSyntaxAction::getDeferredChild(OpaqueSyntaxNode node,
                                                         size_t ChildIndex) {
  Optional<DeferredNodeInfo> explicitActionNodeInfo;
  if (ExplicitAction) {
    explicitActionNodeInfo =
        ExplicitAction->getDeferredChild(getExplicitNodeFor(node), ChildIndex);
  } else {
    explicitActionNodeInfo = None;
  }

  DeferredNodeInfo libSyntaxActionNodeInfo;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNodeInfo = *explicitActionNodeInfo;
  } else {
    libSyntaxActionNodeInfo = LibSyntaxAction->getDeferredChild(
        getLibSyntaxNodeFor(node), ChildIndex);
  }
  if (explicitActionNodeInfo) {
    assert(explicitActionNodeInfo->Data.getKind() ==
           libSyntaxActionNodeInfo.Data.getKind());
    assert(explicitActionNodeInfo->SyntaxKind ==
           libSyntaxActionNodeInfo.SyntaxKind);
    assert(explicitActionNodeInfo->TokenKind ==
           libSyntaxActionNodeInfo.TokenKind);
    assert(explicitActionNodeInfo->IsMissing ==
           libSyntaxActionNodeInfo.IsMissing);
  }
  OpaqueSyntaxNode Data = makeHiddenNode(
      explicitActionNodeInfo ? explicitActionNodeInfo->Data.getOpaque()
                             : nullptr,
      libSyntaxActionNodeInfo.Data.getOpaque());
  return DeferredNodeInfo(
      RecordedOrDeferredNode(Data, libSyntaxActionNodeInfo.Data.getKind()),
      libSyntaxActionNodeInfo.SyntaxKind, libSyntaxActionNodeInfo.TokenKind,
      libSyntaxActionNodeInfo.IsMissing);
}

std::pair<size_t, OpaqueSyntaxNode>
HiddenLibSyntaxAction::lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
  if (ExplicitAction) {
    // TODO: (syntax-parse) We only perform the node lookup in the explicit
    // action If HiddenSyntaxAction should stay, we should find an
    // implementation that also performs lookup in the LibSyntaxAction
    size_t length;
    OpaqueSyntaxNode n;
    std::tie(length, n) = ExplicitAction->lookupNode(lexerOffset, kind);
    if (length == 0) {
      return {0, nullptr};
    }
    if (ExplicitAction == LibSyntaxAction) {
      return {length, makeHiddenNode(n, n)};
    } else {
      return {length, makeHiddenNode(n, nullptr)};
    }
  } else {
    return {0, nullptr};
  }
}

const RawSyntax *
HiddenLibSyntaxAction::getLibSyntaxNodeFor(OpaqueSyntaxNode node) const {
  if (ExplicitAction == nullptr || ExplicitAction == LibSyntaxAction) {
    return static_cast<const RawSyntax *>(node);
  } else {
    auto hiddenNode = static_cast<const HiddenNode *>(node);
    return static_cast<const RawSyntax *>(hiddenNode->LibSyntaxNode);
  }
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::getExplicitNodeFor(OpaqueSyntaxNode node) const {
  if (ExplicitAction == nullptr) {
    return nullptr;
  } else if (ExplicitAction == LibSyntaxAction) {
    return node;
  } else {
    return static_cast<const HiddenNode *>(node)->ExplicitActionNode;
  }
}
