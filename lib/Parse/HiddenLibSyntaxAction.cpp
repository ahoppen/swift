//===--- HiddenLibSyntaxAction.cpp ----------------------------------------===//
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
  auto node = static_cast<HiddenNode *>(root);
  return LibSyntaxAction->realizeSyntaxRoot(getLibSyntaxNodeFor(node), SF);
}

void HiddenLibSyntaxAction::discardRecordedNode(OpaqueSyntaxNode opaqueN) {
  if (!opaqueN) {
    return;
  }
  if (ExplicitAction) {
    ExplicitAction->discardRecordedNode(getExplicitNodeFor(opaqueN));
  }
  if (ExplicitAction != LibSyntaxAction) {
    LibSyntaxAction->discardRecordedNode(getLibSyntaxNodeFor(opaqueN));
  }
}

SyntaxParseActions::DeferredNodeInfo HiddenLibSyntaxAction::getDeferredChild(
    OpaqueSyntaxNode node, size_t ChildIndex, SourceLoc ThisNodeLoc) {
  Optional<DeferredNodeInfo> explicitActionNodeInfo;
  if (ExplicitAction) {
    explicitActionNodeInfo = ExplicitAction->getDeferredChild(
        getExplicitNodeFor(node), ChildIndex, ThisNodeLoc);
  } else {
    explicitActionNodeInfo = None;
  }

  DeferredNodeInfo libSyntaxActionNodeInfo;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNodeInfo = *explicitActionNodeInfo;
  } else {
    libSyntaxActionNodeInfo = LibSyntaxAction->getDeferredChild(
        getLibSyntaxNodeFor(node), ChildIndex, ThisNodeLoc);
  }
#ifndef NDEBUG
  if (explicitActionNodeInfo) {
    // For the purpose of ParsedRawSyntaxNodes we don't differentiate between invalid and empty ranges.
    bool explicitRangeEmpty = !explicitActionNodeInfo->Range.isValid() || explicitActionNodeInfo->Range.getByteLength() == 0;
    bool libSyntaxRangeEmpty = !libSyntaxActionNodeInfo.Range.isValid() || libSyntaxActionNodeInfo.Range.getByteLength() == 0;
    assert(explicitRangeEmpty == libSyntaxRangeEmpty || (explicitActionNodeInfo->Range == libSyntaxActionNodeInfo.Range));
    assert(explicitActionNodeInfo->SyntaxKind == libSyntaxActionNodeInfo.SyntaxKind);
    assert(explicitActionNodeInfo->TokenKind == libSyntaxActionNodeInfo.TokenKind);
    assert(explicitActionNodeInfo->IsMissing == libSyntaxActionNodeInfo.IsMissing);
  }
#endif
  OpaqueSyntaxNode Data = makeHiddenNode(
      explicitActionNodeInfo ? explicitActionNodeInfo->Data : nullptr,
      libSyntaxActionNodeInfo.Data);
  return DeferredNodeInfo(
      Data, libSyntaxActionNodeInfo.Range, libSyntaxActionNodeInfo.SyntaxKind,
      libSyntaxActionNodeInfo.TokenKind, libSyntaxActionNodeInfo.IsMissing);
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

RawSyntax *HiddenLibSyntaxAction::getLibSyntaxNodeFor(OpaqueSyntaxNode node) {
  if (ExplicitAction == nullptr || ExplicitAction == LibSyntaxAction) {
    return static_cast<RawSyntax *>(node);
  } else {
    auto hiddenNode = static_cast<HiddenNode *>(node);
    return static_cast<RawSyntax *>(hiddenNode->LibSyntaxNode);
  }
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::getExplicitNodeFor(OpaqueSyntaxNode node) {
  if (ExplicitAction == nullptr) {
    return nullptr;
  } else if (ExplicitAction == LibSyntaxAction) {
    return node;
  } else {
    return static_cast<HiddenNode *>(node)->ExplicitActionNode;
  }
}
