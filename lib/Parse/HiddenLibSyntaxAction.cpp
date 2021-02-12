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

OpaqueSyntaxNode
HiddenLibSyntaxAction::makeHiddenNode(OpaqueSyntaxNode explicitActionNode,
                                      OpaqueSyntaxNode libSyntaxNode) {
  if (ExplicitAction == nullptr || ExplicitAction == LibSyntaxAction) {
    return libSyntaxNode;
  } else {
    auto dat = NodeAllocator.Allocate();
    return new (dat) HiddenNode(explicitActionNode, libSyntaxNode);
  }
}

OpaqueSyntaxNode HiddenLibSyntaxAction::recordToken(tok tokenKind,
                                                    StringRef leadingTrivia,
                                                    StringRef trailingTrivia,
                                                    CharSourceRange range) {

  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    explicitActionNode = ExplicitAction->recordToken(tokenKind, leadingTrivia,
                                                     trailingTrivia, range);
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    libSyntaxActionNode = LibSyntaxAction->recordToken(tokenKind, leadingTrivia,
                                                       trailingTrivia, range);
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
}

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

OpaqueSyntaxNode HiddenLibSyntaxAction::recordRawSyntax(
    syntax::SyntaxKind kind, const SmallVector<OpaqueSyntaxNode, 4> &elements,
    CharSourceRange range) {
  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    if (ExplicitAction == LibSyntaxAction) {
      explicitActionNode =
          ExplicitAction->recordRawSyntax(kind, elements, range);
    } else {
      SmallVector<OpaqueSyntaxNode, 4> explicitActionElements;
      explicitActionElements.reserve(elements.size());
      for (auto element : elements) {
        OpaqueSyntaxNode explicitActionElement = nullptr;
        if (element) {
          explicitActionElement =
              static_cast<HiddenNode *>(element)->ExplicitActionNode;
        }
        explicitActionElements.push_back(explicitActionElement);
      }

      explicitActionNode =
          ExplicitAction->recordRawSyntax(kind, explicitActionElements, range);
    }
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    if (ExplicitAction == nullptr) {
      libSyntaxActionNode =
          LibSyntaxAction->recordRawSyntax(kind, elements, range);
    } else {
      SmallVector<OpaqueSyntaxNode, 4> libSyntaxActionElements;
      libSyntaxActionElements.reserve(elements.size());
      for (auto element : elements) {
        OpaqueSyntaxNode libSyntaxActionElement = nullptr;
        if (element) {
          libSyntaxActionElement = getLibSyntaxNodeFor(element);
        }
        libSyntaxActionElements.push_back(libSyntaxActionElement);
      }
      libSyntaxActionNode = LibSyntaxAction->recordRawSyntax(
          kind, libSyntaxActionElements, range);
    }
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

OpaqueSyntaxNode HiddenLibSyntaxAction::makeDeferredToken(
    tok tokenKind, StringRef leadingTrivia, StringRef trailingTrivia,
    CharSourceRange range, bool isMissing) {
  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    explicitActionNode = ExplicitAction->makeDeferredToken(
        tokenKind, leadingTrivia, trailingTrivia, range, isMissing);
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    libSyntaxActionNode = LibSyntaxAction->makeDeferredToken(
        tokenKind, leadingTrivia, trailingTrivia, range, isMissing);
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
}

OpaqueSyntaxNode HiddenLibSyntaxAction::makeDeferredLayout(
    syntax::SyntaxKind k, CharSourceRange Range, bool IsMissing,
    const SmallVector<OpaqueSyntaxNode, 4> &children) {
  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    if (ExplicitAction == LibSyntaxAction) {
      explicitActionNode =
          ExplicitAction->makeDeferredLayout(k, Range, IsMissing, children);
    } else {
      SmallVector<OpaqueSyntaxNode, 4> explicitActionChildren;
      explicitActionChildren.reserve(children.size());
      for (auto child : children) {
        OpaqueSyntaxNode explicitActionChild = nullptr;
        if (child) {
          explicitActionChild =
              static_cast<HiddenNode *>(child)->ExplicitActionNode;
        }
        explicitActionChildren.push_back(explicitActionChild);
      }

      explicitActionNode = ExplicitAction->makeDeferredLayout(
          k, Range, IsMissing, explicitActionChildren);
    }
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    if (ExplicitAction == nullptr) {
      libSyntaxActionNode =
          LibSyntaxAction->makeDeferredLayout(k, Range, IsMissing, children);
    } else {
      SmallVector<OpaqueSyntaxNode, 4> libSyntaxActionChildren;
      libSyntaxActionChildren.reserve(children.size());
      for (auto child : children) {
        OpaqueSyntaxNode libSyntaxActionChild = nullptr;
        if (child) {
          libSyntaxActionChild = getLibSyntaxNodeFor(child);
        }
        libSyntaxActionChildren.push_back(libSyntaxActionChild);
      }
      libSyntaxActionNode = LibSyntaxAction->makeDeferredLayout(
          k, Range, IsMissing, libSyntaxActionChildren);
    }
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::recordDeferredToken(OpaqueSyntaxNode deferred) {
  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    explicitActionNode =
        ExplicitAction->recordDeferredToken(getExplicitNodeFor(deferred));
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    libSyntaxActionNode =
        LibSyntaxAction->recordDeferredToken(getLibSyntaxNodeFor(deferred));
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::recordDeferredLayout(OpaqueSyntaxNode deferred) {
  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    explicitActionNode =
        ExplicitAction->recordDeferredLayout(getExplicitNodeFor(deferred));
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    libSyntaxActionNode =
        LibSyntaxAction->recordDeferredLayout(getLibSyntaxNodeFor(deferred));
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
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
