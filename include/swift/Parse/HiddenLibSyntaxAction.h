//===--- HiddenLibSyntaxAction.h ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 202- Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_HIDDENLIBSYNTAXACTION_H
#define SWIFT_PARSE_HIDDENLIBSYNTAXACTION_H

#include "swift/Parse/SyntaxParseActions.h"
#include "swift/SyntaxParse/SyntaxTreeCreator.h"
#include "llvm/Support/Allocator.h"

namespace swift {
namespace syntax {
class RawSyntax;
}

// TODO: (syntax-parse) remove when possible
/// Dispatches all syntax actions to both an explicit action and an implicit
/// (hidden) action that generates a libSyntax tree. It thereby guarantees that
/// the libSyntax tree is always generated. The returned opaque nodes contain
/// both the node created by the explicit action and the implicit action.
/// \c getLibSyntaxNodeFor and \c getExplicitNodeFor can be used to retrieve
/// the opaque nodes of the underlying actions.
class HiddenLibSyntaxAction final : public SyntaxParseActions {

  /// The type of the opaque nodes returned by the \c HiddenLibSyntaxAction.
  /// Contains a pointer to the explicit action's opaque node and the opaque
  /// libSyntax node.
  struct HiddenNode {
    OpaqueSyntaxNode ExplicitActionNode;
    OpaqueSyntaxNode LibSyntaxNode;

    HiddenNode(OpaqueSyntaxNode explicitActionNode,
               OpaqueSyntaxNode libSyntaxNode)
        : ExplicitActionNode(explicitActionNode), LibSyntaxNode(libSyntaxNode) {
    }
  };

  /// The explicit action.
  std::shared_ptr<SyntaxParseActions> ExplicitAction;

  /// The implicit action that generates the libSyntax tree. Is never \c
  /// nullptr.
  std::shared_ptr<SyntaxTreeCreator> LibSyntaxAction;

  /// An allocator using which the \c HiddenNodes are created.
  llvm::SpecificBumpPtrAllocator<HiddenNode> NodeAllocator;

  /// Create a hidden node that contains the given explicit and libSyntax node.
  OpaqueSyntaxNode makeHiddenNode(OpaqueSyntaxNode explicitActionNode,
                                  OpaqueSyntaxNode libSyntaxNode) {
    if (ExplicitAction == nullptr || ExplicitAction == LibSyntaxAction) {
      return libSyntaxNode;
    } else {
      auto dat = NodeAllocator.Allocate();
      return new (dat) HiddenNode(explicitActionNode, libSyntaxNode);
    }
  }

public:
  /// Create a new HiddenLibSyntaxAction. \c LibSyntaxAction must not be \c
  /// null.
  HiddenLibSyntaxAction(
      const std::shared_ptr<SyntaxParseActions> &explicitAction,
      const std::shared_ptr<SyntaxTreeCreator> &libSyntaxAction)
      : ExplicitAction(explicitAction), LibSyntaxAction(libSyntaxAction) {
    assert(libSyntaxAction != nullptr);
  };

  OpaqueSyntaxNode recordToken(tok tokenKind, StringRef leadingTrivia,
                               StringRef trailingTrivia,
                               CharSourceRange range) override {
    
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

  OpaqueSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc) override;

  OpaqueSyntaxNode
  recordRawSyntax(syntax::SyntaxKind kind,
                  const SmallVector<OpaqueSyntaxNode, 4> &elements,
                  size_t ByteLength) override {
    OpaqueSyntaxNode explicitActionNode;
    if (ExplicitAction) {
      if (ExplicitAction == LibSyntaxAction) {
        explicitActionNode =
            ExplicitAction->recordRawSyntax(kind, elements, ByteLength);
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
            ExplicitAction->recordRawSyntax(kind, explicitActionElements, ByteLength);
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
            LibSyntaxAction->recordRawSyntax(kind, elements, ByteLength);
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
            kind, libSyntaxActionElements, ByteLength);
      }
    }

    return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
  }

  /// Realize the syntax root using the implicit \c LibSyntaxAction.
  Optional<syntax::SourceFileSyntax>
  realizeSyntaxRoot(OpaqueSyntaxNode root, const SourceFile &SF) override;

  void discardRecordedNode(OpaqueSyntaxNode node) override;

  OpaqueSyntaxNode makeDeferredToken(tok tokenKind, StringRef leadingTrivia,
                                     StringRef trailingTrivia,
                                     CharSourceRange range,
                                     bool isMissing) override {
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

  OpaqueSyntaxNode
  makeDeferredLayout(syntax::SyntaxKind k, size_t ByteLength,
                     bool IsMissing,
                     const SmallVector<OpaqueSyntaxNode, 4> &children) override {
    OpaqueSyntaxNode explicitActionNode;
    if (ExplicitAction) {
      if (ExplicitAction == LibSyntaxAction) {
        explicitActionNode =
            ExplicitAction->makeDeferredLayout(k, ByteLength, IsMissing, children);
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
            k, ByteLength, IsMissing, explicitActionChildren);
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
            LibSyntaxAction->makeDeferredLayout(k, ByteLength, IsMissing, children);
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
            k, ByteLength, IsMissing, libSyntaxActionChildren);
      }
    }

    return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
  }

  OpaqueSyntaxNode recordDeferredToken(OpaqueSyntaxNode deferred) override {
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
  OpaqueSyntaxNode recordDeferredLayout(OpaqueSyntaxNode deferred) override {
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

  DeferredNodeInfo getDeferredChild(OpaqueSyntaxNode node, size_t ChildIndex) override;
  
  size_t getByteLength(OpaqueSyntaxNode node) override {
    size_t libSyntaxLength = LibSyntaxAction->getByteLength(getLibSyntaxNodeFor(node));
    if (ExplicitAction) {
      assert(ExplicitAction->getByteLength(getExplicitNodeFor(node)) == libSyntaxLength);
    }
    return libSyntaxLength;
  }
  
  tok getTokenKind(OpaqueSyntaxNode node) override {
    tok libSyntaxTokKind = LibSyntaxAction->getTokenKind(getLibSyntaxNodeFor(node));
    if (ExplicitAction) {
      assert(ExplicitAction->getTokenKind(getExplicitNodeFor(node)) == libSyntaxTokKind);
    }
    return libSyntaxTokKind;
  }

  /// Look a node at the given lexer offset up using the \c ExplicitAction.
  ///
  /// IMPORTANT: The \c LibSyntaxAction will not be called during the node
  /// lookup and the LibSyntaxNode part of the resulting \c HiddenNode will be a
  /// \c nullptr.
  ///
  /// Discussion: We only perform the lookup because otherwise we could get a
  /// lookup mismatch between the two actions. In that case, it is not clear,
  /// what kind of node and lexer offset we should return. For now, just
  /// performing the lookup in the explicit action seems to be sufficient and
  /// since \c HiddenLibSyntax action is designed to only be temporary, we
  /// should be able to live with it.
  std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) override;

  /// Returns the libSyntax node from the specified node that has been created
  /// by this action.
  syntax::RawSyntax *getLibSyntaxNodeFor(OpaqueSyntaxNode node);

  /// Returns the node created by explicit syntax action from the specified
  /// node that has been created by this action.
  OpaqueSyntaxNode getExplicitNodeFor(OpaqueSyntaxNode node);

  /// Returns the underlying libSyntax \c SyntaxTreeCreator.
  std::shared_ptr<SyntaxTreeCreator> getLibSyntaxAction() {
    return LibSyntaxAction;
  }
};
} // namespace swift

#endif // SWIFT_PARSE_HIDDENLIBSYNTAXACTION_H
