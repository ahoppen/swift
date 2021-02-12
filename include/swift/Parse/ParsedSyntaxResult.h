//===--- ParsedSyntaxResult.h -----------------------------------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_SYNTAXPARSERRESULT_H
#define SWIFT_PARSE_SYNTAXPARSERRESULT_H

#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/ParserResult.h"
#include "llvm/ADT/Optional.h"

namespace swift {

/// The result type of parsing a source file into a libSyntax tree.
/// Consists of a \c ParsedRawSyntaxNode that has been created by the parser
/// and the current status of the parser, indicating parser errors or code
/// completion presence.
/// Every \c ParsedSyntaxResult has an associated \c ParsedSyntaxNode. Syntax
/// nodes like \c UnknownTypeSyntax or missing tokens are used to represent
/// parser results that did not consume any tokens from the source file.
template <typename ParsedSyntaxNode>
class ParsedSyntaxResult {
public:
  template <typename OtherParsedSyntaxNode>
  friend class ParsedSyntaxResult;

private:
  ParsedSyntaxNode Node;
  ParserStatus Status;

public:
  ParsedSyntaxResult(ParsedSyntaxNode &&Node, ParserStatus Status)
      : Node(std::move(Node)), Status(Status) {
    assert(!this->Node.getRaw().isNull());
  }

  explicit ParsedSyntaxResult(ParsedSyntaxNode &&Node)
      : Node(std::move(Node)), Status() {}

  template <typename OtherParsedSyntaxNode,
            typename Enable = typename std::enable_if<std::is_base_of<
                ParsedSyntaxNode, OtherParsedSyntaxNode>::value>::type>
  ParsedSyntaxResult(ParsedSyntaxResult<OtherParsedSyntaxNode> &&other)
      : Node(std::move(other.Node)), Status(std::move(other.Status)) {}

  bool isSuccess() const { return Status.isSuccess(); }

  bool isError() const { return Status.isError(); }
  void setIsError() { Status.setIsParseError(); }

  bool hasCodeCompletion() const { return Status.hasCodeCompletion(); }
  void setHasCodeCompletionAndIsError() {
    Status.setHasCodeCompletionAndIsError();
  }

  const ParsedSyntaxNode &get() { return Node; }

  ParsedSyntaxNode &&take() { return std::move(Node); }

  ParserStatus getStatus() const { return Status; }
};

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedResult(ParsedSyntaxNode &&Node) {
  return ParsedSyntaxResult<ParsedSyntaxNode>(std::move(Node));
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedError(ParsedSyntaxNode &&Node) {
  auto result = ParsedSyntaxResult<ParsedSyntaxNode>(std::move(Node));
  result.setIsError();
  return result;
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode> makeParsedError() {
  return ParsedSyntaxResult<ParsedSyntaxNode>();
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedCodeCompletion(ParsedSyntaxNode &&Node) {
  auto result = ParsedSyntaxResult<ParsedSyntaxNode>(std::move(Node));
  result.setHasCodeCompletionAndIsError();
  return result;
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedResult(ParsedSyntaxNode &&Node, ParserStatus Status) {
  return ParsedSyntaxResult<ParsedSyntaxNode>(std::move(Node), Status);
}

} // namespace swift

#endif
