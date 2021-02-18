// REQUIRES: syntax_parser_lib
// REQUIRES: disabled
// RUN: %swift-syntax-parser-test %s -dump-tree > %t.result
// RUN: diff -u %s.result %t.result

func test() {
  "a\(b)c"
}
