// RUN: %target-typecheck-verify-swift -swift-version 6

@ MainActor  // expected-error {{extraneous whitespace between '@' and attribute name}}
class Foo {
  func funcWithEscapingClosure(_ x: @ escaping () -> Int) {} // expected-error {{extraneous whitespace between '@' and attribute name}}
}

@available (*, deprecated) // expected-error {{extraneous whitespace between attribute name and '('}}
func deprecated() {}

@propertyWrapper
struct MyPropertyWrapper {
  var wrappedValue: Int = 1

  init(param: Int) {}
}

struct PropertyWrapperTest {
  @MyPropertyWrapper (param: 2)  // expected-error {{extraneous whitespace between attribute name and '('}}
  var x: Int
}
