// RUN %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t
// RUN2: %target-swift-ide-test -code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t -code-completion-token=COMPLETE_CASE

@resultBuilder
struct TupleBuilder<T> {
  static func buildBlock() -> () { }

  static func buildBlock(_ t1: T) -> T {
    return t1
  }

  static func buildBlock(_ t1: T, _ t2: T) -> (T, T) {
    return (t1, t2)
  }
  static func buildBlock(_ t1: T, _ t2: T, _ t3: T) -> (T, T, T) {
    return (t1, t2, t3)
  }
}

func buildStringTuple<Result>(@TupleBuilder<String> x: () -> Result) {}

enum StringFactory {
  static func makeString(x: String) -> String { return x }
}

enum Letters {
  case a
  case b
  case c
}

let MyConstantString = "MyConstant"

func testGlobalLookup() {
  @TupleBuilder<String> var x1 {
    #^GLOBAL_LOOKUP^#
    // GLOBAL_LOOKUP: Begin completions
    // GLOBAL_LOOKUP: Decl[GlobalVar]/CurrModule:         MyConstantString[#String#];
    // GLOBAL_LOOKUP: End completions
  }

  @TupleBuilder<String> var x2 {
    if true {
      #^GLOBAL_LOOKUP_IN_IF_BODY?check=GLOBAL_LOOKUP^#
    }
  }

  @TupleBuilder<String> var x3 {
    "hello: \(#^GLOBAL_LOOKUP_IN_STRING_LITERAL^#)"
// GLOBAL_LOOKUP_IN_STRING_LITERAL: Begin completions
// GLOBAL_LOOKUP_IN_STRING_LITERAL: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: MyConstantString[#String#];
// GLOBAL_LOOKUP_IN_STRING_LITERAL: End completions
  }
}

func testStaticMemberLookup() {
  @TupleBuilder<String> var x1 {
    StringFactory.#^COMPLETE_STATIC_MEMBER^#
    // COMPLETE_STATIC_MEMBER: Begin completions
    // COMPLETE_STATIC_MEMBER: Decl[StaticMethod]/CurrNominal:     makeString({#x: String#})[#String#];
    // COMPLETE_STATIC_MEMBER: End completions
  }

  @TupleBuilder<String> var x2 {
    if true {
      StringFactory.#^COMPLETE_STATIC_MEMBER_IN_IF_BODY?check=COMPLETE_STATIC_MEMBER^#
    }
  }

  @TupleBuilder<String> var x3 {
    "hello: \(StringFactory.#^COMPLETE_STATIC_MEMBER_IN_STRING_LITERAL?check=COMPLETE_STATIC_MEMBER;xfail=TODO^#)"
  }
}

func testPatternMatching() {
  @TupleBuilder<String> var x1 {
    let x = Letters.a
    switch x {
    case .#^COMPLETE_CASE^#:
      // COMPLETE_CASE: Begin completions
      // COMPLETE_CASE-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: a[#Letters#];
      // COMPLETE_CASE-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: b[#Letters#];
      // COMPLETE_CASE-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: c[#Letters#];
      // COMPLETE_CASE: End completions
      break
    }
  }


  @TupleBuilder<String> var x2 {
    let x = Letters.b
    if case .#^COMPLETE_PATTERN_MATCHING_IN_IF?check=COMPLETE_CASE^# = x {

    }
  }
}

func testCompleteFunctionArgumentLabels() {
  @TupleBuilder<String> var x1 {
    StringFactory.makeString(#^FUNCTION_ARGUMENT_LABEL^#)
    // FUNCTION_ARGUMENT_LABEL: Begin completions, 1 item
    // FUNCTION_ARGUMENT_LABEL: Decl[StaticMethod]/CurrNominal:     ['(']{#x: String#}[')'][#String#];
    // FUNCTION_ARGUMENT_LABEL: End completions
  }
}

func testCompleteFunctionArgument() {
  @TupleBuilder<String> var x1 {
    StringFactory.makeString(x: #^ARGUMENT_LOOKUP^#)
    // ARGUMENT_LOOKUP: Begin completions
    // ARGUMENT_LOOKUP: Decl[GlobalVar]/CurrModule/TypeRelation[Identical]: MyConstantString[#String#];
    // ARGUMENT_LOOKUP: End completions
  }

  @TupleBuilder<String> var x2 {
    if true {
      StringFactory.makeString(x: #^ARGUMENT_LOOKUP_IN_IF_BODY?check=ARGUMENT_LOOKUP^#)
    }
  }
}

func testX() {
  enum Generic<T> {
    case contains(content: T)
    case empty
    static func create(_: T) -> Generic<T> { fatalError() }
  }
  @TupleBuilder<String> var x1 {
    switch Generic<Int>.empty {
    case let .#^CASE_OF_GENERIC_ENUM^#:
// CASE_OF_GENERIC_ENUM: Begin completions, 3 items
// CASE_OF_GENERIC_ENUM: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: contains({#content: Int#})[#Generic<Int>#]; name=contains(content: Int)
// CASE_OF_GENERIC_ENUM: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: empty[#Generic<Int>#]; name=empty
// CASE_OF_GENERIC_ENUM: Decl[StaticMethod]/ExprSpecific/TypeRelation[Identical]: create({#Int#})[#Generic<Int>#]; name=create(Int)
// CASE_OF_GENERIC_ENUM: End completions
      break
    }
  }
}

struct FooStruct {
  var instanceVar : Int
  init(_: Int = 0) { }
  func boolGen() -> Bool { return false }
  func intGen() -> Int { return 1 }
}

func testGuardCase(x:FooStruct?) {
  @TupleBuilder<String> var x1 {
    guard case .#^GUARD_CASE_PATTERN_1?check=OPTIONAL_FOOSTRUCT^# = x {}
  }
}
func testGuardCase(x:FooStruct?) {
  @TupleBuilder<String> var x1 {
    guard case .#^GUARD_CASE_PATTERN_2?check=OPTIONAL_FOOSTRUCT^#some() = x {}
  }
}

func test003() {
  enum Error4 : Error {
    case E1
    case E2(Int32)
  }
  @TupleBuilder<String> var x1 {
    do {} catch Error4.#^CATCH2^#
  }
// CATCH2: Begin completions
// CATHC2-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: E1[#Error4#]; name=E1
// CATHC2-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: E2({#Int32#})[#Error4#]; name=E2(Int32)
// CATCH2: End completions
}

// OPTIONAL_FOOSTRUCT: Begin completions, 9 items
// OPTIONAL_FOOSTRUCT-DAG: Keyword[nil]/None/Erase[1]/TypeRelation[Identical]:         nil[#FooStruct?#]; name=nil
// OPTIONAL_FOOSTRUCT-DAG: Decl[EnumElement]/CurrNominal/IsSystem/TypeRelation[Identical]: none[#Optional<FooStruct>#]; name=none
// OPTIONAL_FOOSTRUCT-DAG: Decl[EnumElement]/CurrNominal/IsSystem/TypeRelation[Identical]: some({#FooStruct#})[#Optional<FooStruct>#]; name=some(FooStruct)
// FIXME: 'FooStruct' members should not be shown.
// OPTIONAL_FOOSTRUCT-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: init()[#FooStruct#]; name=init()
// OPTIONAL_FOOSTRUCT-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: init({#Int#})[#FooStruct#]; name=init(Int)
// OPTIONAL_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal:   boolGen({#(self): FooStruct#})[#() -> Bool#]; name=boolGen(self: FooStruct)
// OPTIONAL_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal:   intGen({#(self): FooStruct#})[#() -> Int#]; name=intGen(self: FooStruct)
// OPTIONAL_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal/IsSystem: map({#(self): Optional<FooStruct>#})[#((FooStruct) throws -> U) -> U?#]; name=map(self: Optional<FooStruct>)
// OPTIONAL_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal/IsSystem: flatMap({#(self): Optional<FooStruct>#})[#((FooStruct) throws -> U?) -> U?#]; name=flatMap(self: Optional<FooStruct>)
// OPTIONAL_FOOSTRUCT-NOT: init({#(some):
// OPTIONAL_FOOSTRUCT-NOT: init({#nilLiteral:
// OPTIONAL_FOOSTRUCT: End completions
