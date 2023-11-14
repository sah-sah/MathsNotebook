//
//  Expression.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 26/10/2023.
//

import Foundation

enum ExpressionType {
    case empty, constant, variable, indexed_variable, pattern, expression, sysname
}

// Expression values should not be created directly, use the functions provided
struct Expression: Equatable, Identifiable {
    // properties defining the Expression
    let id: UUID = UUID()  // id.uuidString
    var type: ExpressionType    // the type of the expression
    var name: String            // a name for the expression
    var value: String          // a value for the expression represented as a string
    private(set) var subexpressions: Array<Expression>      // sub-expressions
    
    // creating an Expression
    init(type: ExpressionType, name: String, value: String, subexpressions: Array<Expression> = []) {
        self.type = type
        self.name = name
        self.value = value
        self.subexpressions = subexpressions
    }
    
    // updating the sub-expressions
    mutating func setSubexpression(at index: Int, to subexp: Expression) {
        if index >= 0 && index < subexpressions.count {
            subexpressions[index] = subexp
        }
    }
    
    // defining equality
    static func == (lhs: Expression, rhs: Expression) -> Bool {
        // must have the same type
        guard lhs.type == rhs.type else { return false }
        // must have the same name
        guard lhs.name == rhs.name else { return false }
        // must have the same value (which maybe nil)
        guard lhs.value == rhs.value else { return false }
        // must have the same sub expressions (which maybe nil)
        guard lhs.subexpressions == rhs.subexpressions else { return false }
        // nothing more to check
        return true
    }
    
    static func match(lhs: Expression, rhs: Expression) -> Bool {
        // must have the same type
        guard lhs.type == rhs.type else { return false }
        // must have the same name
        guard lhs.name == rhs.name else { return false }
        // must have the same value (which may be nil)
        guard lhs.value == rhs.value else { return false }
        // must have the same number of sub expressions
        guard lhs.subexpressions.count == rhs.subexpressions.count else { return false }
        // nothing more to check
        return true
    }
    
    // default printing function
    static func basic(print exp: Expression, with subexps: [String]) -> String {
        // get String defining the value
        var node_str = "<\(exp.name).\(exp.value)>"
        if subexps.count > 0 {
            node_str += "(" + subexps.joined(separator: ",") + ")"
        }
        return node_str
    }
    
    // adds UUID to mathml string
    func wrapId(mathML: String) -> String {
        return "<mrow class=\"expression\" id=\"\(id.uuidString)\">\(mathML)</mrow>"
    }
    
    mutating func replaceAtCursor(_ cds: [CursorDirection], with exp: Expression) {
        // only works for cds.count > 0
        if cds.count == 1 {
            setSubexpression(at: cds[0].rawValue, to: exp)
        } else {
            subexpressions[cds[0].rawValue].replaceAtCursor(Array(cds[1..<cds.count]), with: exp)
        }
    }
    
}

enum ExpressionNames: String {
    case variable = "Expression.Variable"
    case indexedVariable = "Expression.IndexedVariable"
    case pattern = "Expression.Pattern"
    case empty = "Expression.Empty"
    case sysname = "Expression.Sysname"
}

// ---------- Some useful Expression objects ---------- //

// returns a standard variable expression
func mkVariable(named name: String) -> Expression {
    return Expression(type: ExpressionType.variable, name: ExpressionNames.variable.rawValue, value: name)
}

func mkIndexedVariable(name: String, index_exp: Expression) -> Expression {
    return Expression(type: ExpressionType.indexed_variable, name: ExpressionNames.indexedVariable.rawValue, value: name, subexpressions: [index_exp])
}

func mkPattern(name: String) -> Expression {
    return Expression(type: ExpressionType.pattern, name: ExpressionNames.pattern.rawValue, value: name)
}

func mkEmpty() -> Expression {
    return Expression(type: ExpressionType.empty, name: ExpressionNames.empty.rawValue, value:"")
}

// these are temporary Expressions which should be looked up,
// they should never be used directly
func mkSysname(name: String, args: [Expression]) -> Expression {
    return Expression(type: ExpressionType.sysname, name: ExpressionNames.sysname.rawValue, value:name, subexpressions: args)
}

// ---------- Printing the Expression objects ---------- //
// TODO: should these throw exceptions etc

// basic printers

func print_ExpressionVariable(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == ExpressionNames.variable.rawValue else { return "Unable to print \(exp.name)"}
    // print
    return exp.value
}

func print_ExpressionIndexedVariable(exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == ExpressionNames.indexedVariable.rawValue else { return "Unable to print \(exp.name)"}
    guard subexps.count == 1 else { return "Invalid Expression" }
    // print
    return "\(exp.value)_(\(subexps[0]))"
}

func print_ExpressionPattern(exp: Expression, with subexps: [String]) -> String {
    guard exp.name == ExpressionNames.pattern.rawValue else { return "Unable to print \(exp.name)"}
    // print
    return "@{\(exp.value)}"
}

func print_ExpressionEmpty(exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == ExpressionNames.empty.rawValue else { return "Unable to print \(exp.name)"}
    // print
    return "{}"
}

// text pretty printer

func pretty_ExpressionVariable(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == ExpressionNames.variable.rawValue else { return ("(Unable to print \(exp.name))", 100) }
    // print
    return (exp.value, 100)
}

func pretty_ExpressionIndexedVariable(exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == ExpressionNames.indexedVariable.rawValue else { return ("(Unable to print \(exp.name))", 100) }
    guard subexps.count == 1 else { return ("(Invalid Expression)", 100) }
    // print
    return ("\(exp.value)_(\(subexps[0]))", 100)
}

func pretty_ExpressionPattern(exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    guard exp.name == ExpressionNames.pattern.rawValue else { return ("(Unable to print \(exp.name))", 100) }
    // print
    return ("@{\(exp.value)}", 100)
}

func pretty_ExpressionEmpty(exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == ExpressionNames.empty.rawValue else { return ("(Unable to print \(exp.name))", 100) }
    // print
    return ("{}", 100)
}

// mathml pretty printer

func mathml_ExpressionVariable(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == ExpressionNames.variable.rawValue else { return ("Unable to print \(exp.name)", 100) }
    //return "<mrow class=\"expression\" id=\"\(exp.id.uuidString)\"><mi>\(val)</mi></mrow>"
    return (exp.wrapId(mathML: "<mi>\(exp.value)</mi>"), 100)
}

func mathml_ExpressionIndexedVariable(exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == ExpressionNames.indexedVariable.rawValue else { return ("Unable to print \(exp.name)", 100) }
    guard subexps.count == 1 else { return ("Invalid Expression", 100) }
    //return "<mrow class=\"expression\" id=\"\(exp.id.uuidString)\"><msub><mi>\(val)<mi>\(subexps[0])</msub></mrow>"
    return (exp.wrapId(mathML: "<msub><mi>\(exp.value)<mi>\(subexps[0])</msub>"), 100)
}

func mathml_ExpressionPattern(exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    guard exp.name == ExpressionNames.pattern.rawValue else { return ("Unable to print \(exp.name)", 100) }
    //return "<mrow id=\"\(exp.id.uuidString)\"><mn>@{\(val)}</mn></mrow>"
    return (exp.wrapId(mathML: "<mn>@{\(exp.value)}</mn>"), 100)
}

func mathml_ExpressionEmpty(exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == ExpressionNames.empty.rawValue else { return ("Unable to print \(exp.name)", 100) }
    // print
    //return "<mrow id=\"\(exp.id.uuidString)\"><mn>{}</mn></mrow>"
    return (exp.wrapId(mathML: "<mn>{}</mn>"), 100)
    
}

// register the printing functions

func updatePrinterExpressions(_ printer: inout ExpressionPrinter) {
    // basic printing
    printer.register(printFn:print_ExpressionVariable, for: ExpressionNames.variable.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_ExpressionIndexedVariable, for: ExpressionNames.indexedVariable.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_ExpressionPattern, for: ExpressionNames.pattern.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_ExpressionEmpty, for: ExpressionNames.empty.rawValue, ofType: PrinterType.basic.rawValue)
    // text pretty printing
    printer.register(prettyPrintFn:pretty_ExpressionVariable, for: ExpressionNames.variable.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_ExpressionIndexedVariable, for: ExpressionNames.indexedVariable.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_ExpressionPattern, for: ExpressionNames.pattern.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_ExpressionEmpty, for: ExpressionNames.empty.rawValue, ofType: PrinterType.pretty.rawValue)
    // mathml pretty printing
    printer.register(prettyPrintFn:mathml_ExpressionVariable, for: ExpressionNames.variable.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_ExpressionIndexedVariable, for: ExpressionNames.indexedVariable.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_ExpressionPattern, for: ExpressionNames.pattern.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_ExpressionEmpty, for: ExpressionNames.empty.rawValue, ofType: PrinterType.mathml.rawValue)
    
}
