//
//  Sets.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 15/11/2023.
//

import Foundation

// ---------- Set related Expression objects ---------- //

enum SetNames: String {
    case empty = "Set.Empty"
    case universe = "Set.Universe"
    case binaryOp = "Set.BinaryOp"
    case complement = "Set.Complement"
}

enum SetBinaryOp {
    case intersection, union, subset, superset, equals
}

// these should all be higher than the proposition precedences
// might need to reduce the proposition precedences
let SetBinaryOpInfo = ["intersection": ("n", 84, true),
                       "union": ("u", 84, true),
                       "subset": ("<<", 82, true),
                       "superset": (">>", 82, true),
                       "equals": ("=", 82, true)]


func mkSetEmpty() -> Expression {
    return Expression(type: ExpressionType.constant, name: SetNames.empty.rawValue, value: "empty")
}

func mkSetUniverse() -> Expression {
    return Expression(type: ExpressionType.constant, name: SetNames.universe.rawValue, value: "universe")
}

func mkSetBinaryOp(_ op: SetBinaryOp, left: Expression, right: Expression) -> Expression {
    return Expression(type: ExpressionType.expression, name: SetNames.binaryOp.rawValue, value: "\(op)", subexpressions: [left, right])
}

func mkSetComplement(expression exp: Expression) -> Expression {
    return Expression(type: ExpressionType.expression, name: SetNames.complement.rawValue, value: "", subexpressions: [exp])
}

let setsSystemNames: [(String, Int, ([Expression]) -> Expression)] = [
    ("empty", 0, { _ in mkSetEmpty()}),
    ("universe", 0, { _ in mkSetUniverse()}),
    ("intersection", 2, { exps in mkSetBinaryOp(.intersection, left: exps[0], right: exps[1])}),
    ("union", 2, { exps in mkSetBinaryOp(.union, left: exps[0], right: exps[1])}),
    ("subset", 2, { exps in mkSetBinaryOp(.subset, left: exps[0], right: exps[1])}),
    ("superset", 2, { exps in mkSetBinaryOp(.superset, left: exps[0], right: exps[1])}),
    ("setEquals", 2, { exps in mkSetBinaryOp(.equals, left: exps[0], right: exps[1])})]


// ---------- Printing the Expression objects ---------- //

// basic printers

func print_SetEmpty(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == SetNames.empty.rawValue else { return "Unable to print \(exp.name)"}
    // print
    return "{}"
}

func print_SetUniverse(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == SetNames.universe.rawValue else { return "Unable to print \(exp.name)"}
    // print
    return "U"
}

func print_SetBinaryOp(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == SetNames.binaryOp.rawValue else { return "Unable to print \(exp.name)"}
    guard subexps.count == 2 else { return "Invalid Expression" }
    // print
    return switch exp.value {
    case "intersection": "(\(subexps[0]))n(\(subexps[1]))"
    case "union": "(\(subexps[0]))u(\(subexps[1]))"
    case "subset": "(\(subexps[0]))<<(\(subexps[1]))"
    case "superset": "(\(subexps[0]))>>(\(subexps[1]))"
    case "equals": "(\(subexps[0]))=(\(subexps[1]))"
    default: "Invalid Expression"
    }
}

func print_SetComplement(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == SetNames.complement.rawValue else { return "Unable to print \(exp.name)"}
    guard subexps.count == 1 else { return "Invalid Expression" }
    // print
    return "comp(\(subexps[0]))"
}

// text pretty printers

func pretty_SetEmpty(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == SetNames.empty.rawValue else { return ("Unable to print \(exp.name)", 100) }
    // print
    return ("{}", 100)
}

func pretty_SetUniverse(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == SetNames.universe.rawValue else { return ("Unable to print \(exp.name)", 100) }
    // print
    return ("U", 100)
}

func pretty_SetBinaryOp(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == SetNames.binaryOp.rawValue else { return ("(Unable to print \(exp.name))", 100) }
    guard subexps.count == 2 else { return ("(Invalid Expression)", 100) }
    // print info
    let (left, lPrec) = subexps[0]
    let (right, rPrec) = subexps[1]
    if let (opStr, opPrec, infix) = BinaryOpInfo[exp.value] {
        if infix {
            let leftStr = lPrec > opPrec ? "\(left)" : "(\(left))"
            let rightStr = rPrec > opPrec ? "\(right)" : "(\(right))"
            return ("\(leftStr) \(opStr) \(rightStr)", opPrec)
        } else {
            // prefix
            return ("\(opStr)(\(left),\(right))", 100)
        }
    } else {
        return ("(Invalid Expression)", 100)
    }
}

func pretty_SetComplement(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == SetNames.complement.rawValue else { return ("(Unable to print \(exp.name))", 100) }
    guard subexps.count == 1 else { return ("(Invalid Expression)", 100) }
    // print
    let (subexp, _) = subexps[0]
    return ("comp(\(subexp))", 100)
}

// mathml printers

func mathml_SetEmpty(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == SetNames.empty.rawValue else { return ("Unable to print \(exp.name)", 100) }
    // mathml
    return (exp.wrapId(mathML: "<mtext>{}</mtext>"), 100)
}

func mathml_SetUniverse(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == SetNames.universe.rawValue else { return ("Unable to print \(exp.name)", 100) }
    // mathml
    return (exp.wrapId(mathML: "<mi>U</mi>"), 100)
}

func mathml_SetBinaryOp(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == SetNames.binaryOp.rawValue else { return ("Unable to print \(exp.name)", 100)}
    guard subexps.count == 2 else { return ("Invalid Expression", 100) }
    // print
    let (left, lPrec) = subexps[0]
    let (right, rPrec) = subexps[1]
    if let (opStr, opPrec, infix) = BinaryOpInfo[exp.value] {
        let opStr_ = opStr.replacingOccurrences(of: "<", with: "&lt;")
        let opStr__ = opStr_.replacingOccurrences(of: ">", with: "&gt;")
        if infix {
            let leftStr = lPrec > opPrec ? "\(left)" : "<mo>(</mo>\(left)<mo>)</mo>"
            let rightStr = rPrec > opPrec ? "\(right)" : "<mo>(</mo>\(right)<mo>)</mo>"
            return ("\(leftStr) <mo>\(opStr__)</mo> \(rightStr)", opPrec)
        } else {
            // prefix
            return ("<mn>\(opStr)</mn><mo>(</mo>\(left)<mo>,</mo>\(right)<mo>)</mo>", 100)
        }
    } else {
        return ("Invalid expression", 100)
    }
}

func mathml_SetComplement(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PropositionNames.not.rawValue else { return ("Unable to print \(exp.name)", 100) }
    guard subexps.count == 1 else { return ("Invalid Expression", 100) }
    // print
    let (subexp, _) = subexps[0]
    return ("<mtext>comp</mtext>\(subexp)", 100)
}

// register the printing functions

func updatePrinterSets(_ printer: inout ExpressionPrinter) {
    // basic printing
    printer.register(printFn:print_SetEmpty, for:SetNames.empty.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_SetUniverse, for:SetNames.universe.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_SetBinaryOp, for:SetNames.binaryOp.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_SetComplement, for:SetNames.complement.rawValue, ofType: PrinterType.basic.rawValue)
    // pretty printing
    printer.register(prettyPrintFn:pretty_SetEmpty, for:SetNames.empty.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_SetUniverse, for:SetNames.universe.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_SetBinaryOp, for:SetNames.binaryOp.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_SetComplement, for:SetNames.complement.rawValue, ofType: PrinterType.pretty.rawValue)
    // mathml printing
    printer.register(prettyPrintFn:mathml_SetEmpty, for:SetNames.empty.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_SetUniverse, for:SetNames.universe.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_SetBinaryOp, for:SetNames.binaryOp.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_SetComplement, for:SetNames.complement.rawValue, ofType: PrinterType.mathml.rawValue)
}

// ---------- Rewrite Rules ---------- //

// ---------- Axiom Schemas ---------- //
