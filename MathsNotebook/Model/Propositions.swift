//
//  Propositions.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 27/10/2023.
//

import Foundation

// TODO: we could make these functions static methods of Propositions struct
// TODO: to avoid having so many global variables
// TODO: e.g. use Propositions.mkBool(valued:) instead of mkPropositionBool
// TODO: can make the init function private so the struct cannot be instantiated
// TODO: can use a nested enum to define constants Proposition.Name.bool

// ---------- Some useful Expression objects ---------- //

enum PropositionNames: String {
    case bool = "Proposition.Bool"
    case binaryOp = "Proposition.BinaryOp"
    case not = "Proposition.Not"
}

enum PropositionBinaryOp {
    case and, or, imp, revImp, iff, fst, snd
}

let BinaryOpInfo = ["and": ("&", 80, true),
                    "or": ("|", 80, true),
                    "imp": ("->", 60, true),
                    "revImp": ("<-", 60, true),
                    "iff": ("<->", 60, true),
                    "fst": ("fst", 60, false),
                    "snd": ("snd", 60, false)]

func mkPropositionBool(value: Bool) -> Expression {
    return Expression(type: ExpressionType.constant, name: PropositionNames.bool.rawValue, value: "\(value)")
}

func mkPropositionBinaryOp(_ op: PropositionBinaryOp, left: Expression, right: Expression) -> Expression {
    return Expression(type: ExpressionType.expression, name: PropositionNames.binaryOp.rawValue, value: "\(op)", subexpressions: [left, right])
}



func mkPropositionNot(expression exp: Expression) -> Expression {
    return Expression(type: ExpressionType.expression, name: PropositionNames.not.rawValue, value: "", subexpressions: [exp])
}

let propositionsSystemNames: [(String, Int, ([Expression]) -> Expression)] = [
    ("fst", 2, { exps in mkPropositionBinaryOp(.fst, left: exps[0], right: exps[1])}),
    ("snd", 2, { exps in mkPropositionBinaryOp(.snd, left: exps[0], right: exps[1])})]

// ---------- Printing the Expression objects ---------- //

// basic printers

func print_PropositionBool(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == PropositionNames.bool.rawValue else { return "Unable to print \(exp.name)"}
    // print
    return exp.value == "true" ? "T" : "F"
}

func print_PropositionBinaryOp(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == PropositionNames.binaryOp.rawValue else { return "Unable to print \(exp.name)"}
    guard subexps.count == 2 else { return "Invalid Expression" }
    // print
    return switch exp.value {
    case "and": "(\(subexps[0]))&(\(subexps[1]))"
    case "or": "(\(subexps[0]))|(\(subexps[1]))"
    case "imp": "(\(subexps[0]))->(\(subexps[1]))"
    case "revImp": "(\(subexps[0]))<-(\(subexps[1]))"
    case "iff": "(\(subexps[0]))<->(\(subexps[1]))"
    case "fst": "fst(\(subexps[0]),\(subexps[1]))"
    case "snd": "snd(\(subexps[0]),\(subexps[1]))"
    default: "Invalid Expression"
    }
}

func print_PropositionNot(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == PropositionNames.not.rawValue else { return "Unable to print \(exp.name)"}
    guard subexps.count == 1 else { return "Invalid Expression" }
    // print
    return "~(\(subexps[0]))"
}

// text pretty printers

func pretty_PropositionBool(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PropositionNames.bool.rawValue else { return ("(Unable to print \(exp.name))", 100) }
    // print
    return (exp.value == "true" ? "T" : "F", 100)
}

func pretty_PropositionBinaryOp(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PropositionNames.binaryOp.rawValue else { return ("(Unable to print \(exp.name))", 100) }
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

func pretty_PropositionNot(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PropositionNames.not.rawValue else { return ("(Unable to print \(exp.name))", 100) }
    guard subexps.count == 1 else { return ("(Invalid Expression)", 100) }
    // print
    let (subexp, prec) = subexps[0]
    if prec > 90 {
        // don't need to bracket
        return ("~\(subexp)", 90)
    } else {
        // bracket
        return ("~(\(subexp))", 90)
    }
}

// mathml printers

func mathml_PropositionBool(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PropositionNames.bool.rawValue else { return ("Unable to print \(exp.name)", 100) }
    //return "<mrow id=\"\(exp.id.uuidString)\"><mn>\(val == "true" ? "T" : "F")</mn></mrow>"
    return (exp.wrapId(mathML: "<mn>\(exp.value == "true" ? "T" : "F")</mn>"), 100)
}

func mathml_PropositionBinaryOp(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PropositionNames.binaryOp.rawValue else { return ("Unable to print \(exp.name)", 100)}
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

func mathml_PropositionNot(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PropositionNames.not.rawValue else { return ("Unable to print \(exp.name)", 100) }
    guard subexps.count == 1 else { return ("Invalid Expression", 100) }
    // print
    let (subexp, prec) = subexps[0]
    if prec > 90 {
        // don't need to bracket
        return ("<mn>~</mn>\(subexp)", 90)
    } else {
        // bracket
        return ("<mn>~</mn><mo>(</mo>\(subexps[0])<mo>)</mo>", 90)
    }
}

// register the printing functions

func updatePrinterPropositions(_ printer: inout ExpressionPrinter) {
    // basic printing
    printer.register(printFn:print_PropositionBool, for:PropositionNames.bool.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_PropositionBinaryOp, for:PropositionNames.binaryOp.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_PropositionNot, for:PropositionNames.not.rawValue, ofType: PrinterType.basic.rawValue)
    // pretty printing
    printer.register(prettyPrintFn:pretty_PropositionBool, for:PropositionNames.bool.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_PropositionBinaryOp, for:PropositionNames.binaryOp.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_PropositionNot, for:PropositionNames.not.rawValue, ofType: PrinterType.pretty.rawValue)
    // mathml printing
    printer.register(prettyPrintFn:mathml_PropositionBool, for:PropositionNames.bool.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_PropositionBinaryOp, for:PropositionNames.binaryOp.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_PropositionNot, for:PropositionNames.not.rawValue, ofType: PrinterType.mathml.rawValue)
}

// ---------- Rewrite Rules ---------- //

let equivalence_ex = RewriteRule(name: "equivalence", description: "p <-> q => p -> q && q -> p",
                                 leftPattern: mkPropositionBinaryOp(.iff, left: mkPattern(name: "P"), right: mkPattern(name: "Q")),
                                 rightPattern: mkPropositionBinaryOp(.and, left: mkPropositionBinaryOp(.imp, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPropositionBinaryOp(.imp, left: mkPattern(name: "Q"), right: mkPattern(name: "P"))))

let equivalence_co = RewriteRule(name: "equivalence", description: "p -> q && q -> p => p <-> q",
                                 leftPattern: mkPropositionBinaryOp(.and, left: mkPropositionBinaryOp(.imp, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPropositionBinaryOp(.imp, left: mkPattern(name: "Q"), right: mkPattern(name: "P"))),
                                 rightPattern:mkPropositionBinaryOp(.iff, left: mkPattern(name: "P"), right: mkPattern(name: "Q")))


let reverseImplication_to = RewriteRule(name: "reverseImplication", description: "p -> q => q <- p",
                                        leftPattern: mkPropositionBinaryOp(.revImp, left: mkPattern(name:"P"), right: mkPattern(name: "Q")),
                                        rightPattern: mkPropositionBinaryOp(.imp, left: mkPattern(name:"Q"), right: mkPattern(name: "P")))

let reverseImplication_fr = RewriteRule(name: "reverseImplication", description: "p -> q => q <- p",
                                        leftPattern: mkPropositionBinaryOp(.revImp, left: mkPattern(name:"P"), right: mkPattern(name: "Q")),
                                        rightPattern: mkPropositionBinaryOp(.imp, left: mkPattern(name:"Q"), right: mkPattern(name: "P")))


let implication_ex = RewriteRule(name: "implication", description: "p -> q => (not p) or q",
                                 leftPattern: mkPropositionBinaryOp(.imp, left: mkPattern(name:"P"), right: mkPattern(name: "Q")),
                                 rightPattern: mkPropositionBinaryOp(.or, left: mkPropositionNot(expression: mkPattern(name:"P")), right: mkPattern(name: "Q")))

let implication_co = RewriteRule(name: "implication", description: "(not p) or q => p -> q",
                                 leftPattern: mkPropositionBinaryOp(.or, left: mkPropositionNot(expression: mkPattern(name:"P")), right: mkPattern(name: "Q")),
                                 rightPattern: mkPropositionBinaryOp(.imp, left: mkPattern(name:"P"), right: mkPattern(name: "Q")))


let doubleNegation_co = RewriteRule(name: "doubleNegation", description: "not (not p) => p",
                                    leftPattern: mkPropositionNot(expression: mkPropositionNot(expression: mkPattern(name: "P"))),
                                    rightPattern: mkPattern(name: "P"))

let doubleNegation_ex = RewriteRule(name: "doubleNegation", description: "p => not (not p)",
                                    leftPattern: mkPattern(name: "P"),
                                    rightPattern: mkPropositionNot(expression: mkPropositionNot(expression: mkPattern(name: "P"))))


let idempotent_and_co = RewriteRule(name: "idempotent", description: "p and p => p",
                                    leftPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "P")),
                                    rightPattern: mkPattern(name: "P"))

let idempotent_and_ex = RewriteRule(name: "idempotent", description: "p => p and p",
                                    leftPattern: mkPattern(name: "P"),
                                    rightPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "P")))

let idempotent_or_co = RewriteRule(name: "idempotent", description: "p or p => p",
                                   leftPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "P")),
                                   rightPattern: mkPattern(name: "P"))

let idempotent_or_ex = RewriteRule(name: "idempotent", description: "p => p or p",
                                   leftPattern: mkPattern(name: "P"),
                                   rightPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "P")))


let commutative_and = RewriteRule(name: "commutative", description: "p and q => q and p",
                                  leftPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q")),
                                  rightPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "Q"), right: mkPattern(name: "P")))

let commutative_or = RewriteRule(name: "commutative", description: "p or q => q or p",
                                 leftPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q")),
                                 rightPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "Q"), right: mkPattern(name: "P")))


let associative_and_lf = RewriteRule(name: "associative", description: "(p and q) and r => p and (q and r)",
                                     leftPattern: mkPropositionBinaryOp(.and, left: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPattern(name: "R")),
                                     rightPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.and, left: mkPattern(name: "Q"), right: mkPattern(name: "R"))))

let associative_and_rt = RewriteRule(name: "associative", description: "p and (q and r) => (p and q)",
                                     leftPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.and, left: mkPattern(name: "Q"), right: mkPattern(name: "R"))),
                                     rightPattern: mkPropositionBinaryOp(.and, left: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPattern(name: "R")))

let associative_or_lf = RewriteRule(name: "associative", description: "(p or q) or r => p or (q or r)",
                                    leftPattern: mkPropositionBinaryOp(.or, left: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPattern(name: "R")),
                                    rightPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.or, left: mkPattern(name: "Q"), right: mkPattern(name: "R"))))

let associative_or_rt = RewriteRule(name: "associative", description: "p or (q or r) => (p or q)",
                                    leftPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.or, left: mkPattern(name: "Q"), right: mkPattern(name: "R"))),
                                    rightPattern: mkPropositionBinaryOp(.or, left: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPattern(name: "R")))


let distributive_and_ex = RewriteRule(name: "distributive", description: "p and (q or r) => (p and q) or (p and r)",
                                      leftPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.or, left: mkPattern(name: "Q"), right: mkPattern(name: "R"))),
                                      rightPattern: mkPropositionBinaryOp(.or, left: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "R"))))

let distributive_and_co = RewriteRule(name: "distributive", description: "(p and q) or (p and r) => p and (q or r)",
                                      leftPattern: mkPropositionBinaryOp(.or, left: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "R"))),
                                      rightPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.or, left: mkPattern(name: "Q"), right: mkPattern(name: "R"))))

let distributive_or_ex = RewriteRule(name: "distributive", description: "p or (q and r) <=> (p or q) and (p or r)",
                                     leftPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.and, left: mkPattern(name: "Q"), right: mkPattern(name: "R"))),
                                     rightPattern: mkPropositionBinaryOp(.and, left: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "R"))))

let distributive_or_co = RewriteRule(name: "distributive", description: "(p or q) and (p or r) => p or (q and r)",
                                     leftPattern: mkPropositionBinaryOp(.and, left: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q")), right: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "R"))),
                                     rightPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.and, left: mkPattern(name: "Q"), right: mkPattern(name: "R"))))


let deMorgans_and_ex = RewriteRule(name: "deMorgans", description: "not (p and q) => (not p) or (not q)",
                                   leftPattern: mkPropositionNot(expression: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))),
                                   rightPattern: mkPropositionBinaryOp(.or, left: mkPropositionNot(expression: mkPattern(name: "P")), right: mkPropositionNot(expression: mkPattern(name: "Q"))))

let deMorgans_and_co = RewriteRule(name: "deMorgans", description: "(not p) or (not q) => not (p and q)",
                                   leftPattern: mkPropositionBinaryOp(.or, left: mkPropositionNot(expression: mkPattern(name: "P")), right: mkPropositionNot(expression: mkPattern(name: "Q"))),
                                   rightPattern: mkPropositionNot(expression: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))))

let deMorgans_or_ex = RewriteRule(name: "deMorgans", description: "not (p or q) => (not p) and (not q)",
                                  leftPattern: mkPropositionNot(expression: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))),
                                  rightPattern: mkPropositionBinaryOp(.and, left: mkPropositionNot(expression: mkPattern(name: "P")), right: mkPropositionNot(expression: mkPattern(name: "Q"))))

let deMorgans_or_co = RewriteRule(name: "deMorgans", description: "(not p) and (not q) => not (p or q)",
                                  leftPattern: mkPropositionBinaryOp(.and, left: mkPropositionNot(expression: mkPattern(name: "P")), right: mkPropositionNot(expression: mkPattern(name: "Q"))),
                                  rightPattern: mkPropositionNot(expression: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))))


let identity_and_co = RewriteRule(name: "identity", description: "p and T => p",
                                  leftPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBool(value: true)),
                                  rightPattern: mkPattern(name: "P"))

let identity_and_ex = RewriteRule(name: "identity", description: "p => p and T",
                                  leftPattern: mkPattern(name: "P"),
                                  rightPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBool(value: true)))

let identity_or_co = RewriteRule(name: "identity", description: "p or F => p",
                                 leftPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBool(value: false)),
                                 rightPattern: mkPattern(name: "P"))

let identity_or_ex = RewriteRule(name: "identity", description: "p => p or F",
                                 leftPattern: mkPattern(name: "P"),
                                 rightPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBool(value: false)))


let annihilation_and_co = RewriteRule(name: "annihilation", description: "p and F => F",
                                      leftPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBool(value: false)),
                                      rightPattern: mkPropositionBool(value: false))

let annihilation_and_ex = RewriteRule(name: "annihilation", description: "F => p and F",
                                      leftPattern: mkPropositionBool(value: false),
                                      rightPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBool(value: false)),
                                      inputSpecifications: [InputSpecification(name: "P", description: "Any expression", type: .expression)])

let annihilation_or_co = RewriteRule(name: "annihilation", description: "p or T => T",
                                     leftPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBool(value: true)),
                                     rightPattern: mkPropositionBool(value: true))

let annihilation_or_ex = RewriteRule(name: "annihilation", description: "T => p or T",
                                     leftPattern: mkPropositionBool(value: true),
                                     rightPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBool(value: true)),
                                     inputSpecifications: [InputSpecification(name: "P", description: "Any expression", type: .expression)])


let inverse_and_co = RewriteRule(name: "inverse", description: "p and (not p) => F",
                                 leftPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionNot(expression: mkPattern(name: "P"))),
                                 rightPattern: mkPropositionBool(value: false))

let inverse_and_ex = RewriteRule(name: "inverse", description: "F => p and (not p)",
                                 leftPattern: mkPropositionBool(value: false),
                                 rightPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionNot(expression: mkPattern(name: "P"))),
                                 inputSpecifications: [InputSpecification(name: "P", description: "Any expression", type: .expression)])

let inverse_or_co = RewriteRule(name: "inverse", description: "p or (not p) => T",
                                 leftPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionNot(expression: mkPattern(name: "P"))),
                                 rightPattern: mkPropositionBool(value: true))

let inverse_or_ex = RewriteRule(name: "inverse", description: "T => p or (not p)",
                                 leftPattern: mkPropositionBool(value: true),
                                 rightPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionNot(expression: mkPattern(name: "P"))),
                                 inputSpecifications: [InputSpecification(name: "P", description: "Any expression", type: .expression)])


let absorption_and_co = RewriteRule(name: "absorption", description: "p and (p or q) => p",
                                    leftPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))),
                                    rightPattern: mkPattern(name: "P"))

let absorption_and_ex = RewriteRule(name: "absorption", description: "p => p and (p or q)",
                                    leftPattern: mkPattern(name: "P"),
                                    rightPattern: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))),
                                    inputSpecifications: [InputSpecification(name: "Q", description: "Any expression", type: .expression)])

let absorption_or_co = RewriteRule(name: "absorption", description: "p or (p and q) => p",
                                   leftPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))),
                                   rightPattern: mkPattern(name: "P"))

let absorption_or_ex = RewriteRule(name: "absorption", description: "p => p or (p and q)",
                                   leftPattern: mkPattern(name: "P"),
                                   rightPattern: mkPropositionBinaryOp(.or, left: mkPattern(name: "P"), right: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))),
                                   inputSpecifications: [InputSpecification(name: "Q", description: "Any expression", type: .expression)])


let fstDefinition_co = RewriteRule(name: "fst definition", description: "fst(p,q) => p",
                                   leftPattern: mkPropositionBinaryOp(.fst, left: mkPattern(name: "P"), right: mkPattern(name: "Q")),
                                   rightPattern: mkPattern(name: "P"))

let fstDefinition_ex = RewriteRule(name: "fst definition", description: "p => fst(p,q)",
                                   leftPattern: mkPattern(name: "P"),
                                   rightPattern: mkPropositionBinaryOp(.fst, left: mkPattern(name: "P"), right: mkPattern(name: "Q")),
                                   inputSpecifications: [InputSpecification(name: "Q", description: "Any expression", type: .expression)])

let sndDefinition_co = RewriteRule(name: "snd definition", description: "snd(p,q) => q",
                                   leftPattern: mkPropositionBinaryOp(.snd, left: mkPattern(name: "P"), right: mkPattern(name: "Q")),
                                   rightPattern: mkPattern(name: "Q"))

let sndDefinition_ex = RewriteRule(name: "snd definition", description: "q => snd(p,q)",
                                   leftPattern: mkPattern(name: "Q"),
                                   rightPattern: mkPropositionBinaryOp(.snd, left: mkPattern(name: "P"), right: mkPattern(name: "Q")),
                                   inputSpecifications: [InputSpecification(name: "P", description: "Any expression", type: .expression)])

// ---------------------




