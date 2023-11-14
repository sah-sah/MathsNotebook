//
//  ExpressionPrinters.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 28/10/2023.
//

import Foundation

// TODO: I think we need to redesign this
// Keep it simpler, make them part of the proof context?
// For every expression type there will be various printers (basic, pretty, mathml, latex)
// Can we store them as part of as Expression struct? Not every struct needs them?
// Have a separate object that stores them? Then that object can store the printing general printing functions as well
// We only need one of these objects

// Precedence: higher value means less need for brackets, if brackets are never needed use 100

// printing
typealias Printer = (Expression, [String]) -> String
// printing with precedence
typealias PrettyPrinter = (Expression, [(String,Int)]) -> (String,Int)

enum PrinterType: String {
    case basic = "BasicString"
    case pretty = "PrettyString"
    case mathml = "PrettyMathML"
}


struct ExpressionPrinter {
    
    struct TypeName: Hashable {
        var type: String
        var name: String
    }
    
    var printers: Dictionary<TypeName, Printer> = [:]
    var prettyPrinters: Dictionary<TypeName, PrettyPrinter> = [:]
    var cursorFns: Dictionary<String, (String) -> String> = [:]
    
    init() {
        // register standard cursor functions
        cursorFns[PrinterType.basic.rawValue] = ExpressionPrinter.stdCursorFn
        cursorFns[PrinterType.pretty.rawValue] = ExpressionPrinter.stdCursorFn
        cursorFns[PrinterType.mathml.rawValue] = ExpressionPrinter.mathmlCursorFn
    }
    
    // register a printer
    mutating func register(printFn: @escaping Printer, for name: String, ofType: String) {
        printers[TypeName(type: ofType, name: name)] = printFn
    }
    
    // register a pretty printer
    mutating func register(prettyPrintFn: @escaping PrettyPrinter, for name: String, ofType: String) {
        prettyPrinters[TypeName(type: ofType, name: name)] = prettyPrintFn
    }
    
    static func stdCursorFn(_ strExp: String) -> String {
        return "[\(strExp)]"
    }
    
    static func mathmlCursorFn(_ strExp: String) -> String {
        return "<mo>[</mo>\(strExp)<mo>]</mo>"
    }
    
    // a basic printer
    func print(expression exp: Expression, as type: String) -> String {
        // recursively call subexpressions
        var subexps_str: [String] = []
        subexps_str = exp.subexpressions.map { print(expression: $0, as: type) }
        // lookup
        if let printer = printers[TypeName(type: type, name: exp.name)] {
            return printer(exp, subexps_str)
        } else {
            // use default printer
            return Expression.basic(print: exp, with: subexps_str)
        }
    }
    
    // a basic printer with cursor
    func print(expression exp: Expression, withCursor: [CursorDirection]?, as type: String) -> String {
        if let cds = withCursor {
            if cds.count == 0 {
                // we are at the cursor
                if let cursorFn = cursorFns[type] {
                    return cursorFn(print(expression: exp, as: type))
                } else {
                    // standard cursor function
                    return ExpressionPrinter.stdCursorFn(print(expression: exp, as: type))
                }
            } else {
                // we are not at the cursor
                // check there are sub expressions
                var subexps_str = exp.subexpressions.map { print(expression: $0, as: type) }
                // the cursor is in one of the subexpressions
                let d = cds[0].rawValue
                if d >= 0 && d < exp.subexpressions.count {
                    // a valid direction
                    subexps_str[d] = print(expression: exp.subexpressions[d], withCursor: Array(cds[1..<cds.count]), as:type)
                    // get printer
                    if let printer = printers[TypeName(type: type, name: exp.name)] {
                        return printer(exp, subexps_str)
                    } else {
                        // use default printer
                        return Expression.basic(print: exp, with: subexps_str)
                    }
                } else {
                    return "Invalid direction"
                }
            }
        } else {
            // print with out cursor
            return print(expression: exp, as: type)
        }
    }
    
    // a pretty printer
    // TODO: precedence is a number between 1 and 100
    func pretty(expression exp: Expression, as type: String) -> (String,Int) {
        // recursively call subexpressions
        let subexpsStrPrecs = exp.subexpressions.map { pretty(expression: $0, as: type) }
        // lookup
        if let printer = prettyPrinters[TypeName(type: type, name: exp.name)] {
            return printer(exp, subexpsStrPrecs)
        } else {
            // use default printer
            let justStrs = subexpsStrPrecs.map { $0.0 }
            // TODO: is the default precedence correct
            return (Expression.basic(print: exp, with: justStrs),100)
        }
    }
    
    // a pretty printer with a cursor
    func pretty(expression exp: Expression, withCursor: [CursorDirection]?, as type: String) -> (String, Int) {
        if let cds = withCursor {
            if cds.count == 0 {
                // print the rest without a cursor
                let (prettyStr, _) = pretty(expression: exp, as: type)
                // we are at the cursor
                if let cursorFn = cursorFns[type] {
                    // TODO: what should the precedence of the cursor be
                    return (cursorFn(prettyStr), 100)
                } else {
                    // standard cursor function
                    return (ExpressionPrinter.stdCursorFn(prettyStr), 100)
                }
            } else {
                // we are not at the cursor
                // check there are sub expressions
                var subexpsStrInt = exp.subexpressions.map { pretty(expression: $0, as: type) }
                // the cursor is in one of the subexpressions
                let d = cds[0].rawValue
                if d >= 0 && d < exp.subexpressions.count {
                    // a valid direction
                    subexpsStrInt[d] = pretty(expression: exp.subexpressions[d], withCursor: Array(cds[1..<cds.count]), as:type)
                    // get printer
                    if let printer = prettyPrinters[TypeName(type: type, name: exp.name)] {
                        return printer(exp, subexpsStrInt)
                    } else {
                        // use default printer
                        return (Expression.basic(print: exp, with: subexpsStrInt.map { $0.0 } ), 100)
                    }
                } else {
                    return ("Invalid direction", 0)
                }
            }
        } else {
            // print with out cursor
            return pretty(expression: exp, as: type)
        }
    }
}

/*
 a b c d e
 we have:
  for a: b + c = d + e or b + d = c + e or b + e = c + d
  for b: a + c = d + e or a + d = c + e or a + e = c + d
  for c: a + b = d + e or a + d = b + e or a + e = b + d
  for d: a + b = c + e or a + c = b + e or a + e = b + c
  for e: a + b = c + d or a + c = b + d or a + d = b + c
 
  let's take:
  (1) b + c = d + e, (2) a + c = d + e, (3) a + b = d + e, (4) a + b = c + e, (5) a + b = c + d
 by (4), (5) we have d = e
 rewrite
 (1) b + c = 2d, (2) a + c = 2d, (3) a+b = 2d, (4)=(5) a + b = c + d
 From (1), (2) we have a = b
 rewrite
 (1)=(2) a + c = 2d, (3) 2a = 2d, (4)=(5) 2a = c + d
 From (3), a = d (= e = b)
 rewrite
 (1)=(2) c = d, ...
 Done
 In general, can we be sure exists 2 equations that differ by one variable, then reduce
 */
