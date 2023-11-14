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


// printing
typealias Printer = (Expression, [String]) -> String

enum PrinterType: String {
    case basic = "Basic"
    case mathml = "MathML"
}

// might need a protocol ExpressionPrinter
// which has a function print: Expression -> String

protocol ExpressionPrinter {
    // registering a printing function for an expression
    mutating func register(printer: @escaping Printer, for name: String)
    // printing an expression
    func print(expression exp: Expression) -> String
    // printing with a cursor
    func print(expression exp: Expression, withCursor: [CursorDirection]?) -> String
}

// TODO: define MathMLPrinter: ExpressionPrinter
// TODO: this is basic as it always uses brackets
struct BasicPrinter: ExpressionPrinter {
    var printing_dict: Dictionary<String, Printer>
    var cursorFn: (String) -> String
    
    // start with an empty dictionary
    init() {
        self.printing_dict = [:]
        //self.cursorFn = { str in "[\(str)]"}
        // mathml cursor function
        self.cursorFn = { str in "<mo>[</mo>\(str)<mo>]</mo>"}
    }
    
    // register a printer
    mutating func register(printer: @escaping Printer, for name: String) {
        printing_dict[name] = printer
    }
    
    mutating func setCursorFn(cursorFn: @escaping (String) -> String) {
        self.cursorFn = cursorFn
    }
    
    func print(expression exp: Expression) -> String {
        // recursively call subexpressions
        var subexps_str: [String] = []
        subexps_str = exp.subexpressions.map { print(expression: $0) }
        // lookup
        if let printer = printing_dict[exp.name] {
            return printer(exp, subexps_str)
        } else {
            // use default printer
            return Expression.basic(print: exp, with: subexps_str)
        }
    }
    
    func print(expression exp: Expression, withCursor: [CursorDirection]?) -> String {
        if let cds = withCursor {
            if cds.count == 0 {
                // we are at the cursor
                return cursorFn(print(expression: exp))
            } else {
                // check there are sub expressions
                var subexps_str: [String] = []
                subexps_str = exp.subexpressions.map { print(expression: $0) }
                // the cursor is in one of the subexpressions
                let d = cds[0].rawValue
                if d >= 0 && d < exp.subexpressions.count {
                    // a valid direction
                    subexps_str[d] = print(expression: exp.subexpressions[d], withCursor: Array(cds[1..<cds.count]))
                    // get printer
                    if let printer = printing_dict[exp.name] {
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
            return print(expression: exp)
        }
    }
}
