//
//  InputView.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 5/11/2023.
//

import SwiftUI
import Parsing

struct InputView: View {
    @State var expInput: String = ""
    @State var expName: String = ""
    @State var expParsed: String = ""
    @State var inputExp: Expression? = nil
    @ObservedObject var proofSession: ProofSession
    
    var body: some View {
        VStack {
            InputWebView(proofSession: proofSession)
            HStack {
                TextField("Expression name:", text:$expName)
                TextField("Expression input:", text:$expInput)
            }
            HStack {
                Button("Parse input") {
                    // debug
                    //let lBrace = seq(spaces(), literal("{"), with: { _,_ in () })
                    //let rBrace = seq(spaces(), literal("}"), with: { _,_ in () })
                    //let args = seq3(lBrace, expressionP, rBrace, with: { _,exp,_ in exp})
                    //let testStr = "p}"
                    //print(expressionP(input: testStr[...]))
                    
                    // try to parse input
                    let input = expInput[...]
                    if let (exp,_) = expressionTotal(input: input) {
                    //if let (exp,_) = sysFn(input: input) {
                        print("Parsed to: \(proofSession.proof.context.printer.pretty(expression: exp, as: PrinterType.pretty.rawValue))")
                        print("Parsed expr has \(exp.subexpressions.count) args")
                        // we have parsed the expression, now resolve any system names
                        if let rexp = proofSession.proof.context.resolveSystemNames(for: exp) {
                            inputExp = rexp
                            let (expStr,_) = proofSession.proof.context.printer.pretty(expression: rexp, as: PrinterType.pretty.rawValue)
                            expParsed = expStr
                        }
                    } else {
                        //print("No parse for \(expInput)")
                        expParsed = "no parse"
                    }
                }
                Button("Save input") {
                    // save input if it exists
                    if let exp = inputExp {
                        if expName.count > 0 && proofSession.savedExpressions[expName] == nil  {
                            proofSession.savedExpressions[expName] = exp
                            expName = ""
                            inputExp = nil
                            expInput = ""
                            expParsed = ""
                        }
                    }
                }
            }
            Text(expParsed)
        }
    }
}


/*
 Parsec like parsers for expressions
 
 General form:
 a parser is a function that takes a SubString and returns a result and a SubString (the remainder)
 At the top level, a result will be an Expression, at the intermediate level it will be and Int/Bool/nothing etc
 parser: SubString -> (Expression, SubString)
 How do we implement failure? etc
 Need a way to combine parsers
 
 Take variable parser: a variable name is a letter followed by a zero or more alphanumerics
 
 A parser is a struct? Then we have to use generics?
 
 typealias Parser<T> = SubString -> (T, SubString)?
 
 func follows(first: Parser<T>, second: Parser<S>, with comb: (<T>,<S>) -> <R>) -> Parser<R> {
    if let (res_f, str) = first(input) {
        if let (res_s, stra) = second(str) {
            return comb(res_f, res_s)
        }
    }
 return nil
 }
 
 
 */
