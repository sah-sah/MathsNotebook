//
//  Parser.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 5/11/2023.
//

import Foundation

/*
 There do not seem to be any up to date parsing libraries for Swift
 This is a simplified Parsec-like parsing library using Optionals as a Maybe monad
 If we need error information, we can create a struct ParseResult<T> which
 has property of type T, other properties that describe the error, and an
 enum that specifies whether there is an error or not
 */

typealias ParseResult<T> = (T, Substring)?
typealias Parser<T> = (Substring) -> ParseResult<T>

// ---------- MathsNotebook Parsing Functions ----------
// TODO: need to account for whitespace
// TODO: assume all whitespace is leading
// TODO: I think only the primitive parsers need to consume whitespace
// TODO: including literal, binaryOp
// TZZD: only need to consume whitespace for parsers that use literal, letter etc and any other
// parser that directly consumes the string?

// a var is a letter followed by zero or more alphanumerics
func name(input: Substring) -> ParseResult<Expression> {
    seq3(spaces(), letter(), many(alphanumeric()), with: { _,h,tail in
        let str = ([h] + tail).joined()
        if str == "T" { return mkPropositionBool(value: true) }
        if str == "F" { return mkPropositionBool(value: false) }
        return mkVariable(named: str)
    })(input)
}

func bracketP(input: Substring) -> ParseResult<Expression> {
    let lBrac = seq(spaces(), literal("("), with: { _,_ in () })
    let rBrac = seq(spaces(), literal(")"), with: { _,_ in () })
    return seq3(lBrac, expressionP, rBrac, with: { _,exp,_ in exp })(input)
    //seq(seq(literal("("), expressionP, with: snd), literal(")"), with: fst)(input)
}

// \<name>{<exp>}{<exp>}
// this is not working for some reason, the braces aren't parsing
func sysFn(input: Substring) -> ParseResult<Expression> {
    let backslash = seq(spaces(), literal("\\"), with: { _,_ in () })
    let name = seq(letter(), many(alphanumeric()), with: { h,tail in ([h] + tail).joined() })
    
    let lBrace = seq(spaces(), literal("{"), with: { _,_ in () })
    let rBrace = seq(spaces(), literal("}"), with: { _,_ in () })
    let args = seq3(lBrace, expressionP, rBrace, with: { _,exp,_ in exp})
    
    return seq3(backslash, name, many(args), with: { _,n,args in mkSysname(name: n, args: args)})(input)
}

// TODO: this should not use mkSysname
func function(input: Substring) -> ParseResult<Expression> {
    // <name>(<exp>,<exp>,...)
    let name = seq3(spaces(), letter(), many(alphanumeric()), with: { _,h,tail in ([h] + tail).joined() })
    let lBrac = literal("(")
    let rBrac = seq(spaces(), literal(")"), with: { _,_ in () })
    let arg = seq3(spaces(), literal(","), expressionP, with: { _,_,exp in exp })
    let args = seq(expressionP, many(arg), with: { h,tail in [h]+tail })
    return seq4(name, lBrac, args, rBrac, with: { n,_,es,_ in mkPredicateFunction(named: n, args: es)})(input)
}

func predicateP(input: Substring) -> ParseResult<Expression> {
    // forall|exists <exp> s.t. <exp>, <exp>
    // forall|exists <exp>, <exp>
    let pred = seq(spaces(), either(literal("forall"), literal("exists")), with: { _,p in p })
    let st = seq3(spaces(), literal("s.t."), expressionP, with: { _,_,exp in exp })
    let fn = seq3(spaces(), literal(","), expressionP, with: { _,_,exp in exp })
    return seq4(pred, expressionP, opt(st, with: { exp in exp ?? mkEmpty() }), fn, with: { p,v,stExp,exp in
        if p == "forall" {
            return mkPredicate(forall: v, st: stExp, in: exp)
        } else {
            return mkPredicate(exists: v, st: stExp, in: exp)
        }
    } )(input)
}

//var prim_: Parser<String> = either(letter_,bracket_)
//var bracket_: Parser<String> = seq(literal("("), prim_, with: snd)

func notP(input: Substring) -> ParseResult<Expression> {
    seq3(spaces(), literal("~"), primitiveP, with: { _,_,exp in return mkPropositionNot(expression: exp)})(input)
//    seq(literal("~"), primitiveP, with: { _, exp in
//        return mkPropositionNot(expression: exp)
//    })(input)
}

func primitiveP(input: Substring) -> ParseResult<Expression> {
    // function, sysFn
    eitherL([predicateP, function, name, bracketP, notP, sysFn])(input)
}

func binaryOp(string: String) -> Parser<String> {
    //seq(seq(spaces(), literal(string), with: const(string)), spaces(), with: fst)
    seq(spaces(), literal(string), with: { _,_ in string })
    //transform(literal(string), by: { _ in string })
}

func andorP(input: Substring) -> ParseResult<Expression> {
    let lhs = primitiveP
    let op = either(binaryOp(string: "&"), binaryOp(string: "|"))
    let rhs = seq(op, primitiveP, with: pair )
    return seq(lhs, many(rhs), with: { l, rhs in
        var exp = l
        if rhs.count > 0 {
            for ix in 0..<rhs.count {
                if rhs[ix].0 == "&" {
                    exp = mkPropositionBinaryOp(.and, left: exp, right: rhs[ix].1)
                } else {
                    exp = mkPropositionBinaryOp(.or, left: exp, right: rhs[ix].1)
                }
            }
        }
        return exp
    })(input)
}

// TODO: where does = go?
func impP(input: Substring) -> ParseResult<Expression> {
    let lhs = andorP
    let op = either(binaryOp(string: "->"), binaryOp(string: "<-"))
    let rhs = seq(op, andorP, with: pair)
    return seq(lhs, many(rhs), with: { l, rhs in
        var exp = l
        if rhs.count > 0 {
            for ix in 0..<rhs.count {
                // TODO: we should be able to do for (op, rexp) in rhs
                if rhs[ix].0 == "->" {
                    exp = mkPropositionBinaryOp(.imp, left: exp, right: rhs[ix].1)
                } else {
                    exp = mkPropositionBinaryOp(.revImp, left: exp, right: rhs[ix].1)
                }
            }
        }
        return exp
    })(input)
}

// TODO: abstract out the lhs (op rhs)* pattern
func equivP(input: Substring) -> ParseResult<Expression> {
    let lhs = impP
    let op = binaryOp(string: "<->")
    let rhs = seq(op, impP, with: pair)
    return seq(lhs, many(rhs), with: { l, rhs in
        var exp = l
        if rhs.count > 0 {
            for ix in 0..<rhs.count {
                // TODO: we should be able to do for (op, rexp) in rhs
                exp = mkPropositionBinaryOp(.iff, left: exp, right: rhs[ix].1)
            }
        }
        return exp
    })(input)
}

// recursive top-level expression (doesn not expect end of input)
func expressionP(input: Substring) -> ParseResult<Expression> { equivP(input: input) }

// parse the entire input (an expression followed by eoi)
func expressionTotal(input: Substring) -> ParseResult<Expression> { seq3(equivP, spaces(), eoi(), with: { exp,_,_ in exp })(input) }

// something like this?
/*
func runParser<T>(_ parser: Parser<T>, on input: Substring) -> T? {
    lazy var p = parser
    if let (res, _) = p(input) {
        return res
    }
    return nil
}
 */

// but what we want to write is
/*
 we could use @resultBuilder to use syntax more like
 combining views, but it is not really necessary at this stage
 andP = seq {
 primitiveP
 many {
   seq { 
    literal("&")
    primitiveP
 
  }
 
 seq(parserA, parserB) with: { resA, resB in return resB }
 
 
 */



// ---------- Specific Parsing Functions ----------

func letter() -> Parser<String> { return first(if: isLetter, map: id) }
var letter_: Parser<String> = first(if: isLetter, map: id)
func space() -> Parser<String> { return first(if: isWhitespace, map: id) }
func spaces() -> Parser<String> { transform(many(space()), by: { l in l.joined() }) }
func alphanumeric() -> Parser<String> { return first(if: isAlphanumeric, map: id) }

func isLetter(_ str: String) -> Bool {
    if let us = Unicode.Scalar(str) {
        return CharacterSet.letters.contains(us)
    }
    return false
}

func isAlphanumeric(_ str: String) -> Bool {
    if let us = Unicode.Scalar(str) {
        return CharacterSet.alphanumerics.contains(us)
    }
    return false
}

func isWhitespace(_ str: String) -> Bool {
    if let us = Unicode.Scalar(str) {
        return CharacterSet.whitespaces.contains(us)
    }
    return false
}

func pair<S,T>(_ f: S, _ s: T) -> (S,T) { (f,s) }
func fst<S,T>(_ f: S, _ s: T) -> S { f }
func snd<S,T>(_ f: S, _ s: T) -> T { s }
func disc<S,T>(_ f: S, _ s: T) -> Void { () }
func const<S,T,R>(_ val: S) -> ((T,R) -> S) {
    return { _,_ in val }
}

// ---------- General Parsing Functions ----------

// NOTE: these functions should generally not return a closure
// as recursively defined closures will generate an infinite loop
// A closure should only be returned if it will not rely on another closure


func id<T>(_ x: T) -> T { return x }

func head(_ str: String) -> Character { return str[str.startIndex] }

// tries to take n characters off of sstr, returns nil if it can't
func take(_ n: Int, input sstr:Substring) -> ParseResult<String> {
    if sstr.count >= n {
        let st = sstr.startIndex
        let fn = sstr.index(st, offsetBy: n)
        return (String(sstr[st..<fn]), sstr[fn..<sstr.endIndex])
    } else {
        return nil
    }
}

// parses the first character of the input if it meets the given condition
func first<T>(if check: @escaping (String) -> Bool, map: @escaping (String) -> T = id) -> Parser<T> {
    return { sstr in
        if let (ch,sstr_) = take(1, input:sstr) {
            if check(ch) {
                return (map(ch), sstr_)
            }
        }
        return nil
    }
}

func first<T>(unless check: @escaping (String) -> Bool, map: @escaping (String) -> T = id) -> Parser<T> {
    return { sstr in
        if let (ch,sstr_) = take(1, input:sstr) {
            if !check(ch) {
                return (map(ch), sstr_)
            }
        }
        return nil
    }
}

func literal(_ string: String) -> Parser<String> {
    return { sstr in
        if let (pf,sstr_) = take(string.count, input: sstr) {
            if pf == string {
                return (string, sstr_)
            }
        }
        return nil
    }
}

// parse many times
func many<T>(_ parser: @escaping Parser<T>, minCount: Int = 0) -> Parser<[T]> {
    return { sstr in
        var allres: [T] = []
        var input: Substring = sstr
        var cont = true
        
        repeat {
            if let (res, rem) = parser(input) {
                allres.append(res)
                input = rem
            } else {
                cont = false
            }
        } while cont
        
        if allres.count >= minCount {
            return (allres, input)
        } else { return nil }
    }
}

// one of two parsers, output is first to return
func either<T>(_ parserA: @escaping Parser<T>, _ parserB: @escaping Parser<T>) -> Parser<T> {
    return { sstr in
        if let res = parserA(sstr) {
            return res
        } else {
            return parserB(sstr)
        }
    }
}

func eitherL<T>(_ parsers: [Parser<T>]) -> Parser<T> {
    return { sstr in
        for ix in 0..<parsers.count {
            if let (res, rem) = parsers[ix](sstr) {
                return (res, rem)
            }
        }
        return nil
    }
}

// run two parsers sequentially, combining the output
func seq<T,S,R>(_ parserA: @escaping Parser<T>, _ parserB: @escaping Parser<S>, with comb: @escaping (T,S) -> R) -> Parser<R> {
    return { sstr in
        if let (resA,sstr_) = parserA(sstr) {
            if let (resB,sstr__) = parserB(sstr_) {
                return (comb(resA, resB), sstr__)
            }
        }
        return nil
    }
}

func seq3<T0,T1,T2,S>(_ parserA: @escaping Parser<T0>, _ parserB: @escaping Parser<T1>, _ parserC: @escaping Parser<T2>, with comb: @escaping (T0,T1,T2) -> S) -> Parser<S> {
    return { sstr in
        if let (resA,sstr_) = parserA(sstr) {
            if let (resB, sstr__) = parserB(sstr_) {
                if let (resC, sstr___) = parserC(sstr__) {
                    return (comb(resA,resB,resC), sstr___)
                }
            }
        }
        return nil
    }
}

func seq4<T0,T1,T2,T3,S>(_ parserA: @escaping Parser<T0>, _ parserB: @escaping Parser<T1>, _ parserC: @escaping Parser<T2>, _ parserD: @escaping Parser<T3>, with comb: @escaping (T0,T1,T2,T3) -> S) -> Parser<S> {
    return { sstr in
        if let (resA,sstr_) = parserA(sstr) {
            if let (resB, sstr__) = parserB(sstr_) {
                if let (resC, sstr___) = parserC(sstr__) {
                    if let (resD, sstr____) = parserD(sstr___) {
                        return (comb(resA,resB,resC,resD), sstr____)
                    }
                }
            }
        }
        return nil
    }
}

// TODO: rewrite everything like this
func seq_<S,T,R>(_ parserA: Parser<S>, _ parserB: Parser<T>, with comb: (S,T) -> R, at input: Substring) -> (R, Substring)? {
   if let (resA, remA) = parserA(input) {
     if let (resB, remB) = parserB(remA) {
        return (comb(resA, resB), remB)
     }
   }
 return nil
 }

// transform the output of the parser
func transform<T,S>(_ parser: @escaping Parser<T>, by fn: @escaping (T) -> S) -> Parser<S> {
    return { sstr in
        if let (res,rem) = parser(sstr) {
            return (fn(res),rem)
        }
        return nil
    }
}

// end of input
func eoi() -> Parser<Void> {
    return { sstr in
        if sstr.count == 0 {
            return ((), sstr)
        }
        return nil
    }
}

// optionally parse
func opt<T>(_ parser: @escaping Parser<T>, with fn: @escaping (T?) -> T) -> Parser<T> {
    return { sstr in
        if let (res, rem) = parser(sstr) {
            return (fn(res), rem)
        }
        return (fn(nil), sstr)
    }
}

// succeed without consuming input
func pass<T>(def: T) -> Parser<T> {
    return { sstr in return (def, sstr) }
}

// fail without consuming input
func fail<T>(def: T) -> Parser<T> {
    return { sstr in return nil }
}

/*
func transform<T,S>(_ parser: @escaping Parser<T>, by fn: @escaping (T) -> S) -> Parser<S> {
    return { sstr in
        if let (res,rem) = parser(sstr) {
            return (fn(res),rem)
        }
        return nil
    }
}
 */

/*
 parsing code looks like
 let simple_var = mapP(follows(first(...), many(...)), mkVariable)
 let complex_var = follows(follows(simple_var, literal("_"), fst), expression), mkIndexedVariable)
 
 let complex_var = { sstr in
   if let (sv,inputa) = simple_var(sstr) {
     if let (_,inputb) = literal("_") {
       if let (exp,inputc) = expression(inputb) {
          return (mkIndexedVariable(sv, exp), inputc)
       }
     }
   } else {
     return nil
 }
 
 */

/*
// it works!
func andP() -> Parser<Expression> {
    return { sstr in
        if let (res, sstr_) = primitiveP()(sstr) {
            if let (rhs, sstr__) = many(seq(binaryOp(string: "&"), primitiveP(), with: { h, tail in (h,tail) }))(sstr_) {
                // construct the binary op expressions
                if rhs.count == 0 { return (res,sstr__) }
                else {
                    var exp = res
                    for ix in 0..<rhs.count {
                        exp = mkPropositionBinaryOp(.and, left: exp, right: rhs[ix].1)
                    }
                    return (exp,sstr__)
                }
            }
        }
        return nil
    }
}

func andP_() -> Parser<Expression> {
    let lhs: Parser<Expression> = primitiveP()
    let rhs: Parser<Expression> = seq(binaryOp(string: "&"), primitiveP(), with: { h, tail in tail })
    return seq(lhs, many(rhs), with: { l, rs in
        var exp = l
        if rs.count > 0 {
            for ix in 0..<rs.count {
                exp = mkPropositionBinaryOp(.and, left: exp, right: rs[ix])
            }
        }
        return exp
    })
}
 */

/*
func seq<T,S,R>(_ parserA: @escaping Parser<T>, _ parserB: @escaping Parser<S>, with comb: @escaping (T,S) -> R) -> Parser<R> {
    return { sstr in
        if let (resA,sstr_) = parserA(sstr) {
            if let (resB,sstr__) = parserB(sstr_) {
                return (comb(resA,resB), sstr__)
            }
        }
        return nil
    }
}
*/
