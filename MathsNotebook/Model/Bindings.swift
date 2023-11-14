//
//  Bindings.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 30/10/2023.
//

import Foundation

// TODO: should we make bindings a struct?
typealias ExpressionBinding = Dictionary<String, [Expression]>

let emptyBinding: ExpressionBinding = [:]

func makeBinding(to form: Expression, with exp: Expression, updating binding: ExpressionBinding? = nil) -> ExpressionBinding? {
    
    func makeBindingAux(to form_aux: Expression, with exp_aux: Expression, updating b: inout ExpressionBinding) -> Bool {
        
        if form_aux.type == .pattern {
            // we have reached a pattern, add it to the binding
            if var bExps = b[form_aux.value] {
                bExps.append(exp_aux)
            } else {
                b[form_aux.value] = [exp_aux]
            }
            // binding succeeded
            return true
        } else {
            if Expression.match(lhs:form_aux, rhs:exp_aux) {
                // if the node matches, then recursively match on the sub-expressions
                // note: match should confirm the same number of sub-expressions
                for ix in 0..<form_aux.subexpressions.count {
                    let bound = makeBindingAux(to: form_aux.subexpressions[ix], with: exp_aux.subexpressions[ix], updating: &b)
                    if !bound { return false } // binding failed
                }
                return true   // binding succeeded
            } else { return false } // match at node failed
        }
    }
        
    var b: ExpressionBinding = binding == nil ? [:] : binding!
        
    if !makeBindingAux(to: form, with: exp, updating: &b) { return nil }
    else { return b }
}

func unionBinding(_ bindingA: ExpressionBinding, _ bindingB: ExpressionBinding) -> ExpressionBinding {
    var b: ExpressionBinding = [:]
    for (n, exps) in bindingA {
        if var b_exps = b[n] {
            b_exps += exps
        } else {
            b[n] = exps
        }
    }
    for (n, exps) in bindingB {
        if var b_exps = b[n] {
            b_exps += exps
        } else {
            b[n] = exps
        }
    }
    return b
}

// doesn't check the binding is consistent
func willBind(to form: Expression, with exp: Expression) -> Bool {
    // always bind to a pattern
    if form.type == .pattern { return true }
    // otherwise, check nodes match
    if Expression.match(lhs: form, rhs: exp) {
        // check sub expressions
        for ix in 0..<form.subexpressions.count {
            if !willBind(to: form.subexpressions[ix], with: exp.subexpressions[ix]) {
                return false
            }
        }
        // we have matched the sub expressions
        return true
    } else { return false }
}

func apply(binding: ExpressionBinding, to exp: Expression) -> Expression? {
    if exp.type == .pattern {
        // return expression from binding
        if let b_exp = binding[exp.value] {
            // we assume the binding is consistent
            return b_exp[0]
        } else { return nil }
    } else {
        var n_exp = exp
        // bind to the sub expressions
        for ix in 0..<n_exp.subexpressions.count {
            if let n_sub_exp = apply(binding: binding, to: n_exp.subexpressions[ix]) {
                n_exp.setSubexpression(at: ix, to: n_sub_exp)
            } else { return nil }
        }
        return n_exp
    }
}

func isConsistent(binding: ExpressionBinding) -> Bool {
    for (_, exps) in binding {
        if !exps.allSatisfy({ $0 == exps[0] }) {
            return false
        }
    }
    return true
}

