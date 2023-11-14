//
//  ProofsDeductions.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 30/10/2023.
//

import Foundation

extension Proof {
    // transforms

    func transform(resultId: ResultId, by rule: RewriteRule, with b: ExpressionBinding, in proofId: ProofId) {
        // TODO: need to check proofId is in the hierarchy of ResultId
        if let res = getResult(forId: resultId) {
            // try to apply at cursor
            if let exp = res.getCursor() {
                // apply the rewrite rule
                if let n_exp = rule.apply(to: [exp], with:b) {
                    if let n_res_exp = res.replaceAtCursor(with: n_exp[0]) {
                        record(expressions: [n_res_exp], as: ProofStep(type: .transform, source: [resultId], equivalence: true), at: proofId)
                    } else { print("Unable to replace at cursor")}
                } else { print("Unable to apply rule")}
            } else { print("Unable to get cursor")}
        } else { print("Unable to get result")}
    }
    
    func checkTransform(for rule: RewriteRule, at resultId: ResultId) -> Bool {
        if let res = getResult(forId: resultId) {
            // check at cursor
            if let exp = res.getCursor() {
                return rule.canApply(to: [exp])
            }
        }
        // else
        return false
    }

}

/*
// TODO: some of these need to take a parameter
// TODO: e.g. replace F with p && F for any p, replace T with p || T for any p
struct RewriteRule {
    var name: String
    var leftPattern: Expression
    var rightPattern: Expression
    
    func apply(to exp: Expression) -> Expression? {
        if let b = makeBinding(to: leftPattern, with: exp) {
            // there is a binding
            if isConsistent(binding: b) {
                return MathsNotebook.apply(binding: b, to: rightPattern)
            } // else, binding is inconsistent
        } else { print("Failed to make binding")}
        // else, there is no binding
        return nil
    }
    
    func canApply(to exp: Expression) -> Bool {
        if let b = makeBinding(to: leftPattern, with: exp) {
            // there is a binding
            return isConsistent(binding: b)
        }
        return false
    }
}

func reversed(rewriteRule: RewriteRule, named:String) -> RewriteRule {
    return RewriteRule(name: named, leftPattern: rewriteRule.rightPattern, rightPattern: rewriteRule.leftPattern)
}
*/

// The deduction structs should be as small as possible and name and description and checking whether
// they apply to make it easier to use them
/*
protocol Deduction: Identifiable {
    var id: UUID { get }
    var name: String { get }
    var description: String { get }
    var inputSpecifications: [InputSpecification]? { get }
    
    // accepts a list of expressions and tries to apply the rule
    func apply(to exps : [Expression], with b:ExpressionBinding) -> [Expression]?
    
    // does this deduction apply?
    func canApply(to exps: [Expression]) -> Bool
}
 */

// Specifying inputs to proof steps
struct InputSpecification {
    enum InputType {
        case expression, string, int, double
    }
    
    var name: String
    var description: String
    var type: InputType
    // assigning a value to the input
    var val: Any? = nil
}

// TODO: where should we try to match e.g. if we have (not p and p)
// TODO: and we want to use the rule (p and not p => F)
// TODO: especially if we filter, need at least one selection method where there is no filtering?
// TODO: when matching, generate equivalent expressions using rewrite rules, then match against any
// TODO: We need separate RewriteRules and Deductions as RewriteRules apply at the cursor and
// TODO: Deductions apply at the level of results
struct RewriteRule: Hashable, Equatable, Identifiable {
    var id: UUID = UUID()
    var name: String
    var description: String
    var leftPattern: Expression
    var rightPattern: Expression
    var inputSpecifications: [InputSpecification]? = nil
    
    static func == (lhs: RewriteRule, rhs: RewriteRule) -> Bool {
        return lhs.id == rhs.id
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(id)
    }
    
    func apply(to exps: [Expression], with b: ExpressionBinding) -> [Expression]? {
        // a rewrite rule applies to one expression only
        if exps.count != 1 { return nil }
        // check if we can bind to the left pattern
        if let lb = makeBinding(to: leftPattern, with: exps[0]) {
            // union the bindings
            let ab = unionBinding(lb,b)
            // if the binding is consistent
            if isConsistent(binding: ab) {
                if let res = MathsNotebook.apply(binding: ab, to: rightPattern) {
                    return [res]
                }
            }
        }
        // no applicable rule found
        return nil
    }
    
    func canApply(to exps: [Expression]) -> Bool {
        // a rewrite rule applies to one expression only
        if exps.count != 1 { return false }
        // check if any of the rules apply
        if let b = makeBinding(to: leftPattern, with: exps[0]) {
            // there is a binding
            if isConsistent(binding: b) { return true }
        }
        return false
    }
}
