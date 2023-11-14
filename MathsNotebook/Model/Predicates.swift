//
//  Predicates.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 11/11/2023.
//

import Foundation

// ---------- Predicate Expression functions ---------- //

/*
func mkPropositionBinaryOp(_ op: PropositionBinaryOp, left: Expression, right: Expression) -> Expression {
    return Expression(type: ExpressionType.expression, name: PropositionNames.binaryOp.rawValue, value: "\(op)", subexpressions: [left, right])
}
 */

enum PredicateNames: String {
    case exists = "Predicates.Exists"
    case forall = "Predicates.Forall"
    case equals = "Predicates.Equals"
    case function = "Predicates.Function"
}

func mkPredicate(exists: Expression, st: Expression, in exp: Expression) -> Expression {
    return Expression(type: ExpressionType.expression, name: PredicateNames.exists.rawValue, value: "exists", subexpressions: [exists, st, exp])
}

func mkPredicate(forall: Expression, st: Expression, in exp: Expression) -> Expression {
    return Expression(type: ExpressionType.expression, name: PredicateNames.forall.rawValue, value: "forall", subexpressions: [forall, st, exp])
}

func mkPredicateEquals(lhs: Expression, rhs: Expression) -> Expression {
    return Expression(type: ExpressionType.expression, name: PredicateNames.equals.rawValue, value: "equals", subexpressions: [lhs,rhs])
}

func mkPredicateFunction(named: String, args: [Expression]) -> Expression {
    return Expression(type: ExpressionType.expression, name: PredicateNames.function.rawValue, value: named, subexpressions: args)
}

let predicatesSystemNames: [(String, Int, ([Expression]) -> Expression)] = [
    ("exists", 3, { exps in mkPredicate(exists: exps[0], st: exps[1], in: exps[2])}),
    ("forall", 3, { exps in mkPredicate(forall: exps[0], st: exps[1], in: exps[2])}),
    ("equals", 2, { exps in mkPredicateEquals(lhs: exps[0], rhs: exps[1])})]

// ---------- Printing the Expression objects ---------- //

// basic printers
// TODO: do we need brackets
// TODO: parsing
// TODO: deal with whitespace in parser

func print_PredicatesExists(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == PredicateNames.exists.rawValue else { return "Unable to print \(exp.name)" }
    guard subexps.count == 3 else { return "Invalid Expression" }
    // print
    return "exists \(subexps[0]) s.t. \(subexps[1]). \(subexps[2])"
}

func print_PredicatesForall(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == PredicateNames.forall.rawValue else { return "Unable to print \(exp.name)" }
    guard subexps.count == 3 else { return "Invalid Expression" }
    // print
    // Do the sub expressions need brackets?
    return "forall \(subexps[0]) s.t. \(subexps[1]). \(subexps[2])"
}

func print_PredicatesEquals(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == PredicateNames.equals.rawValue else { return "Unable to print \(exp.name)" }
    guard subexps.count == 2 else { return "Invalid Expression" }
    // print
    return "\(subexps[0]) == \(subexps[1])"
}

func print_PredicatesFunction(_ exp: Expression, with subexps: [String]) -> String {
    // check
    guard exp.name == PredicateNames.function.rawValue else { return "Unable to print \(exp.name)" }
    // print
    return "\(exp.value)(\(subexps.joined(separator: ","))"
}

// pretty printers
// TODO: like the basic printers but they return a precedence
func pretty_PredicatesExists(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PredicateNames.exists.rawValue else { return ("Unable to print \(exp.name)", 100) }
    guard subexps.count == 3 else { return ("Invalid Expression", 100) }
    // print
    return ("exists \(subexps[0].0) s.t. \(subexps[1].0). \(subexps[2].0)", 100)
}

func pretty_PredicatesForall(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PredicateNames.forall.rawValue else { return ("Unable to print \(exp.name)", 100) }
    guard subexps.count == 3 else { return ("Invalid Expression", 100) }
    // print
    // Do the sub expressions need brackets?
    return ("forall \(subexps[0].0) s.t. \(subexps[1].0). \(subexps[2].0)", 100)
}

func pretty_PredicatesEquals(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PredicateNames.equals.rawValue else { return ("Unable to print \(exp.name)", 100) }
    guard subexps.count == 2 else { return ("Invalid Expression", 100) }
    // print
    let (lhs, lPrec) = subexps[0]
    let (rhs, rPrec) = subexps[1]
    let left = lPrec > 60 ? lhs : "(\(lhs))"
    let right = rPrec > 60 ? rhs : "(\(rhs))"
    return ("\(left) = \(right)", 60)
}

func pretty_PredicatesFunction(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PredicateNames.function.rawValue else { return ("Unable to print \(exp.name)", 100) }
    // print
    let expStrs = subexps.map({ $0.0 })
    return ("\(exp.value)(\(expStrs.joined(separator: ","))", 100)
}


// mathml printers

// TODO: use the correct symbols
func mathml_PredicatesExists(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PredicateNames.exists.rawValue else { return ("Unable to print \(exp.name)", 100)}
    guard subexps.count == 3 else { return ("Invalid Expression", 100) }
    // mathml
    let (varStr,_) = subexps[0]
    let (stStr,_) = subexps[1]
    let (expStr,_) = subexps[2]
    return (exp.wrapId(mathML: "<mtext>exists </mtext>\(varStr)<mtext>s.t.</mtext>\(stStr)<mo>.</mo>\(expStr)"), 100)
}

func mathml_PredicatesForall(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PredicateNames.forall.rawValue else { return ("Unable to print \(exp.name)", 100)}
    guard subexps.count == 3 else { return ("Invalid Expression", 100) }
    // mathml
    let (varStr,_) = subexps[0]
    let (stStr,_) = subexps[1]
    let (expStr,_) = subexps[2]
    return (exp.wrapId(mathML: "<mtext>forall </mtext>\(varStr)<mtext>s.t.</mtext>\(stStr)<mo>.</mo>\(expStr)"), 100)
}

func mathml_PredicatesEquals(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PredicateNames.equals.rawValue else { return ("Unable to print \(exp.name)", 100)}
    guard subexps.count == 2 else { return ("Invalid Expression", 100) }
    // mathml
    let (lhs, lPrec) = subexps[0]
    let (rhs, rPrec) = subexps[1]
    let lStr = lPrec > 60 ? lhs : "<mo>(</mo>\(lhs)<mo>)</mo>"
    let rStr = rPrec > 60 ? rhs : "<mo>(</mo>\(rhs)<mo>)</mo>"
    return (exp.wrapId(mathML: "\(lStr)<mo>=</mo>\(rStr)"), 60)
}

func mathml_PredicatesFunction(_ exp: Expression, with subexps: [(String,Int)]) -> (String,Int) {
    // check
    guard exp.name == PredicateNames.function.rawValue else { return ("Unable to print \(exp.name)", 100)}
    // mathml
    let args = subexps.map({ s,_ in s}).joined(separator: "<mo>,</mo>")
    return (exp.wrapId(mathML: "<mtext>\(exp.value)</mtext><mo>(</mo>\(args)<mo>)</mo>"), 100)
}


func updatePrinterPredicates(_ printer: inout ExpressionPrinter) {
    // basic printing
    printer.register(printFn:print_PredicatesExists, for:PredicateNames.exists.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_PredicatesForall, for:PredicateNames.forall.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_PredicatesEquals, for:PredicateNames.equals.rawValue, ofType: PrinterType.basic.rawValue)
    printer.register(printFn:print_PredicatesFunction, for:PredicateNames.function.rawValue, ofType: PrinterType.basic.rawValue)
    // pretty printing
    printer.register(prettyPrintFn:pretty_PredicatesExists, for:PredicateNames.exists.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_PredicatesForall, for:PredicateNames.forall.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_PredicatesEquals, for:PredicateNames.equals.rawValue, ofType: PrinterType.pretty.rawValue)
    printer.register(prettyPrintFn:pretty_PredicatesFunction, for:PredicateNames.function.rawValue, ofType: PrinterType.pretty.rawValue)
    // mathml printing
    printer.register(prettyPrintFn:mathml_PredicatesExists, for:PredicateNames.exists.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_PredicatesForall, for:PredicateNames.forall.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_PredicatesEquals, for:PredicateNames.equals.rawValue, ofType: PrinterType.mathml.rawValue)
    printer.register(prettyPrintFn:mathml_PredicatesFunction, for:PredicateNames.function.rawValue, ofType: PrinterType.mathml.rawValue)
}

// TODO: register the RewriteRules

let forall_shift_r = RewriteRule(name: "forall", description: "forall x s.t. P, Q => forall x, P -> Q", 
                                 leftPattern: mkPredicate(forall: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPattern(name: "Q")),
                                 rightPattern: mkPredicate(forall: mkPattern(name: "x"), st: mkEmpty(), in: mkPropositionBinaryOp(.imp, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))))
                                                                                                                    
let forall_shift_l = RewriteRule(name: "forall", description: "forall x, P -> Q => forall x s.t. P, Q",
                                 leftPattern: mkPredicate(forall: mkPattern(name: "x"), st: mkEmpty(), in: mkPropositionBinaryOp(.imp, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))),
                                 rightPattern: mkPredicate(forall: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPattern(name: "Q")))

let exists_shift_r = RewriteRule(name: "exists", description: "exists x s.t. P, Q => exists x, P & Q",
                                 leftPattern: mkPredicate(exists: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPattern(name: "Q")),
                                 rightPattern: mkPredicate(exists: mkPattern(name: "x"), st: mkEmpty(), in: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))))
                                                                                                                    
let exists_shift_l = RewriteRule(name: "exists", description: "exists x, P & Q => exists x s.t. P, Q",
                                 leftPattern: mkPredicate(exists: mkPattern(name: "x"), st: mkEmpty(), in: mkPropositionBinaryOp(.and, left: mkPattern(name: "P"), right: mkPattern(name: "Q"))),
                                 rightPattern: mkPredicate(exists: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPattern(name: "Q")))

let demorgans_forall_ex = RewriteRule(name: "deMorgans", description: "not (forall x s.t. P, Q) => exists x s.t. P, not Q",
                                      leftPattern: mkPropositionNot(expression: mkPredicate(forall: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPattern(name: "Q"))),
                                      rightPattern: mkPredicate(exists: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPropositionNot(expression:mkPattern(name: "Q"))))

let demorgans_forall_co = RewriteRule(name: "deMorgans", description: "exists x s.t. P, Q => not (forall x s.t. P, not Q)",
                                      leftPattern: mkPredicate(exists: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPattern(name: "Q")),
                                      rightPattern: mkPropositionNot(expression: mkPredicate(forall: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPropositionNot(expression: mkPattern(name: "Q")))))

let demorgans_exists_ex = RewriteRule(name: "deMorgans", description: "not (exists x s.t. P, Q) => forall x s.t. P, not Q",
                                      leftPattern: mkPropositionNot(expression: mkPredicate(exists: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPattern(name: "Q"))),
                                      rightPattern: mkPredicate(forall: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPropositionNot(expression:mkPattern(name: "Q"))))

let demorgans_exists_co = RewriteRule(name: "deMorgans", description: "forall x s.t. P, Q => not (exists x s.t. P, not Q)",
                                      leftPattern: mkPredicate(exists: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPattern(name: "Q")),
                                      rightPattern: mkPropositionNot(expression: mkPredicate(forall: mkPattern(name: "x"), st: mkPattern(name: "P"), in: mkPropositionNot(expression: mkPattern(name: "Q")))))


/*
 # deductions:
 # modus ponens under forall, forall x. P(x) -> Q(x), P(y) gives Q(y)
 #     or forall x. P(x) <-> Q(x), P(y) gives Q(y)
 #     or forall x. P(x) <-> Q(x), Q(y) gives P(y)
 #     or forall x s.t. F(x). P(x) -> Q(x), F(y), P(y) gives Q(y) etc
 # need to check for variable capture

 # modus ponens (general case): P -> Q, P gives Q etc

 # universal instantiation: forall x. P(x), to P(y) (avoiding variable capture)
 # generalisation: P(y) gives forall x. P(x), but when can we do this
 # proofs go like: let x be arbitrary, ..., P(x), therefore, forall x. P(x)
 # need a let proof step, like assume, let x be arbitrary s.t. that P(x)
 # should it start a sub-proof, how would it interact with universal instantiation
 # (A u B) <<= A -> B <<= A
 # Let x in B, then x in A | x in B -> x \in A u B -> x in A
 # derive x in B -> x in A
 # go to forall x. x in B -> x in A => B <<= A
 # Choose x s.t. F(x)    starts a sub-proof? No adds the result F(x) to the result list, but it is displayed like Choose x s.t. F(x)?
 # ...
 # G(x)
 # generalise G(x),


 # rename bound variable (cannot rename unbound variables safely)
 # var equality: x = y, P(x) gives P(y)
 # from P(x) derive exists x s.t. _, P(x)

 # deduction rules
 # P & Q => P,Q
 deduction_split_and = RewriteRules.Deduction('Predicates.SplitAnd',
                                              RewriteRules.Deduction.std_deduce_func([Propositions.mk_binary_op('and',Expressions.mk_pattern('P'),Expressions.mk_pattern('Q'))],
                                                                                     [Expressions.mk_pattern('P'),Expressions.mk_pattern('Q')]),
                                              True)

 # P,Q => P & Q
 deduction_join_and = RewriteRules.Deduction('Predicates.JoinAnd',
                                             RewriteRules.Deduction.std_deduce_func([Expressions.mk_pattern('P'),Expressions.mk_pattern('Q')],
                                                                                    [Propositions.mk_binary_op('and',Expressions.mk_pattern('P'),Expressions.mk_pattern('Q'))]),
                                             True)

 def _join_or_deduce(exps, args):
     # P => P | Q where Q given in args
     # check there is one expression
     if len(exps) == 1:
         return [Propositions.mk_binary_op('or', exps[0], args['q_exp'])]
     else:
         raise Exception(f"Predicates._join_or_deduce(): expected one expression, found {len(exps)}")

 # P => P | Q for arbitrary Q
 deduction_join_or = RewriteRules.Deduction('Predicates.JoinOr',
                                            _join_or_deduce,
                                            False)


 def _match_expressions(a_exps, b_exps):
     # returns true if the a_exps is some permutation of b_exps
     if not len(a_exps) == len(b_exps):
         return False
     a_ixs = list(range(len(a_exps)))
     b_ixs = list(range(len(b_exps)))
     for a_ix in a_ixs:
         found = False
         for b_ix in b_ixs:
             if a_exps[a_ix] == b_exps[b_ix]:
                 found = True
                 b_ixs.remove(b_ix)
                 break
         if not found:
             return False
     return True

 # TODO: try to satisfy modus ponens with or's (and more generally?)

 def _modus_ponens_deduce(exps, args):
     def _flatten(exp):
         # returns list of expressions after removing &s
         rets = []
         if exp.name == 'Propositions.BinaryOp' and exp.value == 'and':
             rets.extend(_flatten(exp.sub_expressions[0]))
             rets.extend(_flatten(exp.sub_expressions[1]))
             return rets
         else:
             return [exp]

     # check the first arg
     if not (exps[0].name == 'Propositions.BinaryOp' and exps[0].value in ['imp','rev_imp','iff']):
         raise Exception(f"Predicates._modus_ponens_match(): expected P -> Q, Q <- P, P <-> Q but found {exps[0]}")
     # flatten the input Ps
     p_matches = []
     for e in exps[1:]:
         p_matches.extend(_flatten(e))
     # get the implication Ps (from P -> Q, P <-> Q, Q <- P)
     if exps[0].value in ['imp','iff']:
         p_exp = exps[0].sub_expressions[0]
         q_exp = exps[0].sub_expressions[1]
     else: # rev_imp
         p_exp = exps[0].sub_expressions[1]
         q_exp = exps[0].sub_expressions[0]
     # attempt to match
     p_imps = _flatten(p_exp)
     match_res = _match_expressions(p_imps, p_matches)
     # try <- for iff case if no matching
     if not match_res and exps[0].value == 'iff':
         # try the iff the other way
         p_exp = exps[0].sub_expressions[1]
         q_exp = exps[0].sub_expressions[0]
         p_imps = _flatten(p_exp)
         match_res = _match_expressions(p_imps, p_matches)
     # return the result
     if match_res:
         # there is a matching, need to add Q to it
         return [q_exp]
     else:
         raise Exception(f"Predicates._modus_ponens_deduce(): unable to match expressions")

 # P, P -> Q => Q or P, P <-> Q => P or P, Q <- P => Q
 deduction_modus_ponens = RewriteRules.Deduction('Predicates.ModusPonens',
                                                 _modus_ponens_deduce,
                                                 False)

 # TODO: I feel like this should be redesigned, feels very clunky
 # TODO: maybe have a separate modus ponens file
 def _modus_ponens_forall_deduce(exps, args):
     def _flatten(exp):
         # returns list of expressions after removing &s
         rets = []
         if exp.name == 'Propositions.BinaryOp' and exp.value == 'and':
             rets.extend(_flatten(exp.sub_expressions[0]))
             rets.extend(_flatten(exp.sub_expressions[1]))
             return rets
         else:
             return [exp]
     # check we have a forall,
     if not (exps[0].name == 'Predicates.Forall'):
         raise Exception(f"Predicates._modus_ponens_forall_match(): {exps[0]} must be forall ...")
     # go down the nested foralls, collecting the s.t. expressions
     fa_exp = exps[0]
     st_exps = []
     while fa_exp.name == 'Predicates.Forall':
         if not fa_exp.sub_expressions[1].is_empty():
             st_exps.append(fa_exp.sub_expressions[1])
         fa_exp = fa_exp.sub_expressions[2]
     # now, fa_exp should be P -> Q, Q <- P, P <-> Q
     if not (fa_exp.name == 'Propositions.BinaryOp' and fa_exp.value in ['imp', 'rev_imp', 'iff']):
         raise Exception(f"Predicates._modus_ponens_forall_match(): found {fa_exp} under forall, expected P -> Q, Q <- P, or P <-> Q")
     # flatten the input Ps
     p_matches = []
     for e in exps[1:]:
         p_matches.extend(_flatten(e))
     # get the implication Ps (from P -> Q, P <-> Q, Q <- P)
     if fa_exp.value in ['imp','iff']:
         p_exp = fa_exp.sub_expressions[0]
         q_exp = fa_exp.sub_expressions[1]
     else: # rev_imp
         p_exp = exps[0].sub_expressions[1]
         q_exp = exps[0].sub_expressions[0]
     # attempt to match Ps and s.t. expressions
     p_imps = _flatten(p_exp)
     p_imps.extend(st_exps)
     # apply change in variables
     if args is not None and 'rename_vars' in args:
         n_p_imps = []
         for pe in p_imps:
             pe_copy = pe.copy()
             for vr in args['rename_vars']:
                 pe_copy.rename_free_variable(vr[0],vr[1])
             n_p_imps.append(pe_copy)
         # try the matching
         match_res = _match_expressions(n_p_imps, p_matches)
     else:
         match_res = _match_expressions(p_imps, p_matches)
     # try <- for iff case if no matching
     if not match_res and fa_exp.value == 'iff':
         p_exp = fa_exp.sub_expressions[1]
         q_exp = fa_exp.sub_expressions[0]
         p_imps = _flatten(p_exp)
         p_imps.extend(st_exps)
         # apply change in variables
         if args is not None and 'rename_vars' in args:
             n_p_imps = []
             for pe in p_imps:
                 pe_copy = pe.copy()
                 for vr in args['rename_vars']:
                     pe_copy.rename_free_variable(vr[0], vr[1])
                 n_p_imps.append(pe_copy)
             # try the matching
             match_res = _match_expressions(n_p_imps, p_matches)
         else:
             match_res = _match_expressions(p_imps, p_matches)
     # return the result
     if match_res:
         # there is a matching
         if args is not None and 'rename_vars' in args:
             q_copy = q_exp.copy()
             for vr in args['rename_vars']:
                 q_copy.rename_free_variable(vr[0], vr[1])
             return [q_copy]
         else:
             return [q_exp]
     else:
         raise Exception(f"_modus_ponens_forall_deduce(): unable to match expressions")

 # as for modus ponens, but under a forall
 deduction_modus_ponens_forall = RewriteRules.Deduction('Predicates.ModusPonensForall',
                                                        _modus_ponens_forall_deduce,
                                             False)

 def _substitution_deduce(exps, args):
     # exps[0] is the expression with which to make the substitution
     # exps[1] is the expression justifying old_exp == new_exp
     check_exp = mk_equals(args['old_exp'], args['new_exp'])
     if not exps[1] == check_exp:
         raise Exception(f"Predicates._substitution_deduce(): unable to confirm {check_exp}")
     n_exp = exps[0].copy()
     return [n_exp.substitute_expression(args['old_exp'], args['new_exp'])]

 # substitute exp_a for exp_b in exp where exp_a= exp_b
 deduction_substitution = RewriteRules.Deduction('Predicates.Substitution',
                                                 _substitution_deduce,
                                                 True)

 def _rename_bound_variable_deduce(exps, args):
     # exps[0] is the expression, at the cursor
     # args['cursor_directions'], args['new_var']
     # get expression at the cursor
     cursor_exp = exps[0]
     for d in args['cursor_directions']:
         # assume the cursor directions are valid
         if d == -1:
             cursor_exp = cursor_exp.parent_exp
         else:
             cursor_exp = cursor_exp.sub_expressions[d]
     # check that the cursor is a forall or exists
     if cursor_exp.name not in ['Predicates.Forall', 'Predicates.Exists']:
         raise Exception(f"Predicates._rename_bound_variable_deduce(): expected forall or exists, found {cursor_exp}")
     # rename the variable
     n_st_exp = cursor_exp.sub_expressions[1].rename_free_variable(cursor_exp.sub_expression[0], args['new_var'])
     n_exp = cursor_exp.sub_expressions[2].rename_free_variable(cursor_exp.sub_expression[0], args['new_var'])
     cursor_exp.set_sub_expression(args['new_var'], 0)
     cursor_exp.set_sub_expression(n_st_exp, 1)
     cursor_exp.set_sub_expression(n_exp, 2)

 # rename the bound vars at the cursor
 deduction_rename_bound_vars = RewriteRules.Deduction('Predicates.RenameBoundVariable',
                                                      _rename_bound_variable_deduce,
                                                      True)


 # input: forall x s.t. P(x), Q(x) and y
 # output: @x : y, @Q :Q(x) with x replaced by @x
 def _universal_instantiation_deduce(exps, args):
     # check exps[0] is a forall
     if not exps[0].name == 'Predicates.Forall':
         raise Exception(f"Predicates._universal_instantiation_deduce(): expected forall, found {exps[0]}")
     if args is None or not 'at_var' in args:
         raise Exception(f"Predicates._universal_instantiation_deduce(): requires substitution variable in args['at_var']")
     # get st_exp if it exists
     if not exps[0].sub_expressions[1].is_empty():
         st_exp = exps[0].sub_expressions[1].copy()
         # make the substitution
         st_exp.rename_free_variable(exps[0].sub_expressions[0], args['at_var'])
         # check the st condition is satisfied
         if len(exps) < 2 or not st_exp == exps[1]:
             raise Exception(f"Predicates._universal_instantiation_deduce(): unable to satisfy such that condition {st_exp}")
     # return output result
     return [exps[0].sub_expressions[2].copy().rename_free_variable(exps[0].sub_expressions[0], args['at_var'])]

 # forall x: P(x) => P(a), forall x s.t. P(x), Q(x) and P(a) => Q(a)
 deduction_universal_instantiation = RewriteRules.Deduction('Predicates.UniversalInstantiation',
                                                            _universal_instantiation_deduce,
                                                 False)

 def _existential_generalisation_deduce(exps, args):
     # exps is [P(a)], args=[old_var, new_var]
     # var_exp,st_exp,exp
     return mk_exists(args['new_var'],None,exps[0].rename_free_variable(args['old_var'], args['new_var']))

 # Existential Generalisation: P(a) => exists x. P(x)
 deduction_existential_generalisation = RewriteRules.Deduction('Predicates.ExistentialGeneralisation',
                                                               _existential_generalisation_deduce,
                                                               False)

 def add_all_to_proof_context(pc):
     # names
     for n in ['Predicates.Forall', 'Predicates.Exists', 'Predicates.Equals']:
         pc.register_expression_name(n)
     # rewrite rules
     for rr in [forall_alt_form, exists_alt_form, demorgans_not_over_exists, demorgans_not_over_forall ]:
         pc.register_rewrite_rule(rr)
     # deductions
     for dd in [deduction_join_or,
                deduction_join_and,
                deduction_split_and,
                deduction_modus_ponens,
                deduction_modus_ponens_forall,
                deduction_universal_instantiation,
                deduction_existential_generalisation,
                deduction_rename_bound_vars,
                deduction_substitution]:
         pc.register_deduction(dd)

 */
