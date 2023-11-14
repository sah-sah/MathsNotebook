//
//  Proof.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 27/10/2023.
//

import Foundation

typealias ProofStepId = String
typealias ProofId = String
typealias ResultId = String

enum CursorMove {
    case up, down, left, right
}

// TODO: we can split this up using extension, except we can't access properties directly from another file
class Proof {
    
    // Data
    struct ProofInfo {
        var numResults: Int = 0
        var numProofSteps: Int = 0
        var numSubproofs: Int = 0
    }
    
    static let rootId: ProofId = ""
    
    var context: ProofContext
    var info: Dictionary<ProofId, ProofInfo>
    var steps: Dictionary<ProofStepId, ProofStep>
    var results: Dictionary<ResultId, Result>
    
    // Initialisation
    init() {
        self.context = ProofContext()
        self.info = ["": ProofInfo()]
        self.steps = [:]
        self.results = [:]
        
        // TODO: initialise the proof context? we need a better way, store these details in the file in which they are defined,
        for rr in [equivalence_ex,equivalence_co,reverseImplication_to,reverseImplication_fr,implication_ex,implication_co,doubleNegation_co,doubleNegation_ex,idempotent_and_co,idempotent_and_ex,idempotent_or_co,idempotent_or_ex,commutative_and,commutative_or,associative_and_lf,associative_and_rt,associative_or_lf,associative_or_rt,distributive_and_ex,distributive_and_co,distributive_or_ex,distributive_or_co,deMorgans_and_ex,deMorgans_and_co,deMorgans_or_ex,deMorgans_or_co,identity_and_co,identity_and_ex,identity_or_co,identity_or_ex,annihilation_and_co,annihilation_and_ex,annihilation_or_co,annihilation_or_ex,inverse_and_co,inverse_and_ex,inverse_or_co,inverse_or_ex,absorption_and_co,absorption_and_ex,absorption_or_co,absorption_or_ex,fstDefinition_co,fstDefinition_ex,sndDefinition_co,sndDefinition_ex] {
            context.register(rewriteRule: rr)
        }
        
        for rr in [forall_shift_r,forall_shift_l,exists_shift_r,exists_shift_l,demorgans_forall_ex,demorgans_forall_co,demorgans_exists_ex,demorgans_exists_co] {
            context.register(rewriteRule: rr)
        }
        
        for (sn,nargs,fn) in propositionsSystemNames + predicatesSystemNames {
            context.register(systemName: sn, for: fn, with:nargs)
        }
    }
    
    
    func getResult(forId resultId: ResultId) -> Result? {
        return results[resultId]
    }
    
    // Proofsteps
    func assume(expressions: [Expression], forProof proofId: ProofId) {
        // get proof info
        if let p_info = self.info[proofId] {
            // start new sub proof
            // get sub proof id
            let subProofId = "\(proofId)\(p_info.numSubproofs + 1)."
            // update proof info
            self.info[proofId] = ProofInfo(numResults: p_info.numResults, numProofSteps: p_info.numProofSteps, numSubproofs: p_info.numSubproofs+1)
            self.info[subProofId] = ProofInfo()
            // record sub proof creation
            record(expressions:[], as:ProofStep(type: ProofStepType.subproof, args: ["subProofId":subProofId]), at:proofId)
            // add expression to the results
            record(expressions:expressions, as:ProofStep(type:ProofStepType.assumption), at:subProofId)
        } else {
            print("Error: no such proof id")
        }
    }
    
    // Utility functions
    func record(expressions: [Expression], as proofStep: ProofStep, at proofId: ProofId) {
        if let p_info = self.info[proofId] {
            // get proof step id
            let ps_id = "\(proofId)\(p_info.numProofSteps + 1)"
            // record results
            var res_ids: [ResultId] = []
            if expressions.count > 0 {
                for ix in 1...expressions.count {
                    let r_id = "\(proofId)\(p_info.numResults + ix)"
                    res_ids.append(r_id)
                    self.results[r_id] = Result(resultId: r_id, expression: expressions[ix-1], proofStepId: ps_id)
                }
            }
            // record proof step
            proofStep.update(resultIds: res_ids, proofStepId: ps_id)
            self.steps[ps_id] = proofStep
            // update proof details
            self.info[proofId] = ProofInfo(numResults: p_info.numResults + res_ids.count, numProofSteps: p_info.numProofSteps + 1, numSubproofs: p_info.numSubproofs)
        }
    }
    
    // sets cursor at the root (whether there was an existing cursor or not)
    func startCursor(for resultId: ResultId) {
        if let res = self.results[resultId] {
            res.cursorDirections = []
        }
        // else no such result
    }
    
    // clears the cursor
    func clearCursor(for resultId: ResultId) {
        if let res = self.results[resultId] {
            res.cursorDirections = nil
        }
        // else no such result
    }
    
    // TODO: maybe return Bool indicating success or failure
    func moveCursor(_ dirn: CursorDirection, for resultId: ResultId) -> Bool {
        if let res = self.results[resultId] {
            // check it is a valid direction
            if dirn.rawValue == -1 {
                // go up
                if let cds = res.cursorDirections {
                    if cds.count > 0 {
                        res.cursorDirections = Array(cds[0..<cds.count-1])
                        //objectWillChange.send()
                        return true
                    }
                } // else ignore (cursor not initiated)
            } else { // dirn.rawValue >= 0
                // go down
                // get to the current cursor (we can assume the directions are valid)
                if let cds = res.cursorDirections {
                    var exp = res.expression
                    for ix in 0..<cds.count {
                        exp = exp.subexpressions[cds[ix].rawValue]
                    }
                    // exp is the current cursor
                    if dirn.rawValue < exp.subexpressions.count {
                        res.cursorDirections = cds + [dirn]
                        //objectWillChange.send()
                        return true
                    }
                } // else ignore (cursor not initiated)
            }
        } // else no such result
        // failed to move cursor above
        return false
    }
    
    

}

// TODO: Put some of this stuff in another file so this file doesn't get too cluttered
class ProofContext {
    // TODO: axiomSchemas, rewriterules should have a namespace
    private(set) var axiomSchemas: Dictionary<String, String>
    private(set) var rewriteRules: [RewriteRule]
    private(set) var expressionNames: Set<String>
    private(set) var deductions: Dictionary<String, String>
    private(set) var evaluations: Dictionary<String, String>
    private(set) var printer: ExpressionPrinter
    private(set) var systemNames: [String:(Int, ([Expression])->Expression)]
    
    init() {
        self.axiomSchemas = [:]
        self.rewriteRules = []
        self.expressionNames = Set<String>()
        self.deductions = [:]
        self.evaluations = [:]
        // set up printing
        self.printer = ExpressionPrinter()
        updatePrinterExpressions(&printer)
        updatePrinterPropositions(&printer)
        updatePrinterPredicates(&printer)
        // system names for parsing
        self.systemNames = [:]
    }
    
    // TODO: check whether they have already been registered?
    func registerAxiomSchema(name: String, for schema: String) {
        self.axiomSchemas[name] = schema
    }
    
    func register(rewriteRule: RewriteRule) {
        self.rewriteRules.append(rewriteRule)
    }
    
    func rewriteRuleByUUID(_ uuid: UUID) -> RewriteRule? {
        for rr in rewriteRules {
            if rr.id == uuid { return rr }
        }
        return nil
    }
    
    func registerExpressionName(name: String) {
        self.expressionNames.insert(name)
    }
    
    func registerDeduction(name: String, for deduction: String) {
        self.deductions[name] = deduction
    }
    
    func registerEvaluation(name: String, for evaluation: String) {
        self.evaluations[name] = evaluation
    }
    
    func register(systemName: String, for sysFn: @escaping ([Expression]) -> Expression, with nargs: Int) {
        self.systemNames[systemName] = (nargs, sysFn)
    }
    
    //func mkSysname(name: String, args: [Expression]) -> Expression {
    //    return Expression(type: ExpressionType.sysname, name: ExpressionNames.sysname.rawValue, value:name, subexpressions: args)
    //}
    
    func resolveSystemNames(for exp: Expression) -> Expression? {
        var rSubExps: [Expression] = []
        // convert list of optionals to an optional of a list (return on the nil)
        for rseQ in exp.subexpressions.map(resolveSystemNames(for:)) {
            if let rse = rseQ {
                rSubExps.append(rse)
            } else {
                return nil
            }
        }
        // lookup the system name
        if exp.type == ExpressionType.sysname {
            if let (nargs, fn) = self.systemNames[exp.value] {
                if rSubExps.count == nargs {
                    return fn(rSubExps)
                }
            } // else, lookup failure or invalid expression
            return nil
        }
        // a non-system name expression
        // update the sub-expressions
        var nexp = exp
        for ix in 0..<rSubExps.count {
            nexp.setSubexpression(at: ix, to: rSubExps[ix])
        }
        return nexp
    }
}

enum ProofStepType {
    case axiom, comment, transform, deduction, assumption, choose, define, subproof, lift, generalise, evaluation
}

class ProofStep {
    var type: ProofStepType
    var sourceResultIds: [ResultId]
    var equivalence: Bool
    var args: Dictionary<String, Any>?
    // these will be set after initialisation
    private(set) var resultIds: [ResultId]
    private(set) var proofStepId: ProofStepId
    
    init(type: ProofStepType, source: [ResultId] = [], equivalence: Bool = false, args: Dictionary<String, Any>? = nil) {
        // set at initialisation
        self.type = type
        self.sourceResultIds = source
        self.equivalence = equivalence
        self.args = args
        // these should all be set after initialisation
        self.proofStepId = ""
        self.resultIds = []
    }
    
    func update(resultIds: [ResultId], proofStepId: ProofStepId) {
        self.resultIds = resultIds
        self.proofStepId = proofStepId
    }
}

// TODO: add in a cursor

enum CursorDirection: Int {
    case up = -1
    case down1 = 0
    case down2 = 1
    case down3 = 2
    case down4 = 3
    case down5 = 4
    case down6 = 5
    case down7 = 6
    case down8 = 7
    case down9 = 8
    case down10 = 9
    case down11 = 10
    case down12 = 11
}

// TODO: do we want all the results to have a cursor
// if not, start with cursorDirections = nil
// add a center button that starts a cursor
class Result {
    var resultId: ResultId
    var expression: Expression
    var proofStepId: ProofStepId
    var cursorDirections: [CursorDirection]?
    
    init(resultId: ResultId, expression: Expression, proofStepId: ProofStepId) {
        self.resultId = resultId
        self.expression = expression
        self.proofStepId = proofStepId
        self.cursorDirections = [] // root cursor
    }
    
    func getCursor() -> Expression? {
        if let cds = cursorDirections {
            // follow directions to cursor
            var cursor = expression
            for d in cds {
                cursor = cursor.subexpressions[d.rawValue]
            }
            return cursor
        } else {
            // no cursor
            return nil
        }
    }
    
    // we could move this to the transform function
    func replaceAtCursor(with exp: Expression) -> Expression? {
                
        if let cds = cursorDirections {
            if cds.count == 0 { return exp }
            else {
                var n_exp = expression
                n_exp.replaceAtCursor(cds, with: exp)
                return n_exp
            }
        } else {
            // no cursor to replace
            return nil
        }
    }
}




