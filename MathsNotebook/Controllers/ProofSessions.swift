//
//  ProofSessions.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 3/11/2023.
//

import Foundation

class ProofSession: ObservableObject {
    // the proof, maybe later this should be a list of proofs
    var proof: Proof = Proof()
    // data for managing the views
    @Published var focusId: ResultId = ""
    @Published var selectedRewriteRule: RewriteRule? = nil
    @Published var applicableRewriteRules: [RewriteRule] = []
    
    @Published var availableResultIds: [String] = []
    
    @Published var savedExpressions: [String:Expression] = [:]
    var selectedSavedExpressions: Set<String> = Set<String>()
    
    // give default of OK button that does nothing
    var expressionSelectorViewInfo: ExpressionSelectorViewInfo = ExpressionSelectorViewInfo(btnName: "OK", btnFn: { _ in })
    
    // default values
    var ruleInputViewInfo: RuleInputInfo = RuleInputInfo(inputSpecifications: [], deductionFn: { _ in }, btnName: "OK")
    
    enum ProofAction {
        case assume, transform
    }
    
    // force update of views
    func update() {
        objectWillChange.send()
    }
    
    func updateAvailableResultIds() {
        availableResultIds = Array(proof.results.keys)
        availableResultIds.sort { // <=
            // get components
            let lhs = $0.split(separator: ".")
            let rhs = $1.split(separator: ".")
            // compare
            for ix in 0..<min(lhs.count,rhs.count) {
                let l_int = Int(lhs[ix]) ?? 0
                let r_int = Int(rhs[ix]) ?? 0
                if l_int < r_int { return true }
                if l_int > r_int { return false }
                // otherwise they are equal, try the next component
            }
            // see which is longer
            if lhs.count < rhs.count { return true }
            if lhs.count > rhs.count { return false }
            // they are equal
            return true
        }
        availableResultIds.insert("", at: 0)
    }
    
    func updateApplicableRewriteRules() {
        let names = proof.context.rewriteRules
        // filter to those that are applicable
        applicableRewriteRules = names.filter {
            proof.checkTransform(for: $0, at: focusId)
        }
        //if !namesF.contains(ruleName) { ruleName = "" }
        applicableRewriteRules.sort() {
            $0.name < $1.name
        }
        // check the selectedRewriteRule is still valid
        if let selId = selectedRewriteRule?.id {
            if !applicableRewriteRules.contains(where: { $0.id == selId }) {
                selectedRewriteRule = nil
            }
        }
    }
    
    func setFocusId(_ resId: String) {
        // check it is a valid result
        if let _ = proof.results[resId] {
            self.focusId = resId
            updateApplicableRewriteRules()
        }
    }
    // TODO: could bring the cursorDirections out and keep them
    // TODO: in sync with the res values, then use @Published to avoid
    // TODO: calling objectWilChange()
    // TODO: redo the below using Proof methods to move cursor
    func moveFocusCursor(_ move: CursorMove) {
        if let res = proof.results[focusId] {
            // we have a result
            if let cds = res.cursorDirections, let cursor = res.getCursor() {
                // a cursor exists, get the current cursor
                // try to move the cursor
                switch move {
                case .up:
                    if cds.count > 0 {
                        res.cursorDirections = Array(cds[0..<cds.count-1])
                        updateApplicableRewriteRules()
                        objectWillChange.send()
                    }
                case .down:
                    if cursor.subexpressions.count > 0 {
                        // go down the left most branch if there is no cursor, else try the current branch
                        let currBranch = cds.count == 0 ? 0 : cds[cds.count-1].rawValue
                        let belowBranch = min(11,cursor.subexpressions.count-1,currBranch)
                        res.cursorDirections = Array(cds + [CursorDirection(rawValue: belowBranch)!])
                        updateApplicableRewriteRules()
                        objectWillChange.send()
                    }
                case .left:
                    // there must be an expression above
                    if cds.count > 0 {
                        let currBranch = cds[cds.count-1].rawValue
                        let leftBranch = currBranch > 0 ? currBranch - 1 : 0
                        res.cursorDirections = Array(cds[0..<cds.count-1] + [CursorDirection(rawValue: leftBranch)!])
                        updateApplicableRewriteRules()
                        objectWillChange.send()
                    }
                case .right:
                    // there must be an expression above
                    if cds.count > 0 {
                        // get the expression above to check whether we can go right
                        res.cursorDirections = Array(cds[0..<cds.count-1])
                        // get the new cursor
                        let cursorAbove = res.getCursor()!
                        // try the right branch
                        let currBranch = cds[cds.count-1].rawValue
                        let rightBranch = min(cursorAbove.subexpressions.count-1,currBranch+1)
                        // set the new cursor
                        res.cursorDirections = Array(cds[0..<cds.count-1] + [CursorDirection(rawValue: rightBranch)!])
                        updateApplicableRewriteRules()
                        objectWillChange.send()
                    }
                }
            } else {
                // start a new cursor, if the move is down
                if move == .down {
                    if res.expression.subexpressions.count > 0 {
                        res.cursorDirections = [CursorDirection.down1]
                        updateApplicableRewriteRules()
                        objectWillChange.send()
                    }
                }
            }
        } // else, invalid focus
    }
    
    // printing the proof
    func proofString(for proofId: ProofId = "", prefix: String = "") -> String {
        var proofStr = ""
        if let p_info = proof.info[proofId] {
            for ix in 1...p_info.numProofSteps {
                // get proof step id
                let ps_id = "\(proofId)\(ix)"
                // get the proof step
                if let ps = proof.steps[ps_id] {
                    // are we starting a sub proof
                    if ps.type == .subproof {
                        proofStr += prefix + "+++++ New Sub Proof +++++\n"
                        // recursively print sub proof
                        if let sp_id = ps.args?["subProofId"] {
                            proofStr += proofString(for: sp_id as! ProofId, prefix: prefix+"    ")
                        } else {
                            proofStr += "Unknown sub proof for proof step: \(ps_id)"
                        }
                    } else {
                        // describe the proof step
                        proofStr += prefix + "+++++ \(ps.type) +++++\n"
                        // describe the results
                        for r_id in ps.resultIds {
                            if let res = proof.results[r_id] {
                                proofStr += prefix + "    [\(r_id)] \(proof.context.printer.print(expression: res.expression, withCursor:res.cursorDirections, as: PrinterType.basic.rawValue))\n"
                            } else {
                                proofStr += "Unknown result with id: \(r_id)\n"
                            }
                        }
                    }
                } else {
                    proofStr += "No proof step with proof id: \(ps_id)\n"
                }
            }
        }
        return proofStr
    }
    

    
    // TODO: because it is part of the view
    // TODO: the second call is recursive and the focusId is not passed through
    func mathmlString(for proofId: ProofId = "", prefix: String = "") -> String {
        var mathmlStr = ""
        if let p_info = proof.info[proofId], p_info.numProofSteps > 0 {
            for ix in 1...p_info.numProofSteps {
                // get proof step id
                let ps_id = "\(proofId)\(ix)"
                // get the proof step
                if let ps = proof.steps[ps_id] {
                    // we have a proof step
                    mathmlStr += "<div class=\"proof-step\">\n"
                    // are we starting a sub proof
                    if ps.type == .subproof {
                        mathmlStr += prefix + "<hr>\n"
                        // recursively print sub proof
                        if let sp_id = ps.args?["subProofId"] {
                            mathmlStr += mathmlString(for: sp_id as! ProofId, prefix: prefix+"    ")
                        } else {
                            mathmlStr += "Unknown sub proof for proof step: \(ps_id)"
                        }
                        mathmlStr += prefix + "<hr>\n"
                    } else {
                        // describe the proof step
                        mathmlStr += prefix + "<p>+++++ \(ps.type) +++++</p>\n"
                        // describe the results
                        for r_id in ps.resultIds {
                            mathmlStr += prefix + "<div class=\"proof-result\" id=\"\(r_id)\">"
                            if let res = proof.results[r_id] {
                                // TODO: add a div (or use p) with the result id
                                //mathmlStr += prefix + "<p>[\(r_id)] <math>\(printer.print(expression: res.expression))</math></p>\n"
                                let rIdStr = r_id == focusId ? "*[\(r_id)]" : "[\(r_id)]"
                                mathmlStr += prefix + "<p>\(rIdStr) <math>\(proof.context.printer.pretty(expression: res.expression, withCursor: res.cursorDirections, as: PrinterType.mathml.rawValue))</math></p>\n"
                            } else {
                                mathmlStr += "Unknown result with id: \(r_id)\n"
                            }
                            mathmlStr += prefix + "</div>"
                        }
                    }
                    // close proof step div
                    mathmlStr += "</div>"
                } else {
                    mathmlStr += "No proof step with proof id: \(ps_id)\n"
                }
            }
        }
        return mathmlStr
    }
    
    /*
     How to structure the mathml
     Each proof step can havae multiple results.
     Each result needs its own tile
     */
    
}
