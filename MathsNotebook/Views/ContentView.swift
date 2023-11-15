//
//  ContentView.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 26/10/2023.
//

import SwiftUI
import WebKit

// TODO: Add predicates (& update parser)
// TODO: Add sets (& update parser)
// TODO: Add deductions (modus ponens etc)


struct ContentView: View {
    @ObservedObject var proofSession: ProofSession
    @State private var expressionSelectorSheet = false
    @State private var ruleSheet = false
    
    var body: some View {
        VStack {
            HStack {
                Button("Add to proof") {
                    // we are going to show the expression selector
                    // TODO: a way to specify the proofId
                    proofSession.expressionSelectorViewInfo.btnName = "Assume"
                    proofSession.expressionSelectorViewInfo.btnFn = { exps in
                        proofSession.proof.assume(expressions: exps, forProof: Proof.rootId)
                        proofSession.updateAvailableResultIds()
                        proofSession.update()
                    }
                    expressionSelectorSheet.toggle()
                }
                Picker("Focus", selection: $proofSession.focusId) {
                    ForEach(proofSession.availableResultIds, id: \.self) {
                        Text($0)
                    }
                }
                .pickerStyle(.menu)
                .frame(maxWidth: 200)
                CursorPadView(proofSession: proofSession)
                Button("Apply Rule") {
                    // if there is a focus (focusId != "") and a selected rewrite rule
                    if proofSession.focusId.count > 0, let rule = proofSession.selectedRewriteRule {
                        // TODO: check whether the rule requires inputs
                        if let sps = rule.inputSpecifications, sps.count > 0 {
                            // specify the ruleInputViewInfo data
                            proofSession.ruleInputViewInfo.inputSpecifications = sps
                            proofSession.ruleInputViewInfo.btnName = "Transform"
                            proofSession.ruleInputViewInfo.deductionFn = { b in
                                proofSession.proof.transform(resultId: proofSession.focusId, by: rule, with: b, in: "1.")
                                proofSession.updateAvailableResultIds()
                                proofSession.update()
                            }
                            ruleSheet.toggle()
                        } else { // apply the rule directly
                            proofSession.proof.transform(resultId: proofSession.focusId, by: rule, with: emptyBinding, in: "1.")
                            proofSession.updateAvailableResultIds()
                            proofSession.update()
                        }
                    }
                }
                Picker("RewriteRule", selection:$proofSession.selectedRewriteRule) {
                    Text("Select a rule").tag(Optional<RewriteRule>(nil))
                    ForEach(proofSession.applicableRewriteRules) {
                        Text($0.name + ": " + $0.description).tag(Optional($0))
                    }
                }
                .pickerStyle(.menu)
            }
            .onChange(of: proofSession.focusId) {
                proofSession.updateApplicableRewriteRules()
            }
            ProofWebView(proofSession: proofSession)
        }
        .onKeyPress { press in
            switch press.key {
            case KeyEquivalent.upArrow:
                // if the code moves too fast, it throws an error Publishing changes from within view updates...
                // TODO: try moving the async call to objectWilChange.send()
                DispatchQueue.main.async() { proofSession.moveFocusCursor(.up) }
            case KeyEquivalent.downArrow:
                DispatchQueue.main.async() { proofSession.moveFocusCursor(.down) }
            default: break
            }
            return .handled
        }
        .padding()
        .sheet(isPresented: $expressionSelectorSheet) {
            ExpressionSelectorView(proofSession: proofSession)
        }
        .sheet(isPresented: $ruleSheet) {
            RuleInputView(proofSession: proofSession)
        }
    }
    
}

