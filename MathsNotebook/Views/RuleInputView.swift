//
//  RuleInputView.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 10/11/2023.
//

import SwiftUI

// necessary data is stored in here
struct RuleInputInfo {
    // get any inputs
    var inputSpecifications: [InputSpecification]
    // apply the deduction
    var deductionFn: (ExpressionBinding) -> ()
    // button name
    var btnName: String
}

// view for inputting data needed to apply RewriteRules, Deductions etc
struct RuleInputView: View {
    @Environment(\.dismiss) var dismiss
    @ObservedObject var proofSession: ProofSession

    var body: some View {
        Form {
            Section {
                ForEach(proofSession.ruleInputViewInfo.inputSpecifications, id: \.name) {
                    InputExpressionView(inputSpec: $0, proofSession: proofSession)
                }
            }
            Section {
                HStack {
                    Button("Cancel") { dismiss() }
                    Button(proofSession.ruleInputViewInfo.btnName) {
                        var b: ExpressionBinding = [:]
                        for spec in proofSession.ruleInputViewInfo.inputSpecifications {
                            // TODO: this needs to check the type of input
                            // try to create ExpressionBinding from specs
                            if let val_exp = proofSession.savedExpressions[spec.val as! String] {
                                b[spec.name] = [val_exp]
                            } else {
                                print("RuleInputView: error processing input")
                                dismiss()
                            }
                        }
                        proofSession.ruleInputViewInfo.deductionFn(b)
                        dismiss()
                    }
                }
            }
        }
        .padding()
        .frame(width:300, height:150)
    }
}

// view for inputting an expression (as opposed to an int etc)
struct InputExpressionView: View {
    var inputSpec: InputSpecification
    @ObservedObject var proofSession: ProofSession
    @State private var expName: String = ""
    
    var body: some View {
        HStack {
            Picker(inputSpec.description, selection: $expName) {
                ForEach(Array(proofSession.savedExpressions.keys), id:\.self) {
                    Text($0)
                }
            }
            .onChange(of: expName) {
                for ix in 0..<proofSession.ruleInputViewInfo.inputSpecifications.count {
                    if proofSession.ruleInputViewInfo.inputSpecifications[ix].name == inputSpec.name {
                        proofSession.ruleInputViewInfo.inputSpecifications[ix].val = expName
                    }
                }
            }
        }
    }
}
