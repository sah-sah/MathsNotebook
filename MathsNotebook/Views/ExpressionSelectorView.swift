//
//  ExpressionSelectorView.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 10/11/2023.
//

import SwiftUI

// the information needed to use the ExpressionSelectorView as a sheet
struct ExpressionSelectorViewInfo {
    var btnName: String
    var btnFn: ([Expression]) -> ()
}

struct ExpressionSelectorView: View {
    @Environment(\.dismiss) var dismiss
    @ObservedObject var proofSession: ProofSession
    
    private var savedExpNames: [String] { proofSession.savedExpressions.keys.sorted() }

    var body: some View {
        VStack {
            List(savedExpNames, id: \.self, selection: $proofSession.selectedSavedExpressions) { name in
                Text(name)
            }
            HStack {
                Button("Cancel") {
                    dismiss()
                }
                Button(proofSession.expressionSelectorViewInfo.btnName) {
                    // add the selected expressions as assumptions
                    if proofSession.selectedSavedExpressions.count > 0 {
                        var exps: [Expression] = []
                        for n in proofSession.selectedSavedExpressions {
                            if let e = proofSession.savedExpressions[n] {
                                exps.append(e)
                            }
                        }
                        if exps.count > 0 {
                            proofSession.expressionSelectorViewInfo.btnFn(exps)
                        }
                    }
                    dismiss()
                }
            }
        }
        .padding()
        .frame(width:300, height:150)
    }
}

