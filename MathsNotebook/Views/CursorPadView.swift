//
//  CursorPadView.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 2/11/2023.
//

import SwiftUI

struct CursorPadView: View {
    @ObservedObject var proofSession: ProofSession
    
    var body: some View {
        VStack {
            HStack {
                Button { proofSession.moveFocusCursor(.up) } label: { Image(systemName: "arrowshape.up")}
                    .padding(-2)
            }
            HStack {
                Button { proofSession.moveFocusCursor(.left) } label: { Image(systemName: "arrowshape.left")}
                    .padding(-2)
                Button { proofSession.moveFocusCursor(.right) } label: { Image(systemName: "arrowshape.right")}
                    .padding(-2)
            }
            HStack {
                Button { proofSession.moveFocusCursor(.down) } label: { Image(systemName: "arrowshape.down")}
                    .padding(-2)
            }
        }
    }
    
    // TODO: add a function that moves the cursor
}

