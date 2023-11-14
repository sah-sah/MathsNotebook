//
//  MainView.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 5/11/2023.
//

import SwiftUI

// for improving the tabview 
// https://stackoverflow.com/questions/60674035/swiftui-custom-tab-view-for-macos-ios

struct MainView: View {
    // TODO: proofSession should be made an enviroment object
    @StateObject var proofSession: ProofSession = ProofSession()
    
    var body: some View {
        TabView {
            ContentView(proofSession: proofSession)
                .tabItem {
                    Label("Proof", systemImage: "list.dash")
                }

            InputView(proofSession: proofSession)
                .tabItem {
                    Label("Input", systemImage: "square.and.pencil")
                }
        }
    }
}


