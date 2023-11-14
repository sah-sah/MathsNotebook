//
//  InputWebView.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 9/11/2023.
//

import SwiftUI
import WebKit

// generating the html code
func IWVHtml(body: String) -> String {
    return """
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Bootstrap demo</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-T3c6CoIi6uLrA9TneNEoa7RxnatzjcDSCmG1MXxSR1GAsXEV/Dwwykc2MPK8M2HN" crossorigin="anonymous">
  </head>
  <body>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js" integrity="sha384-C6RzsynM9kWDrMNeT87bh95OGNyZPhcTNXj1NW7RuBCsyN/o0jlpcV8Qyq46cDfL" crossorigin="anonymous"></script>
    \(body)
  </body>
</html>
"""
}

func IWVHtmlRow(for expHtml: String, named: String) -> String {
    return """
<div class="row">
<div class="col-1"><input class="form-check-input me-1" type="checkbox" value="" onclick="msgInputView('\(named)')" id="\(named)"></div>
<div class="col-1">\(named)</div>
<div class="col-5">\(expHtml)</div>
</div>
"""
}

func IWVHtmlContainer(for expsHtml: String) -> String {
    return """
<div class="container">
\(expsHtml)
</div>
"""
}

let initScript = """
function msgInputView(name) {
  let cb = document.getElementById(name)
  if (cb.checked) {
    window.webkit.messageHandlers.inputWebViewListener.postMessage('checkbox_checked:' + name);
  } else {
    window.webkit.messageHandlers.inputWebViewListener.postMessage('checkbox_unchecked:' + name);
  }
}
"""

struct InputWebView: NSViewRepresentable {
    @ObservedObject var proofSession: ProofSession
    
    func makeNSView(context: Context) -> WKWebView {
        // set up js code for detecting mouse clicks etc
        let config = WKWebViewConfiguration()
        let script = WKUserScript(source: initScript, injectionTime: .atDocumentEnd, forMainFrameOnly: false)
        config.userContentController.addUserScript(script)
        config.userContentController.add(InputWebViewMessageHandler(proofSession: proofSession), name: "inputWebViewListener")
        let webview = WKWebView(frame: .zero, configuration: config)
        
        return webview
    }
    
    func updateNSView(_ nsView: WKWebView, context: Context) {
        // generate string from savedExpressions
        // get mathml printer from proof
        
        var mathmlStr = ""
        for (name, exp) in proofSession.savedExpressions {
            // add html
            let mathmlExp = proofSession.proof.context.printer.pretty(expression: exp, as: PrinterType.mathml.rawValue)
            // TODO: pass through whether the expression is selected
            mathmlStr += IWVHtmlRow(for: mathmlExp.0, named: name)
        }
        
        //print(mathmlStr)
        let htmlString = IWVHtml(body:IWVHtmlContainer(for: mathmlStr))
        
        nsView.loadHTMLString(htmlString, baseURL: nil)
    }
}

class InputWebViewMessageHandler: NSObject, WKScriptMessageHandler {
    var proofSesssion: ProofSession
    
    init(proofSession: ProofSession) {
        self.proofSesssion = proofSession
    }
    
    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if let body = message.body as? String {
            // parse body
            if body.starts(with: "checkbox_checked:") {
                let start = body.index(body.startIndex, offsetBy: 17)
                let exp_name = String(body[start..<body.endIndex])
                // try to add exp_name to list of selected savedExpressions
                if !proofSesssion.selectedSavedExpressions.contains(exp_name) {
                    proofSesssion.selectedSavedExpressions.insert(exp_name)
                }
            } else if body.starts(with: "checkbox_unchecked:") {
                let start = body.index(body.startIndex, offsetBy: 19)
                let exp_name = String(body[start..<body.endIndex])
                // try to remove exp_name from list of selected savedExpressions
                if proofSesssion.selectedSavedExpressions.contains(exp_name) {
                    proofSesssion.selectedSavedExpressions.remove(exp_name)
                }
            } else {
                print("Unexpected msg: \(body)")
            }
        }
        
        // update the Proof object
    }
}
