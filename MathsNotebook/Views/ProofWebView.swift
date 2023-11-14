//
//  ProofWebView.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 26/10/2023.
//

import SwiftUI
import WebKit

// Use <style> in head
//<style>
//mrow .expression { outline-style: solid; }
//</style>

let htmlHeader = """
    <!doctype html>
    <html lang="en-US">
      <head>
    <title>Proof</title>
      </head>
      <body>
"""

let htmlFooter = """
      </body>
    </html>
"""

// https://www.hackingwithswift.com/articles/112/the-ultimate-guide-to-wkwebview
// https://stackoverflow.com/questions/45062929/handling-javascript-events-in-wkwebview
// https://stackoverflow.com/questions/34751860/get-html-from-wkwebview-in-swift

// Order of user events:
// User interacts with webpage, which causes interactino with UI, to make a proof step
// Proof is updated and webview is reloaded

// TODO: How to show the cursor? Similar to previous, use extra brackets (maybe colour them)
// TODO: or use javascript to find the cursor element and color it's children
// TODO: Use brackets for now, it is the easiest solution

struct ProofWebView: NSViewRepresentable {
    @ObservedObject var proofSession: ProofSession
    // TODO: binding to a Proof?
    // TODO: header and footer for html page (including css and javascript)
    
    // webView.navigationDelegate = self
    // self conforms to WKNavigationDelegate
    
    let js_example = """
const para = document.createElement("p");
  const node = document.createTextNode("This is a paragraph.");

  para.appendChild(node);
  document.body.appendChild(para);
"""
    
    let buttonExample = """
    var button = document.getElementById('test-button');
    if(button != null) {
        button.addEventListener("click", function(){
            window.webkit.messageHandlers.iosClickListener.postMessage('open_invitation');
        });
    }

    """
    
    // on click, go up the document tree looking for an element with an id
    // if we find one, highlight it, send the id to the listener
    // const el = e.closest(".expression")
    // var el = e.target.parentElement;
    // set background view the proof object
    let docExample = """
        document.addEventListener("click", function(e) {
        
            window.webkit.messageHandlers.proofWebViewListener.postMessage('mouse_click: ' + e.target.tagName + ' ' + e.target.id);
        
            var el = e.target.closest(".expression")
        
            window.webkit.messageHandlers.proofWebViewListener.postMessage('mouse_click: ' + el.tagName + ' ' + el.id);
        });
        """
    
    // js selecting focus by clicking class=proof-result, id=ResultId
    let focusClickJS = """
document.addEventListener("click", function(event) {
    var res = event.target.closest(".proof-result");
    window.webkit.messageHandlers.proofWebViewListener.postMessage('mouse_click: resultId: ' + res.id);
});
"""
    
    //var webView: WKWebView!
    //var msgHandler: ProofWebViewMessageHandler = ProofWebViewMessageHandler(proofSession: proofSession)
    
    func makeNSView(context: Context) -> WKWebView {
        // set up js code for detecting mouse clicks etc
        let config = WKWebViewConfiguration()
        let script = WKUserScript(source: focusClickJS, injectionTime: .atDocumentEnd, forMainFrameOnly: false)
        config.userContentController.addUserScript(script)
        config.userContentController.add(ProofWebViewMessageHandler(proofSession: proofSession), name: "proofWebViewListener")
        let webview = WKWebView(frame: .zero, configuration: config)
        
        return webview
    }
    
    // TODO: check when this is called, do we need to use it or can everything be put in makeNSView
    // TODO: this should be called when we have updated the proof
    // TODO: generate the new HTML in this function using context?
    // TODO: declare the proof var as an environment variable, then it can be obtained from the context
    func updateNSView(_ nsView: WKWebView, context: Context) {
        // get mathml printer from proof
        let mathmlStr = proofSession.mathmlString()
        //print(mathmlStr)
        let htmlString = htmlHeader + mathmlStr + htmlFooter
        
        nsView.loadHTMLString(htmlString, baseURL: nil)
        
        // scroll to focus
        // TODO: this is not working
        // TODO: it's better if we can scroll to an element
        // TODO: add a JS function to focusClickJS that does this?
        // TODO: in general we should put as much inside the html and initial JS script as possible
        //nsView.evaluateJavaScript("document.getElementById('30').scrollIntoView();")
        if let scrollView = nsView.enclosingScrollView {
            scrollView.scrollToEndOfDocument(nil)
            //scrollRectToVisible(CGRect(origin: CGPointZero, size: scrollView.contentSize))
        }
    }
}

class ProofWebViewMessageHandler: NSObject, WKScriptMessageHandler {
    var proofSesssion: ProofSession
    
    init(proofSession: ProofSession) {
        self.proofSesssion = proofSession
    }
    
    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if let body = message.body as? String {
            // parse body
            // TODO: set the cursor using the mouse
            if body.starts(with: "mouse_click: resultId:") {
                let start = body.index(body.startIndex, offsetBy: 23)
                let res_id = body[start..<body.endIndex]
                print("ResultId:\(String(res_id))")
                // update focus
                proofSesssion.setFocusId(String(res_id))
            }
        }
        
        // update the Proof object
    }
}

// we shouldn't need this, as everything will be based on event listeners
// that can only happen after the view is displayed
/*
class NavDelegate: NSObject, WKNavigationDelegate {
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!)
    {
        let js_example = """
 const para = document.createElement("p");
  const node = document.createTextNode("This is a paragraph.");

  para.appendChild(node);
  document.body.appendChild(para);
"""
        webView.evaluateJavaScript(js_example) { (result, error) in
            if error == nil {
                print(result!)
            } else {
                print(error)
            }
        }
    }
}
 */
