//
//  ViewController.swift
//  OffApp
//
//  Created by Christian Kjaer Laustsen on 28/01/2018.
//  Copyright © 2018 Christian Kjaer Laustsen. All rights reserved.
//

import UIKit
import WebKit

class ViewController: UIViewController, WKUIDelegate, WKNavigationDelegate, WKScriptMessageHandler {
    
    var webView: WKWebView!
    @IBOutlet var containerView: UIView!
    @IBOutlet var firstLabel: UILabel!
    @IBOutlet var secondLabel: UILabel!
    @IBOutlet var offloadCounter: UILabel!
    var offloadNo: Int = 0
    var localNo: Int = 0
    
    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if message.name == "log" && message.body is String {
            print(message.body)
        } else if message.name == "labelOne" && message.body is String {
            firstLabel.text = message.body as? String
        } else if message.name == "labelTwo" && message.body is String {
            secondLabel.text = message.body as? String
        } else if message.name == "offloadCounter" && message.body is String {
            if message.body as? String == "1" {
                offloadNo += 1
            } else {
                localNo += 1
            }
            offloadCounter.text = "Offload Counter: 👍 \(offloadNo) / \(localNo) ✋"
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        // Reset the labels.
        firstLabel.text = "..."
        secondLabel.text = "..."
        
        // Set up JavaScript handlers.
        // Usage: window.webkit.messageHandlers.log.postMessage(messgeToPost);
        var webConfiguration: WKWebViewConfiguration {
            get {
                let webConfig: WKWebViewConfiguration = WKWebViewConfiguration()
                let userController: WKUserContentController = WKUserContentController()
                userController.add(
                    self as WKScriptMessageHandler,
                    name: "log")
                userController.add(
                    self as WKScriptMessageHandler,
                    name: "labelOne")
                userController.add(
                    self as WKScriptMessageHandler,
                    name: "labelTwo")
                userController.add(
                    self as WKScriptMessageHandler,
                    name: "offloadCounter")
                webConfig.userContentController = userController;
                return webConfig;
            }
        }
        webView = WKWebView(
            frame: CGRect(x: 0, y: 0,
                          width: view.frame.width,
                          height: containerView.frame.height),
            configuration: webConfiguration)
        view.addSubview(webView)
        
        let htmlPath = Bundle.main.path(
            forResource: "index",
            ofType: "html")
        let htmlUrl = URL(
            fileURLWithPath: htmlPath!,
            isDirectory: false)
        webView.loadFileURL(
            htmlUrl,
            allowingReadAccessTo: htmlUrl)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
}
