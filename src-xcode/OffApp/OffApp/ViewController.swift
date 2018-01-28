//
//  ViewController.swift
//  OffApp
//
//  Created by Christian Kjaer Laustsen on 28/01/2018.
//  Copyright © 2018 Christian Kjaer Laustsen. All rights reserved.
//

import UIKit
import WebKit

class ViewController: UIViewController, WKUIDelegate {
    
    var webView: WKWebView!
    
    override func loadView() {
        let webConfiguration = WKWebViewConfiguration()
        webView = WKWebView(frame: .zero, configuration: webConfiguration)
        webView.uiDelegate = self
        view = webView
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        let htmlPath = Bundle.main.path(forResource: "index", ofType: "html")
        let htmlUrl = URL(fileURLWithPath: htmlPath!, isDirectory: false)
        webView.loadFileURL(htmlUrl, allowingReadAccessTo: htmlUrl)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

}
