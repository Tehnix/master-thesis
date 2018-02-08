import UIKit


@_silgen_name("mySwiftFunc")
public func mySwiftFunc(number: String) -> String {
    print("Hello from Swift: \(number)")
    return "77"
}

class ViewController: UIViewController {
    
    @IBOutlet var helloWorldLabel: UILabel!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        print("View is loading...")
        helloWorldLabel.text = String(cString: hello())
        print("Done loading")
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
}
