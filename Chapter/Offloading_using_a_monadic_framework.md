# Offloading Using a Monadic Framework {#ch:monadic_framework}
If it was not clear from \cref{ch:approaches} (and the title), we have chosen to implement our offloading system using the monadic framework approach, specifically using `Freer`, as it gave both better ergonomics, composability, extensibility and performance than `Free`, and allowed for a cleaner separation than \gls{mtl}.

\ \

The implementation will be divided into three main components:

- Common: The shared code between the client and the server, e.g. types and offloadable functions.
- Frontend: Our client code that will reside on the mobile device.
- Backend: Our server code, which will be run in an cloud environment.

## Common
We have a lot of shared code between the frontend and backend. Our data types for the program, along with functions to send them into the `Freer` monad. We also have our computations that both the frontend and backend are able to perform, depending on if the code is offloaded or not. As such, we provide a common layer between this two, by structuring it as a package, that they can both import. This ensures consistency in both the data types, effects and computations that can be run on the frontend and backend, along with keeping the algorithms the same for both.

First, let us set up the types that we will use to model our program. We divide these into two different data types, to have a clear separation of the logic. In [@lst:off_common_types] we define `Operation` which will represent effectful operations that we do not want to offload, while `Computation` will represent the pure code we want to offload. We also create a `ComputationRepr` to represent the functions when we send them over the network.

```{#lst:off_common_types .haskell}
{-# LANGUAGE ConstraintKinds #-}
module Common.Types where

type Serialize a = Show a

data Operation next where
  WriteOutput :: Serialize a => a -> Operation ()

data Computation next where
  IsPrime :: Int -> Computation Bool
  FactorialLength :: Int -> Computation Int

data ComputationRepr
  = IsPrimeRepr
  | FactorialLengthRepr
  deriving (Show, Read)
```

: Defining our data types that will be used to model the program

The operations and computations are intentionally kept simple, since the point is to demonstrate the usage of this approach, and not the performance of it. A output function is enough to model effectful behaviour and we can turn our `IsPrime` and `Factorial` into compute heavy functions later on, while keeping the interface simple.

We proceed in [@lst:off_common_functions] by defining the functions that turn our \gls{gadt} constructors into effects, which we will be using in the program.

```{#lst:off_common_functions .haskell}
{-# LANGUAGE FlexibleContexts #-}
module Common.Operations where
import Control.Monad.Freer
import Common.Types

writeOutput :: (Member Operation effs, Serialize a) =>
            a -> Eff effs ()
writeOutput = send . WriteOutput

isPrime :: Member Computation effs => Int -> Eff effs Bool
isPrime = send . IsPrime

factorialLength :: Member Computation effs =>
                Int -> Eff effs Int
factorialLength = send . Factorial
```

: Functions to convert our GADTs into effects

Finally, our computations are by design slightly inefficient, scaling with time on the input. Shown in [@lst:off_common_computation] we define `computeFactorialLength` to calculate the factorial of $n$, and return the number of digits in the result. The `computeIsPrime` function is a naïve primality test, which checks if $n$ is divisible up to square root of n.

```{#lst:off_common_computation .haskell}
module Common.Computation where

computeFactorialLength :: Int -> Int
computeFactorialLength n = length $ show $ product [1..11000]

computeIsPrime :: Int -> Bool
computeIsPrime n = null [ p | p <- [2..sqrt (fromIntegral n)]
                            , n `mod` p  == 0]
```

: The implementation of `FactorialLength` and `IsPrime`

The frontend and backend can now pull in this code, to carry out the computations on each side.


## Frontend
Our frontend will consists of some simple \gls{ui} code, and then an interpreter that will allow us to run our computations and decide if we want to offload it. The frontend consist of:

- \gls{ui} code.
- Communication to Swift/Native mobile code.
- Our offloading interpreter for our operations and computations.
- A profiler that keeps track of our network conditions.

Our \gls{ui} is merely to let the user know that the app has launched, and not much else. We are using `reflex-dom`[^reflexdom] to setup the interface, as shown in

[^reflexdom]: Can be found in https://github.com/reflex-frp/reflex-platform

```{#lst:off_frontend_ffi .haskell}
module UI where
import Reflex.Dom
import Data.Map as Map
import Data.Monoid


headElem :: MonadWidget t m => m ()
headElem = do
  el "title" $ text "Offie The Offloader"
  elAttr "meta" (Map.fromList [("charset", "UTF-8")]) $ return ()
  styleSheet "styles.css"
  where
    styleSheet name = elAttr "link" (
          ("rel" =: "stylesheet")
        <> ("type" =: "text/css")
        <> ("href" =: name))
        blank

bodyElem :: MonadWidget t m => m ()
bodyElem = elClass "div" "main-component" $ do
  rec
    el "h1" $ text "Welcome to Offie The Offloader"
    el "p" $ text "Tests will start automatically."
  blank
```

: A simple user interface for the offloading app

To communicate with Swift, we must set up some GHCJS \gls{ffi} calls into JavaScript, where the function to call Swift will live. The Swift code will be explained later, in [@sec:off_comm_to_swift]. As shown in [@lst:off_frontend_ffi] we set up some conditional \gls{ffi} code. If we are using GHCJS it adds the \gls{ffi} call, and if we are on plain GHC, then it simply prints the output.

```{#lst:off_frontend_ffi .haskell}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}
module JavaScriptFFI (logToiOS) where
#ifdef ghcjs_HOST_OS
import Data.JSString (JSString, pack)

foreign import javascript unsafe
  "if (typeof window.webkit === 'undefined') { window.webkit =
    {messageHandlers: {log: {postMessage: console.log }}}};
    window.webkit.messageHandlers.log.postMessage($1)"
  iOSLog :: JSString -> IO ()
foreign import javascript unsafe
  "if (typeof window.webkit === 'undefined') { window.webkit =
    {messageHandlers: {log: {postMessage: console.labelOne }}}};
    window.webkit.messageHandlers.labelOne.postMessage($1)"
  iOSLabel1 :: JSString -> IO ()
foreign import javascript unsafe
  "if (typeof window.webkit === 'undefined') { window.webkit =
    {messageHandlers: {log: {postMessage: console.labelTwo }}}};
    window.webkit.messageHandlers.labelTwo.postMessage($1)"
  iOSLabel2 :: JSString -> IO ()

logToiOS :: String -> IO ()
logToiOS s = iOSLog (pack s)

setiOSLabel1 :: String -> IO ()
setiOSLabel1 s = iOSLabel1 (pack s)

setiOSLabel2 :: String -> IO ()
setiOSLabel2 s = iOSLabel2 (pack s)
#else
logToiOS :: String -> IO ()
logToiOS s = putStrLn s

setiOSLabel1 :: String -> IO ()
setiOSLabel1 s = putStrLn s

setiOSLabel2 :: String -> IO ()
setiOSLabel2 s = putStrLn s
#endif
```

: GHCJS FFI into JavaScript which communicates to the Swift part

Our interpreters are set up to call the compute functions defined in the common package, as shown in [@lst:off_frontend_effects] and [@lst:off_frontend_main_computations]

```{#lst:off_frontend_effects .haskell}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.Freer

import Common.Operations
import Common.Types
import Common.Computation
import JavaScriptFFI
import Network.Wreq
import Control.Lens
import Profiler (shouldOffload)
import Text.Read (readMaybe)

offload :: String -> String -> IO String
offload fun val = do
  r <- get url
  r ^. responseBody
  where url = "http://localhost:8080/off?f=" ++ fun ++ "&v=" ++ val

program :: Eff '[Operation, Computation, IO] ()
program = do
 fac <- factorialLength 22
 writeOutput fac
 result <- isPrime 12
 writeOutput result

runProgramM :: Eff '[Operation, Computation, IO] a -> IO a
runProgramM p = runM . runComputationM . runEventM $ p

runEventM :: forall effs a. LastMember IO effs => Eff (Operation ': effs) a -> Eff effs a
runEventM = interpretM $ \case
  WriteOutput s -> logToiOS (show s)
```

: The interpreters for our events

```{#lst:off_frontend_main_computations .haskell}
runComputationM :: forall effs a. LastMember IO effs => Eff (Computation ': effs) a -> Eff effs a
runComputationM = interpretM $ \case
  IsPrime i -> do
    shouldOffload' <- shouldOffload
    if shouldOffload'
    then do
      c <- offload (show IsPrimeRepr) (show i)
      setiOSLabel1 c
      case (readMaybe c) :: Maybe Int of
        Nothing -> pure 0
        Just i -> pure i
    else do
      let c = computeIsPrime i
      setiOSLabel1 (show c)
      pure c
  FactorialLength i -> do
    shouldOffload' <- shouldOffload
    if shouldOffload'
    then do
      c <- offload (show FactorialLengthRepr) (show i)
      setiOSLabel2 c
      case (readMaybe c) :: Maybe Int of
        Nothing -> pure 0
        Just i -> pure i
    else do
      let c = computeFactorialLength i
      setiOSLabel2 (show c)
      pure c
```

: The interpreters for our computations

We run the \gls{ui} and the interpreters concurrently in the main function, as shown in [@lst:off_frontend_main].

```{#lst:off_frontend_main .haskell}
module Main where

import Control.Concurrent.Async.Lifted.Safe
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import Reflex.Dom.Core (mainWidgetWithHead)

import UI
import Interpreter

main :: IO ()
main = do
  concurrently_
    (runProgramM program)
    (JSaddle.run 3709 $ mainWidgetWithHead headElem bodyElem)
```

: Running the frontend


## Backend
The backend is responsible for accepting requests from clients, and then making sure they are run using the correct functions before they return their result back to the client. As such, our backend consists of:

- A router in accepting HTTP requests and passing them on.
- An interpreter that can evaluate our incoming requests.

We set up our backend as a simple web server that takes in a `ComputationRepr` and a value, and returns the calculated value, as shown in [@lst:off_backend_main].


```{#lst:off_backend_main .haskell}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Text
import Data.Time (UTCTime)
import Servant.API
import Data.Proxy
import Control.Monad.Trans.Either
import Network.Wai.Handler.Warp

import Common.Types
import Common.Computation

main :: IO ()
main = run 8080 (serve api server)

type Handler a = EitherT ServantErr IO a

type OffloadApi = "off" :> QueryParam "f" ComputationRepr
                        :> QueryParam "v" Int :> Get Int

api :: Proxy OffloadApi
api = Proxy

server :: Server OffloadApi
server = compute
  where
    compute mFun val =
      case mFun of
        Nothing -> "ERROR: Needs a function"
        Just IsPrimeRepr -> computeIsPrime val
        Just FactorialLengthRepr -> computeFactorialLength val
```

This is of course a simplification of what a real system should do, but it will suffice for now.


## Getting the Code onto a Mobile Device
Now that we have written our implementation, we want to get it onto a mobile device. There are a couple of ways to go about this:

- Generate native code: Still _very_ experimental, better support for cross-compilation will most likely land in GHC 8.4. This would be the most desirable approach, if the ecosystem eventually matures, as we can compile both for iOS and Android via this.
- Generate Java via Eta: Eta is a port of GHC that runs on the JVM. This means it is able to generate JAR files, and supports Java via \gls{ffi}. Unfortunately this will only be able to run on Android devices, since it is the only device that runs Java.
- Generate JavaScript via GHCJS: Much like Eta, GHCJS is also a port. GHCJS compiles to clean (i.e. readable) JavaScript, and also supports NodeJS. Compiling to JavaScript means we can target a WebView---an integrated browser environment---on the mobile platform. This provides the broadest compatibility, and even allows the app to run on the web, if so desired.

As an alternative to GHCJS, one could use PureScript to target mobile via WebViews. PureScript was made specifically to compile to JavaScript, and also provides a sufficiently advanced type system to compete with Haskell. Idris is another alternative, but it has a rather immature JavaScript backend as of current. Finally, if one has simpler needs, Elm also targets JavaScript and is a purely functional programming language.

With these choices, we have opted to go for generating JavaScript via GHCJS. We will therefore need a little setup code done in our mobile platform, that will load our index.html files and our JavaScript files. This does mean that we incur a slight performance penalty, at the cost of flexibility in development. That said, using JavaScript on for mobile app development is certainly not an unusual thing, with large companies like Facebook using it, along with a lot of other companies.


### Setting up Xcode/iOS
We are going to demonstrate how to get our generated JavaScript to run on iOS via a WebView, specifically the `WKWebView` component.

First off, we create an Xcode project, by choosing a `Single View App`, as shown in [@fig:off_xcode_setup]. In the following screen, will name the app `OffApp`, and choose `Swift` as our language.

![Xcode project setup](Graphic/XcodeProjectSetup.png "Xcode project setup"){#fig:off_xcode_setup width=100% }

We should now have a project structure that looks something like the following:

```
OffApp
|__ AppDelegate.swift
|__ Assets.xcassets
|   |__ AppIcon.appiconset
|       |__ Contents.json
|── Base.lproj
|   |__ LaunchScreen.storyboard
|   |__ Main.storyboard
|__ Info.plist
|__ ViewController.swift
```

We will then create a group called `Web` in the `OffApp` folder, and put in our `index.html`, `styles.css` and our JavaScript files. Our new structure looks like:

```
OffApp
|__ ...
|__ Web
    |__ index.html
    |__ styles.css
    |__ rts.js
    |__ out.js
    |__ lib.js
    |__ runmain.js
```

Getting the Swift code to load our `index.html` into a WebView is fairly simple, as shown in [@lst:off_xcode_swift]. First though, we create a `Container View` and two labels in our Main.storyboard, and connect them to our `ViewController`. Then we import `WebKit`, and then instantiate the `WKWebView` and add it to the `containerView`. Finally, we set the page to load in `viewDidLoad`, by giving the webview the path to our `index.html` file.

```{#lst:off_xcode_swift .swift}
import UIKit
import WebKit

class ViewController: UIViewController, WKUIDelegate,
                      WKNavigationDelegate {
    var webView: WKWebView!
    @IBOutlet var containerView: UIView!
    @IBOutlet var firstLabel: UILabel!
    @IBOutlet var secondLabel: UILabel!

    override func viewWillAppear(_ animated: Bool) {
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

    // ...
}
```

: Setting up a `WKWebView` in Swift to load our `index.html`

### Communicating from JavaScript to Swift {#sec:off_comm_to_swift}

We can also extend the WebView to respond to JavaScript events. This is done by setting up a controller that will handle incoming calls, placed under `window.webkit.messageHandlers.EVENTNAME.postMessage(MESSAGE)` in JavaScript. This is done by changing `let webConfiguration = WKWebViewConfiguration()` from `loadView` to the code in [@].

```{#lst:off_xcode_swift_config .swift}
var webConfiguration: WKWebViewConfiguration {
    get {
        let webConfig: WKWebViewConfiguration
               = WKWebViewConfiguration()
        let userController: WKUserContentController
               = WKUserContentController()
        userController.add(
            self as WKScriptMessageHandler,
            name: "log")
        webConfig.userContentController = userController;
        return webConfig;
    }
}
```

: Changing the `WKWebView` configuration to attach a script controller

We then extend our `ViewController` class with `WKScriptMessageHandler` and add a method `userContentController`, which will handle the incoming requests. In [@lst:off_xcode_swift_handler_log] we set up the handler to `print` incoming messages of type `log` to the Swift console.

```{#lst:off_xcode_swift_handler_log .swift}
func userContentController(_ userContentController:
     WKUserContentController, didReceive message: WKScriptMessage) {
    if message.name == "log" && message.body is String {
        print(message.body)
    }
    if message.name == "labelOne" && message.body is String {
        firstLabel.text = message.body as? String
    }
    if message.name == "labelTwo" && message.body is String {
        secondLabel.text = message.body as? String
    }
}
```

: Set up handler for JavaScript events

Finally, we can compile the application and run in on iOS, as shown in [@fig:off_ios_app].

![iOS App Running Offie](Graphic/iOSApp.png "iOS App Running Offie"){#fig:off_ios_app width=70% }


## Summary
We have fairly easily structured together a program that mixes \gls{ui} code, and our `Freer` interpreters. The small demo can easily be polished, but this was just a proof-of-concept. Ideally, the interpreters would also be responsible for generating the \gls{ui} code, and then we can run the whole program in the interpreters, giving it more power.

By generating JavaScript we have an easy way to get our pure functional code onto several mobile platforms, and can even interact with the native code part through JavaScript \gls{ffi} calls from our code.
