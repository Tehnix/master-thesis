import Distribution.Simple
main = defaultMain

-- | https://github.com/ghcjs/jsaddle-hello
-- import Distribution.MacOSX
-- import Distribution.Simple

-- main :: IO ()
-- main = defaultMainWithHooks $ simpleUserHooks {
--          postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
--        }

-- guiApps :: [MacApp]
-- guiApps = [MacApp "jsaddle-wkwebview-test"
--                   Nothing
--                   (Just "macos/Info.plist")
--                   [] -- No other resources.
--                   [] -- No other binaries.
--                   DoNotChase -- Try changing to ChaseWithDefaults
--           ]
