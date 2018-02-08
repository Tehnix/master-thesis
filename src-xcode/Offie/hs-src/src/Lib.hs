module Lib where

import Foreign.C

foreign import ccall "Offie-Swift.h mySwiftFunc"
    mySwiftFunc :: CString -> CString

-- | export haskell function @chello@ as @hello@.
foreign export ccall "hello" chello
    :: IO CString

-- | Tiny wrapper to return a CString
chello :: IO CString
chello = do
    newCStr <- newCString "10"
    let s = mySwiftFunc newCStr
    newCString hello

-- | Pristine haskell function.
hello :: String
hello = "Hello from Haskell!!"
