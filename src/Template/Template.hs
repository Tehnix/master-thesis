{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Template where

import TH
import Unsafe (offloadFunction, simpleFunction)

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Language.Haskell.Interpreter


main :: IO ()
main = do
  -- Original.
  print $ offloadFunction "simpleFunction" simpleFunction 3

  -- Two new Template Haskell approaches.
  print $ $(deriveOffload 'simpleFunction) 3
  print $ [off|simpleFunction|] 3

  -- Calling a Haskell function via textual input.
  print "Interpret the received instruction and map it to a function call:"
  res <- endpoint' "Unsafe.simpleFunction" " 3"
  print res

-- NOTE: A problem with implementiong `deriveEndpoints` is that we are generating
-- a function that redirects to other functions, and as such we need to have a
-- uniform type signature. This quickly breaks down the program when you go
-- beyond trivial cases, although we could alleviate some of this with some
-- typeclass trickery and existential types.
--
-- $(deriveEndpoints "FunctionMapping.txt")
-- endpoint :: Show a => String -> a -> [a]

-- | Extremely basic interpreter that converts the textual function call
-- into a Haskell function call.
interpreterEndpoint :: (String, String) -> String -> Interpreter String
interpreterEndpoint (_,f) arg = do
  -- Extract the module name.
  let splitFn = splitOn "." f
      fnName = last splitFn
      moduleName = intercalate "." $ init splitFn
  if null moduleName
    then setImports ["Prelude"]
    else setImports ["Prelude", moduleName]
  eval $ fnName ++ arg

endpoint' :: String -> String -> IO String
endpoint' f arg = do
  res <- runInterpreter $ interpreterEndpoint ("", f) arg
  case res of
    Left err -> do
      print err
      pure "Failed"
    Right e -> pure e
