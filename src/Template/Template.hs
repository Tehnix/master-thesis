{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Template where

import Unsafe (offloadFunction, simpleFunction)
import TH

import Data.List.Split (splitOn)
import Data.List (intercalate)
import Language.Haskell.Interpreter


main :: IO ()
main = do
  -- Original.
  print $ offloadFunction "simpleFunction" simpleFunction 3
  -- Two new Template Haskell approaches.
  print $ $(deriveOffload 'simpleFunction) 3
  print $ [off|simpleFunction|] 3
  res <- endpoint' ("", "Unsafe.simpleFunction") " 3"
  print res

-- $(deriveEndpoints "FunctionMapping.txt")
-- endpoint :: Show a => String -> a -> [a]

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

endpoint' :: (String, String) -> String -> IO String
endpoint' (s,f) arg = do
  res <- runInterpreter $ interpreterEndpoint (s,f) arg
  case res of
    Left err -> do
      print err
      pure "Failed"
    Right e -> pure e
