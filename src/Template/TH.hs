{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.List (dropWhileEnd, dropWhile)
import Data.Char (isSpace)

import System.Directory (doesFileExist)
import Data.List.Split (splitOn)

import Unsafe (offloadFunction)


deriveOffload :: Name -> Q Exp
deriveOffload name =
  [e|
    offloadFunction n $a
    |]
  where
    a = varE name
    n = showName name

extendedDeriveOffload :: Name -> Q Exp
extendedDeriveOffload name = do
  runIO storeMapping
  [e|
    offloadFunction n $a
    |]
  where
    a = varE name
    n = showName name
    fileName = "FunctionMapping.txt"
    storeMapping = do
      fileExists <- doesFileExist fileName
      -- For now, the name and function are identical.
      if fileExists
        then appendFile fileName (n ++ ":" ++ n ++ "\n")
        else writeFile fileName (n ++ ":" ++ n ++ "\n")

off :: QuasiQuoter
off = QuasiQuoter
  { quoteExp = \n -> do
      let name = dropWhileEnd isSpace $ dropWhile isSpace n
      maybeName <- lookupValueName name
      case maybeName of
        Just name' -> extendedDeriveOffload name'
        Nothing -> fail $ "The function '" ++ name
                          ++ "' is either not in scope or"
                          ++ " does not exist"
  , quotePat = error "Doest not support using as pattern"
  , quoteType = error "Doest not support using as type"
  , quoteDec = error "Doest not support using as declaration"
  }

deriveEndpoints :: String -> Q [Dec]
deriveEndpoints path = do
  content <- runIO (readFile path)
  -- Recompile on filechange.
  addDependentFile path
  -- Set up all the cases.
  let mappings = map (splitOn ":") (lines content)
  let name = mkName "endpoint"
      patterns = map stringToPat mappings
      fnBodies = map stringToExp mappings
      clauses = zipWith (\body pat -> Clause [pat] (NormalB body) []) fnBodies patterns
  -- Handle the catch-all last clause.
  lastClauseBody <- [e|error "Undefined mapping"|]
  let s' = mkName "s"
      lastClause = [Clause [VarP s'] (NormalB lastClauseBody) []]
  pure [FunD name (clauses ++ lastClause)]
  where
    stringToPat :: [String] -> Pat
    stringToPat (s:_) = LitP $ StringL s
    stringToExp :: [String] -> Exp
    stringToExp (_:f:[]) = VarE (mkName f)
