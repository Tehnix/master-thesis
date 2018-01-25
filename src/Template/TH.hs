{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.List (dropWhileEnd, dropWhile)
import Data.Char (isSpace)

-- import System.Directory (doesFileExist)
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
  let n = showName name
  runIO $ appendFile "FunctionMapping.txt" (n ++ ":" ++ n ++ "\n")
  [e|offloadFunction n $(varE name)|]

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
  let g (s:f:[]) = (LitP $ StringL s, VarE (mkName f))
      g _ = error "Incorrect mapping, should be 's:f'"
  content <- runIO (readFile path)
  addDependentFile path -- Recompile on filechange.
  lcBody <- [e|error "Undefined mapping"|]
  let mappings = map (splitOn ":") (lines content)
      clauses = zipWith
        (\body pat -> Clause [pat] (NormalB body) [])
        (map (snd . g) mappings) (map (fst . g) mappings)
      lastClause = [Clause [VarP (mkName "s")] (NormalB lcBody) []]
  pure [FunD (mkName "endpoint") (clauses ++ lastClause)]
