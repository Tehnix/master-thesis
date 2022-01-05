{-# LANGUAGE TemplateHaskell #-}
module TH where

import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax

import Data.List.Split (splitOn)

import Unsafe (offloadFunction)


-- | Use quasiquoting to call reduce boilerplate to the offload call, such as
-- `$(deriveOffload 'simpleFunction) 3`.
--
-- The following is happening in the Template Haskell below:
-- - We take a in the function name as a quasi-quoted argument (e.g. 'simpleFunction)
-- - We extract the string name of 'simpleFunction via `showName`
-- - We extract the original expression via `varE`
-- - Finally, we construct a call to offloadFunction via the `[e| ..|]` quasi-quoter.
deriveOffload :: Name -> Q Exp
deriveOffload name =
  [e|
    offloadFunction n $a
    |]
  where
    a = varE name
    n = showName name

-- | Create a mapping entry of the function call, and subsequently call
-- the function.
--
-- This also demonstrates that we are able to perform IO at compile time.
extendedDeriveOffload :: Name -> Q Exp
extendedDeriveOffload name = do
  let n = showName name
  -- Uncomment the line below to create the "FunctionMapping.txt" file.
  runIO $ appendFile "FunctionMapping.txt" (n ++ ":" ++ n ++ "\n")
  [e|offloadFunction n $(varE name)|]

-- | We create our own quasi-quoter, for a more convenient syntax, such as
-- `[off|simpleFunction|] 3`.
off :: QuasiQuoter
off = QuasiQuoter
  {
    -- Only handle expressions.
    quoteExp = \n -> do
      -- Extract the function name.
      let name = dropWhileEnd isSpace $ dropWhile isSpace n
      maybeName <- lookupValueName name
      -- Validate that it's in scope.
      case maybeName of
        Just name' -> extendedDeriveOffload name'
        Nothing -> fail $ "The function '" ++ name
                          ++ "' is either not in scope or"
                          ++ " does not exist"
  , quotePat = error "Does not support using as pattern"
  , quoteType = error "Does not support using as type"
  , quoteDec = error "Does not support using as declaration"
  }

-- | Convert the FunctionMapping file into a a `case` that maps the source
-- function into an actual function call.
deriveEndpoints :: String -> Q [Dec]
deriveEndpoints path = do
  let g [s, f] = (LitP $ StringL s, VarE (mkName f))
      g _      = error "Incorrect mapping, should be 's:f'"
  content <- runIO (readFile path)
  addDependentFile path -- Recompile on filechange.
  lcBody <- [e|error "Undefined mapping"|]
  let mappings = map (splitOn ":") (lines content)
      clauses = zipWith
        (\body pat -> Clause [pat] (NormalB body) [])
        (map (snd . g) mappings) (map (fst . g) mappings)
      lastClause = [Clause [VarP (mkName "s")] (NormalB lcBody) []]
  pure [FunD (mkName "endpoint") (clauses ++ lastClause)]
