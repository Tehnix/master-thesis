module Rewrite where

import Unsafe (offloadFunction)

main :: IO ()
main = do
  print "Start:"
  print $ simpleFunction 3
  print "End!"

-- | The rewrite rule wraps all calls to `simpleFunction` with `offloadFunction`.
{-# RULES
"simpleFunction/offload simpleFunction" forall x.
    simpleFunction x = offloadFunction "simpleFunction" simpleFunction x
  #-}

simpleFunction :: Int -> [Int]
{-# NOINLINE simpleFunction #-}
simpleFunction a = map (+a) $ map (+2) [1,2,3]

{-
λ src/Rewrite ‹master!› stack exec -- ghc Main.hs -O2 -ddump-rule-firings
Rule fired: Class op pure (BUILTIN)
Rule fired: SPEC $fShow[] (GHC.Show)
Rule fired: Class op >> (BUILTIN)
Rule fired: Class op show (BUILTIN)
Rule fired: unpack (GHC.Base)
Rule fired: Class op pure (BUILTIN)
Rule fired: Class op + (BUILTIN)
Rule fired: Class op + (BUILTIN)
Rule fired: map (GHC.Base)
Rule fired: fold/build (GHC.Base)
Rule fired: map (GHC.Base)
Rule fired: fold/build (GHC.Base)
Rule fired: mapFB (GHC.Base)
Rule fired: mapFB (GHC.Base)
Rule fired: mapFB (GHC.Base)
Rule fired: Class op >> (BUILTIN)
Rule fired: Class op show (BUILTIN)
Rule fired: unpack (GHC.Base)
Rule fired: Class op >> (BUILTIN)
Rule fired: SPEC $fShow[] (GHC.Show)
Rule fired: Class op show (BUILTIN)
Rule fired: simpleFunction/offload simpleFunction (Main) <--- our rewrite rule
Rule fired: Class op show (BUILTIN)
Rule fired: unpack (GHC.Base)
Rule fired: unpack-list (GHC.Base)
Rule fired: unpack-list (GHC.Base)
Rule fired: unpack-list (GHC.Base)
Rule fired: +# (BUILTIN)
Rule fired: +# (BUILTIN)
Rule fired: +# (BUILTIN)
Rule fired: +# (BUILTIN)
Rule fired: +# (BUILTIN)
Rule fired: +# (BUILTIN)
Linking Main ...
-}
