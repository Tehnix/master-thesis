module Main where

import System.IO.Unsafe


shouldOffload :: IO Bool
shouldOffload = pure True

offload :: (a -> b) -> a -> IO b
offload f a = do
  print "Offloading function..."
  pure $ f a

offloadFunction :: (a -> b) -> a -> b
{-# NOINLINE offloadFunction #-}
offloadFunction f a =
  if unsafePerformIO shouldOffload then unsafePerformIO $ offload f a else f a

main :: IO ()
main = do
  print "Start:"
  print $ simpleFunction 3
  print "End!"


{-# RULES
"simpleFunction/offload simpleFunction" forall x.
    simpleFunction x = offloadFunction simpleFunction x
  #-}

simpleFunction :: Int -> [Int]
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
