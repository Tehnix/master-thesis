module Unsafe (offloadFunction, simpleFunction, main) where

import System.IO.Unsafe


-- | Stub for `shouldOffload`.
shouldOffload :: IO Bool
shouldOffload = pure True

-- | Stub for `offload`.
offload :: String -> (a -> b) -> a -> IO b
offload name f a = do
  -- Offload the function to the server,
  print $ "Offloading function " ++ name ++ "..."
  -- or support falling back to running it locally.
  pure $ f a

offloadFunction :: String -> (a -> b) -> a -> b
{-# NOINLINE offloadFunction #-}
offloadFunction name f a =
  if unsafePerformIO shouldOffload
    then unsafePerformIO $ offload name f a
    else f a

main :: IO ()
main = do
  print "Start:"
  print $ offloadFunction "simpleFunction" simpleFunction 3
  print "End!"

simpleFunction :: Int -> [Int]
simpleFunction a = map (+a) $ map (+2) [1,2,3]
