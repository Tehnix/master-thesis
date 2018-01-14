module Unsafe where

import System.IO.Unsafe


shouldOffload :: IO Bool
shouldOffload = pure False

offload :: (a -> b) -> a -> IO b
offload f a = do
  print "Offloading function..."
  pure $ f a

offloadFunction :: (a -> b) -> a -> b
offloadFunction f a =
  if unsafePerformIO shouldOffload then unsafePerformIO $ offload f a else f a

main = do
  print "Start:"
  offloadFunction print "Hey"
  print "End!"
