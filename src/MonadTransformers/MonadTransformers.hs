{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             GeneralizedNewtypeDeriving,
             DeriveFunctor #-}
module MonadTransformers where

import Data.Proxy
import Control.Monad.State


newtype OffloaderT o m a = OffloaderT { runOffloaderT :: m (a, o) }

class MonadOffloader o m where
  offload :: (Show a, Show o) => (a -> o) -> a -> m o

-- Base case.
instance (Monad m) => MonadOffloader o (OffloaderT o m) where
  offload f a = offload f a

-- Supporting `StateT`.
instance (Monad m, MonadOffloader o m) => MonadOffloader o (StateT s m) where
  offload f a = lift $ offload f a

-- -- Custom instance for client-side.
-- instance MonadOffloader Output Client where
--   offload f a = do
--     liftIO $ print "Performing offload"
--     return $ f a

-- Our carrier type.
newtype Client a = Client (OffloaderT Output IO a)

data Output = Output String deriving (Show)

class MonadOperations m where
  writeOutput :: String -> m ()
  getInput :: m String

class MonadRemotableOperations m where
  computation :: Int -> Int -> m Int



main :: IO ()
main = print "test"
