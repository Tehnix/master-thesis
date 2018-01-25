{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module MonadTransformers where

import Control.Monad.Reader (MonadReader(..), runReaderT, ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (LoggingT)
import Control.Monad.IO.Class


main :: IO ()
main = do
  runClient . runReaderT program $ Env { envHost = "localhost" }
  runServer . runReaderT program $ Env { envHost = "remote" }

data Env = Env { envHost :: String }

program :: (MonadEffects m, MonadReader Env m) => m ()
program = do
  env <- ask
  inp <- getInput
  writeOutput $ "Input: " ++ inp ++
                ", Hostname: " ++ envHost env
  res <- computation 12 22
  writeOutput res


-- The general interface for our effects.
class (Monad m) => MonadEffects m where
  writeOutput :: Show a => a -> m ()
  getInput :: m String
  computation :: Int -> Int -> m Int

  -- Provide a default implementation for empty instances.
  default writeOutput :: (MonadTrans t, MonadEffects m', m ~ t m', Show a) => a -> m ()
  writeOutput = lift . writeOutput
  default getInput :: (MonadTrans t, MonadEffects m', m ~ t m') => m String
  getInput = lift getInput
  default computation :: (MonadTrans t, MonadEffects m', m ~ t m') => Int -> Int -> m Int
  computation i1 i2 = lift $ computation i1 i2

-- Default instances to make it compatible with common MTL classes.
instance MonadEffects m => MonadEffects (LoggingT m)
instance MonadEffects m => MonadEffects (ReaderT r m)
instance MonadEffects m => MonadEffects (StateT s m)
instance (MonadEffects m, Monoid w) => MonadEffects (WriterT w m)


-- Our client IO instances.
newtype Client m a = Client { runClient :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance MonadEffects (Client IO) where
  getInput = Client $ pure "Fake Input"
  writeOutput = Client . print
  computation i1 i2 = do
    liftIO $ print "Offloading"
    Client . pure $ i1 + i2


-- Our server IO instances.
newtype Server m a = Server { runServer :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEffects (Server IO) where
  getInput = Server $ pure "Fake Input"
  writeOutput = Server . print
  computation i1 i2 =
    Server . pure $ i1 + i2
