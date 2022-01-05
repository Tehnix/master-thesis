{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module MonadTransformers where

import Control.Monad.IO.Class
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Writer (WriterT)


main :: IO ()
main = do
  -- We are running (interpreting) the same program, but are able to make different choices
  -- depending on where it runs.
  runClient . runReaderT program $ Env { envHost = "localhost" }
  runServer . runReaderT program $ Env { envHost = "remote" }

-- Our environment definition for our program (accessible in the program).
data Env = Env { envHost :: String }

-- Our actual business logic.
program :: (MonadEffects m, MonadReader Env m) => m ()
program = do
  env <- ask
  inp <- getInput
  writeOutput $ "Input: " ++ inp ++
                ", Hostname: " ++ envHost env
  res <- computation 12 22
  writeOutput res


-- | The general interface for our effects.
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

-- | Boilerplate: Default instances to make it compatible with common MTL classes.
instance MonadEffects m => MonadEffects (LoggingT m)
instance MonadEffects m => MonadEffects (ReaderT r m)
instance MonadEffects m => MonadEffects (StateT s m)
instance (MonadEffects m, Monoid w) => MonadEffects (WriterT w m)


-- | Our client IO instances.
newtype Client m a = Client { runClient :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)


-- | The actual important part: Our definition of Client effects.
instance MonadEffects (Client IO) where
  getInput = Client $ pure "Fake Input"
  writeOutput = Client . print
  computation i1 i2 = do
    liftIO $ print "Offloading" -- Decision logic.
    Client . pure $ i1 + i2 -- Offloading (or not) of the computation.


-- | Our server IO instances.
newtype Server m a = Server { runServer :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | The actual important part: Our definition of Server effects.
instance MonadEffects (Server IO) where
  getInput = Server $ pure "Fake Input"
  writeOutput = Server . print
  computation i1 i2 =
    Server . pure $ i1 + i2
