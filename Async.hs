{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Proxy

class Monad m => Async m where
  spawn :: m () -> IO ()
  suspend :: ((a -> IO ()) -> IO ()) -> m a
  nowait :: IO a -> m a

data Future a = Future
  { _result :: MVar a
  , _awaiters :: MVar [a -> IO ()]
  , _mutex :: MVar ()
  }

data FutureFulfilledException = FutureFulfilledException
  deriving Show

instance Exception FutureFulfilledException

emptyFuture :: IO (Future a)
emptyFuture = Future <$> newEmptyMVar <*> newMVar [] <*> newMVar ()

fulfill :: Future a -> a -> IO ()
fulfill future result = withMVar (_mutex future) \_ -> do
  ok <- tryPutMVar (_result future) result
  if not ok then throwIO FutureFulfilledException else pure ()
  takeMVar (_awaiters future) >>= mapM_ ($ result)

getFuture :: Async m => Future a -> m a
getFuture future = do
  nowait $ takeMVar (_mutex future)
  res <- nowait $ tryReadMVar (_result future)
  case res of
    Just r -> nowait (putMVar (_mutex future) ()) >> pure r
    Nothing -> suspend \k -> modifyMVar_ (_awaiters future) (pure . (k :)) >> putMVar (_mutex future) ()

newtype ThreadAsync a = ThreadAsync (IO a)
  deriving (Functor, Applicative, Monad)

instance Async ThreadAsync where
  spawn (ThreadAsync io) = forkIO (io >> pure ()) >> pure ()
  suspend action = ThreadAsync do
    box <- newEmptyMVar
    action (putMVar box)
    takeMVar box
  nowait = ThreadAsync

newtype CpsAsync a = CpsAsync {unCpsAsync :: ContT () IO a}
  deriving (Functor, Applicative, Monad)

instance Async CpsAsync where
  spawn = evalContT . resetT . unCpsAsync
  suspend action = CpsAsync . shiftT $ \k -> lift (action k) >> pure ()
  nowait = CpsAsync . lift

asyncWork :: Async m => MVar () -> m ()
asyncWork finish = do
  content <- readFileAsync "./test.txt"
  nowait (putStrLn content)
  content <- readFileAsync "./test.txt"
  nowait (putStrLn content)
  nowait (putMVar finish ())

readFileAsync :: Async m => FilePath -> m String
readFileAsync path = suspend ((pure () <*) . forkIO . (readFile path >>=))

main :: IO ()
main = do
  finish <- newEmptyMVar
  putStrLn "A"
  spawn (asyncWork finish :: CpsAsync ())
  putStrLn "B"
  takeMVar finish

  future <- emptyFuture
  spawn (getFuture future >> nowait (putStrLn "heihei") :: ThreadAsync ())
  spawn (getFuture future >> nowait (putStrLn "houhou") :: CpsAsync ())
  fulfill future ()
  threadDelay 1000