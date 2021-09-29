{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Foldable (traverse_)
import Data.Proxy

class Monad m => Async m where
  spawn :: m () -> IO ()
  suspend :: ((a -> IO ()) -> IO ()) -> m a
  nowait :: IO a -> m a

newtype Future a = Future (MVar (FutureState a))

data FutureState a
  = Pending [a -> IO ()]
  | Fulfilled a

data FutureFulfilledException = FutureFulfilledException
  deriving (Show)

instance Exception FutureFulfilledException

emptyFuture :: IO (Future a)
emptyFuture = Future <$> newMVar (Pending [])

fulfill :: Future a -> a -> IO ()
fulfill (Future fstateVar) result = do
  fstate <- takeMVar fstateVar
  case fstate of
    Pending awaiters -> do
      putMVar fstateVar (Fulfilled result)
      traverse_ ($ result) awaiters
    Fulfilled a -> do
      putMVar fstateVar (Fulfilled a)
      throwIO FutureFulfilledException

getFuture :: Async m => Future a -> m a
getFuture (Future fstateVar) = do
  res <- nowait $ takeMVar fstateVar
  case res of
    Fulfilled r -> nowait (putMVar fstateVar (Fulfilled r)) >> pure r
    Pending awaiters -> suspend $ putMVar fstateVar . Pending . (: awaiters)

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
readFileAsync path = suspend ((pure () <*) . forkIO . (readFile path >>=)) -- forkIO here means any other runtime

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
