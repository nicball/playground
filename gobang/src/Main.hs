module Main where

import qualified Data.Map as M
import Control.Monad (forM_)
import Data.Maybe ()
import System.Environment (getArgs)
import Network.WebSockets (runClient, Connection)
import Data.List (find)
import System.IO (hFlush, stdout)

import Model
import Server
import Network

serverPort :: Int
serverPort = 8081

main :: IO ()
main = startServer serverPort
