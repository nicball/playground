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

serverAddr :: String
serverAddr = "127.0.0.1"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["server"] -> startServer serverPort
        ["client", name] -> startClient name
        _ -> putStrLn "Fuck you."

startClient :: String -> IO ()
startClient name = runClient serverAddr serverPort "/" \conn -> do
    serialize conn (Join name)
    Just (JoinResp ok) <- unserialize conn
    if ok
    then gameLoop conn
    else putStrLn "Fuck off!"

gameLoop :: Connection -> IO ()
gameLoop conn = do
    putStrLn "waiting server"
    Just msg <- unserialize conn
    putStrLn (show msg)
    case msg of
        Poll -> readCommand >>= serialize conn
        FullUpdate pieces -> printBoardLn pieces
        _ -> return ()
    gameLoop conn

printBoardLn :: [(Coord, Side)] -> IO ()
printBoardLn pieces = do
    forM_ [boardSize - 1, boardSize - 2 .. 0] \y ->
        forM_ [0 .. boardSize - 1] \x -> do
            case find ((== (x, y)) <$> fst) pieces of
                Just (_, side) -> printSide side
                Nothing -> putChar '+'
            if x == boardSize - 1
            then putChar '\n'
            else return ()
    putChar '\n'
    where
    printSide White = putChar 'O'
    printSide Black = putChar 'X'

readCommand :: IO Message
readCommand = do
    putStr "> " >> hFlush stdout
    line <- getLine
    case line of
        "pass" -> return (Command Pass)
        "abandon" -> return (Command Abandon)
        _ -> return (Command (Move (read line)))
