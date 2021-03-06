module Server where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newMVar, newEmptyMVar, takeMVar, putMVar, modifyMVar)
-- import Control.Exception ()
import Control.Monad (when, forM_, void)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Network.Socket (withSocketsDo)
import Network.WebSockets (Connection, forkPingThread, runServer, acceptRequest)
import System.IO (hPutStrLn, hPutStr, stderr)

import Network
import Model

roomSize :: Int
roomSize = 2

startServer :: Int -> IO ()
startServer port = do
    waitingRoom <- newMVar Map.empty
    withSocketsDo $ runServer "0.0.0.0" port \pendingConn -> do
        logLn "New pending connection."
        conn <- acceptRequest pendingConn
        logLn "Connection accepted."
        forkPingThread conn 30
        handshake conn waitingRoom
    where
    handshake conn waitingRoom = do
        closeSig <- newEmptyMVar
        Join name <- expect conn isJoin
        logLn $ "Player " ++ name ++ " tried to login."
        (joined, complete) <- modifyMVar waitingRoom \wr ->
            if Map.member name wr
            then return (wr, (False, Nothing))
            else let wr' = Map.insert name ((conn, Map.size wr), closeSig) wr
                in if Map.size wr' == roomSize
                    then return (Map.empty, (True, Just wr'))
                    else return (wr', (True, Nothing))
        logLn $ "Player " ++ name ++
            if joined then " has joined in."
            else " failed to login."
        serialize conn (JoinResp joined)
        case complete of
            Just wr -> void . forkFinally (startRoom (fst <$> wr)) $
                (\_ -> mapM_ (flip putMVar () . snd) (Map.elems wr)
                    >> logLn "Game ended.")
            Nothing -> return ()
        if joined
            then (takeMVar closeSig)
            else handshake conn waitingRoom

startRoom :: Map String (Connection, Int) -> IO ()
startRoom sessions = do
    logStr "New game started: "
    logList (Map.keys sessions)
    logStr ".\n"
    sendGameStarts
    gameBoard <- newIORef emptyBoard
    abandoned <- newIORef []
    while (canContinue <$> readIORef abandoned) do
        abans <- readIORef abandoned
        forM_ (aliveSessions abans) \(name, (conn, pid)) ->
            processCmd name conn pid gameBoard abandoned
    broadcast (const Bye)
    where
    sendGameStarts
        = forM_ (Map.elems sessions) \(conn, pid) ->
            serialize conn (GameStart (sideFromInt pid) (Map.keys sessions))
    processCmd name conn pid gameBoard abandoned = do
        logLn $ "Polling " ++ name ++ "."
        serialize conn Poll
        cmd <- expect conn isCommand
        logLn $ "Recieved command: [" ++ name ++ "] " ++ show cmd
        case cmd of
            Command Pass -> return ()
            Command Abandon -> modifyIORef abandoned (pid :)
            Command (Move coord) -> do
                oldBoard <- readIORef gameBoard
                case addPiece oldBoard (sideFromInt pid) coord of
                    Just newBoard -> do
                        writeIORef gameBoard newBoard
                        broadcast $ \pid ->
                            FullUpdate
                                (toPieceList newBoard)
                                coord
                                (visiblePieces newBoard (sideFromInt pid))
                    Nothing -> processCmd name conn pid gameBoard abandoned
    canContinue abans = Map.size sessions - length abans > 1
    aliveSessions abans = filter (\(_, (_, pid)) -> notElem pid abans) $ Map.assocs sessions
    broadcast f
        = forM_ (Map.elems sessions) \(conn, pid) ->
            serialize conn (f pid)
    toPieceList board
        = concatMap fromPg board
    fromPg (PieceGroup mems side)
        = map (\c -> (c, side)) mems

while :: Monad m => m Bool -> m () -> m ()
while cond action = do
    c <- cond
    if c then action >> while cond action
    else return ()

logLn :: String -> IO ()
logLn str = hPutStrLn stderr str

logStr :: String -> IO ()
logStr str = hPutStr stderr str

logList :: Show a => [a] -> IO ()
logList [a] = logStr (show a)
logList [] = return ()
logList (x : xs)
    = logStr (show x ++ ", ")
    >> logList xs
