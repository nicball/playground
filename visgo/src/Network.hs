module Network where

import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.ByteString.Lazy (ByteString)
import Network.WebSockets (Connection, sendTextData, receiveData)

import Model

data Message
    = Join String
    | JoinResp Bool
    | GameStart Side [String]
    | Command Command
    | FullUpdate [(Coord, Side)] Coord [(Coord, Side)]
    | Poll
    | Bye
    deriving (Show, Generic)

data Command
    = Move Coord | Pass | Abandon
    deriving (Show, Generic)

instance ToJSON Command
instance FromJSON Command
instance ToJSON Message
instance FromJSON Message
instance ToJSON Side
instance FromJSON Side

isJoin :: Message -> Bool
isJoin (Join _) = True
isJoin _ = False

isCommand :: Message -> Bool
isCommand (Command _) = True
isCommand _ = False

expect :: FromJSON a => Connection -> (a -> Bool) -> IO a
expect conn pred = do
    msg <- unserialize conn
    case pred <$> msg of
        Just True -> return (fromJust msg)
        _ -> expect conn pred

serialize :: ToJSON a => Connection -> a -> IO ()
serialize conn a
    = sendTextData conn . encode $ a

unserialize :: FromJSON a => Connection -> IO (Maybe a)
unserialize = fmap decode <$> receiveData
