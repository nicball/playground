module GetData (getData) where

import Control.Lens (filtered, to, toListOf, (^..), (^?!))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, nth, values, _String)
import Data.Foldable (traverse_)
import Data.Text as Text (Text, isSuffixOf, pack, unpack)
import Database.SQLite.Simple as Sql (Connection, executeMany, execute_, open, withExclusiveTransaction)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_)
import System.IO.Unsafe (unsafePerformIO)

getSymbolList :: IO [Text]
getSymbolList = do
  minfo <- getResponseBody <$> httpJSON "https://api.binance.com/api/v3/exchangeInfo" :: IO Value
  pure $ minfo ^.. key "symbols" . values . key "symbol" . _String . filtered ("USDT" `isSuffixOf`)

getKLines :: Text -> IO [(Integer, Double, Double)]
getKLines symbol = do
  let limit = 1000
  let rstr =
        "https://api.binance.com/api/v3/klines?symbol="
          <> Text.unpack symbol
          <> "&interval=1h&limit="
          <> show limit
  putStrLn $ "Getting " <> rstr
  klines <- getResponseBody <$> httpJSON (parseRequest_ rstr) :: IO Value
  pure
    . fmap ((,,) <$> nthField 0 <*> nthField 1 <*> nthField 4)
    . toListOf values
    $ klines
  where
    nthField :: Read a => Int -> Value -> a
    nthField n value = value ^?! nth n . _String . to (read . Text.unpack)

getData :: Connection -> IO ()
getData sqlConn = do
  execute_ sqlConn "create table market (time integer, symbol text, open real, close real, unique (time, symbol))"
  withExclusiveTransaction sqlConn $
    getSymbolList
      >>= traverse (\s -> (s,) . fmap (s,) <$> getKLines s)
      >>= traverse_ insertAll
  where
    insertAll (s, rows) = do
      putStrLn . Text.unpack $ "Inserting " <> s
      executeMany sqlConn "insert into market values (?, ?, ?, ?)" . fmap (\(s, (t, o, c)) -> (t, s, o, c)) $ rows
