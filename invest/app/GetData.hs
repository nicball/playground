module GetData (getData) where

import Control.Lens (filtered, to, toListOf, (^..), (^?!), (%~), _1, folded, minimumOf, (-~))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, nth, values, _String, _Integer)
import Data.Foldable (traverse_)
import Data.Text as Text (Text, isSuffixOf, pack, unpack)
import Data.Maybe (fromJust)
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
    . fmap ((,,) <$> nthField 0 _Integer <*> nthField 1 (_String . toRead) <*> nthField 4 (_String . toRead))
    . toListOf values
    $ klines
  where
    nthField n ty value = value ^?! nth n . ty
    toRead = to (read . Text.unpack)

normalize :: [(Integer, Text, Double, Double)] -> [(Integer, Text, Double, Double)]
normalize = ((fmap . (_1 -~)) =<< fromJust . minimumOf (folded . _1)) . fmap (_1 %~ (`div` (1000 * 3600)))

getData :: Connection -> IO ()
getData sqlConn = do
  execute_ sqlConn "create table market (time integer, symbol text, open real, close real, unique (time, symbol))"
  withExclusiveTransaction sqlConn $
    getSymbolList
      >>= traverse (\s -> fmap (\(t, o, c) -> (t, s, o, c)) <$> getKLines s)
      >>= insertAll . normalize . concat
  where
    insertAll :: [(Integer, Text, Double, Double)] -> IO ()
    insertAll = executeMany sqlConn "insert into market values (?, ?, ?, ?)"
