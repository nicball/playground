module GetData (getData) where

import Control.Lens (filtered, folded, minimumOf, to, toListOf, view, (%~), (-~), (^..), (^?!), _1, _2)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, nth, values, _Integer, _String)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromJust)
import Data.Text as Text (Text, isSuffixOf, pack, unpack)
import Database.SQLite.Simple as Sql (Connection, executeMany, execute_, open, withExclusiveTransaction)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_)
import System.IO.Unsafe (unsafePerformIO)

{-
getSymbolList :: IO [Text]
getSymbolList = do
  minfo <- getResponseBody <$> httpJSON "https://api.binance.com/api/v3/exchangeInfo" :: IO Value
  pure $ minfo ^.. key "symbols" . values . key "symbol" . _String . filtered ("USDT" `isSuffixOf`)
-}

getKLines :: Text -> IO [(Integer, Double, Double)]
getKLines symbol = do
  let limit = 1000
  let rstr =
        "https://api.binance.com/api/v3/klines?symbol="
          <> Text.unpack symbol
          <> "&interval=1d&limit="
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
{-
normalize :: [(Integer, Text, Double, Double)] -> [(Integer, Text, Double, Double)]
normalize = offsetTime . coarserTime . ensureReliableCoin . ensureBigMarket
  where
    offsetTime = (fmap . (_1 -~)) =<< fromJust . minimumOf (folded . _1)
    coarserTime = fmap (_1 %~ (`div` (1000 * 3600)))
    ensureBigMarket = concat . filter ((> 300) . length) . groupBy ((==) `on` view _1) . sortOn (view _1)
    ensureReliableCoin = concat . filter ((> 500) . length) . groupBy ((==) `on` view _2) . sortOn (view _2)
-}

getData :: Connection -> IO ()
getData sqlConn = do
  execute_ sqlConn "create table market (time integer, symbol text, open real, close real, unique (time, symbol))"
  withExclusiveTransaction sqlConn $
      let s = "BTCUSDT" in getKLines s >>= insertAll . fmap (\(t, o, c) -> (t, s, o, c))
  where
    insertAll :: [(Integer, Text, Double, Double)] -> IO ()
    insertAll = executeMany sqlConn "insert into market values (?, ?, ?, ?)"
