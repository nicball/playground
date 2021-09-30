module GetData where

import Control.Lens ((^..), (^?!), filtered, to, toListOf)
import Data.Aeson (Value)
import Data.Aeson.Lens (values, _String, key, nth)
import Data.Foldable (traverse_)
import Data.Text as Text (Text, pack, unpack, isSuffixOf)
import Database.SQLite.Simple as Sql (open, Connection, executeMany, execute_, withExclusiveTransaction)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_)
import System.IO.Unsafe (unsafePerformIO)

getSymbolList :: IO [Text]
getSymbolList = do
  minfo <- getResponseBody <$> httpJSON "https://api.binance.com/api/v3/exchangeInfo" :: IO Value
  pure $ minfo ^.. key "symbols" . values . key "symbol" . _String . filtered ("USDT" `isSuffixOf`)

getKLines :: Text -> IO (Maybe [(Integer, Double, Double)])
getKLines symbol = do
  let limit = 1000
  let rstr =
        "https://api.binance.com/api/v3/klines?symbol="
          <> Text.unpack symbol
          <> "&interval=1h&limit="
          <> show limit
  putStrLn $ "Getting " <> rstr
  klines <- getResponseBody <$> httpJSON (parseRequest_ rstr) :: IO Value
  let res = Prelude.zipWith (\t (o, c) -> (t, o, c)) [0 .. limit - 1] . fmap ((,) <$> (^?! nth 1 . _String . to (read . Text.unpack)) <*> (^?! nth 4 . _String . to (read . Text.unpack))) . toListOf values $ klines
  if length res == fromInteger limit
    then pure (Just res)
    else pure Nothing

{-# NOINLINE sqlConn #-}
sqlConn :: Connection
sqlConn = unsafePerformIO . Sql.open $ "market.db"

main :: IO ()
main = do
  execute_ sqlConn "create table market (time integer, symbol text, open real, close real, unique (time, symbol))"
  withExclusiveTransaction sqlConn $
    getSymbolList >>= traverse (\s -> fmap ((s,) . fmap (s,)) <$> getKLines s) >>= traverse_ (maybe (pure ()) insertAll)
  where
    insertAll (s, rows) = do
      putStrLn . Text.unpack $ "Inserting " <> s
      executeMany sqlConn "insert into market values (?, ?, ?, ?)" . fmap (\(s, (t, o, c)) -> (t, s, o, c)) $ rows
