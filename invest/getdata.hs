{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Database.SQLite.Simple as Sql
import Data.Aeson.Lens as Lens
import Control.Lens as Lens
import Data.Map.Strict as Map
import Network.HTTP.Simple as Http
import Data.Aeson as Aeson
import Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)
import Data.Foldable (traverse_)

getSymbolList :: IO [Text]
getSymbolList = do
    minfo <- getResponseBody <$> httpJSON "https://api.binance.com/api/v3/exchangeInfo" :: IO Value
    pure $ minfo ^.. key "symbols" . values . key "symbol" . _String . filtered ("USDT" `isSuffixOf`)

getKLines :: Text -> IO [(Integer, Double, Double)]
getKLines symbol = do
    let limit = 1000
    let rstr = "https://api.binance.com/api/v3/klines?symbol="
            <> Text.unpack symbol
            <> "&interval=1h&limit="
            <> show limit
    putStrLn $ "Getting " <> rstr
    klines <- getResponseBody <$> httpJSON (parseRequest_ rstr) :: IO Value
    pure . Prelude.zipWith (\t (o, c) -> (t, o, c)) [0 .. limit - 1] . fmap ((,) <$> (^?! nth 1 . _String . to (read . Text.unpack)) <*> (^?! nth 4 . _String . to (read . Text.unpack))) . toListOf values $ klines

sqlConn :: Connection
sqlConn = unsafePerformIO . Sql.open $ "market.db"
{-# NOINLINE sqlConn #-}

main :: IO ()
main = do
    execute_ sqlConn "create table market (time integer, symbol text, open real, close real, unique (time, symbol))"
    withExclusiveTransaction sqlConn $
        getSymbolList >>= traverse (\s -> (s,) . fmap (s,) <$> getKLines s) >>= traverse_ insertAll
    where insertAll (s, rows) = do
             putStrLn . Text.unpack $ "Inserting " <> s
             executeMany sqlConn "insert into market values (?, ?, ?, ?)" . fmap (\(s, (t, o, c)) -> (t, s, o, c)) $ rows
