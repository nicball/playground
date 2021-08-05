#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (p: with p; [ sqlite-simple http-conduit lens-aeson ])"

{-# LANGUAGE OverloadedStrings,TupleSections #-}

import Database.SQLite.Simple as SQL
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Data.List
import qualified Data.Text as Text
import Data.Text (Text)

type Market = Map Text (Double, Double)
type Strategy = Market -> Market -> Assets -> Assets
type Assets = [(Text, Double)]

sellAll :: Market -> Assets -> Double
sellAll market = sum . fmap (\(sym, quant) -> if sym == "USDT" then quant else quant * fst (market ! sym))

hothead :: Strategy 
hothead past = buyNths False [Map.size past - 1] past

coldhead :: Strategy 
coldhead = buyNths True [0]

buyNths :: Bool -> [Int] ->  Strategy 
buyNths force ns past now assets =
    let usdt = sellAll now assets
        rates = Map.map (\(o, c) -> (c - o) / o) past
        newAssets = (if force then id else filter ((> 0) . snd)) . flip fmap ns . (!!) . sortOn snd . Map.toList $ rates
    in if not . null $ newAssets
    then fmap (\(sym, _) -> (sym, usdt / fromIntegral (length newAssets) / fst (now ! sym))) newAssets
    else [("USDT", usdt)]

smarterHothead :: Strategy
smarterHothead past = buyNths False [n - 4 .. n - 1] past
    where n = Map.size past

smarterColdhead :: Strategy
smarterColdhead = buyNths True [0 .. 3]

invest :: Strategy -> [Market] -> [([Text], Double)]
invest strat hist = fmap (\(market, assets) -> (fmap fst assets, sellAll market assets)) . fst . ($ ([], [("USDT", 10000)])) . foldr (flip (.)) id . fmap (\input (trace, state) -> (trace ++ [(snd input, state)], uncurry strat input state)) . zip hist . tail $ hist

main :: IO ()
main = do
    conn <- SQL.open "market.db" 
    symtimeopenclose <- query_ conn "select symbol, time, open, close from market" :: IO [(Text, Integer, Double, Double)]
    let hist = fmap (Map.fromList . fmap (\(a, _, b, c) -> (a, (b, c)))) . groupBy (\(_, a, _, _) (_, b, _, _) -> a == b) . sortOn (\(_, a, _, _) -> a) $ symtimeopenclose 
    -- mapM_ print {-. sortOn (snd . snd)-} . fmap (\n -> (n, ) . last . invest (buyNths True [n]) $ hist) $ [0 .. Map.size (head hist) - 1]
    mapM_ print . fmap (\n -> (n, ) . numRises . invest (buyNths True [n]) $ hist) $ [0 .. Map.size (head hist) - 1]
    -- mapM_ print . invest (buyNths True [1]) $ hist
    -- print . last . invest smarterColdhead $ hist
    where numRises trace = length . filter (> 0) . fmap (uncurry (-)) . zip (tail ns) $ ns
              where ns = fmap snd trace
