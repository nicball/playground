module Invest (investPrint) where

import Control.Arrow ((***))
import Data.List (sortOn, groupBy)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple as SQL (Connection, query_)

type Market = Map Text (Double, Double)

type MarketHistory = [Market]

type Strategy = MarketHistory -> Int -> Assets -> Assets

type Assets = [(Text, Double)]

sellAll :: Market -> Assets -> Double
sellAll market = sum . fmap (\(sym, quant) -> if sym == "USDT" then quant else quant * fst (market ! sym))

hothead :: Strategy
hothead hist = buyNths False [Map.size (head hist) - 1] hist

coldhead :: Strategy
coldhead = buyNths True [0]

buyNths :: Bool -> [Int] -> Strategy
buyNths force ns hist time assets =
  let now = hist !! time
      usdt = sellAll now assets
      rates = Map.map (\(o, c) -> (c - o) / o) $ hist !! (time - 1)
      newAssets = (if force then id else filter ((> 0) . snd)) . flip fmap ns . (!!) . sortOn snd . Map.toList $ rates
   in if not . null $ newAssets
        then fmap (\(sym, _) -> (sym, usdt / fromIntegral (length newAssets) / fst (now ! sym))) newAssets
        else [("USDT", usdt)]

smarterHothead :: Strategy
smarterHothead hist time assets =
  let now = hist !! time
      usdt = sellAll now assets
      rates = Map.filterWithKey (const . inARow) . Map.map (\(o, c) -> (c - o) / o) $ hist !! (time - 1)
      newAssets = filter ((> 0) . snd) . flip fmap [n - 4 .. n - 1] . (!!) . sortOn snd . Map.toList $ rates
      n = Map.size (head hist)
      inARow sym = all (uncurry (<)) . fmap ((! sym) . (hist !!)) $ [time - 4 .. time - 1]
   in if not . null $ newAssets
        then fmap (\(sym, _) -> (sym, usdt / fromIntegral (length newAssets) / fst (now ! sym))) newAssets
        else [("USDT", usdt)]

smarterColdhead :: Strategy
smarterColdhead = buyNths True [0 .. 3]

invest :: Strategy -> MarketHistory -> [([Text], Double)]
invest strat hist =
  fmap (\(n, assets) -> (fmap fst assets, sellAll (hist !! n) assets))
    . reverse
    . scanr (((+ 1) ***) . ($)) (3, [("USDT", 10000)])
    . fmap (uncurry strat)
    . drop 4
    . flip zip [0 ..]
    . replicate (length (head hist))
    $ hist

investPrint :: Connection -> IO ()
investPrint conn = do
  symtimeopenclose <- query_ conn "select symbol, time, open, close from market" :: IO [(Text, Integer, Double, Double)]
  let hist =
        fmap
          ( Map.fromList
              . fmap (\(a, _, b, c) -> (a, (b, c)))
          )
          . groupBy (\(_, a, _, _) (_, b, _, _) -> a == b)
          . sortOn (\(_, a, _, _) -> a)
          $ symtimeopenclose
  -- mapM_ print {-. sortOn (snd . snd)-} . fmap (\n -> (n, ) . last . invest (buyNths True [n]) $ hist) $ [0 .. Map.size (head hist) - 1]
  -- mapM_ print . invest (buyNths True [1]) $ hist
  -- mapM_ print . fmap (\n -> (n,) . numRises . invest (buyNths True [n]) $ hist) $ [0 .. Map.size (head hist) - 1]
  mapM_ print . invest coldhead $ hist
  where
    numRises trace = length . filter (> 0) . fmap (uncurry (-)) . zip (tail ns) $ ns
      where
        ns = fmap snd trace
