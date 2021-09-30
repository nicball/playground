module Main where

import qualified GetData
import qualified Invest
import System.Directory (doesFileExist)

main :: IO ()
main = do
  e <- doesFileExist "market.db"
  if not e then GetData.main else pure ()
  Invest.main
