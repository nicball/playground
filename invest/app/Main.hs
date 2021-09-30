module Main where

import GetData
import Invest
import System.Directory (doesFileExist)
import Database.SQLite.Simple as Sql (open)

main :: IO ()
main = do
  e <- doesFileExist "market.db"
  sql <- Sql.open "market.db"
  if not e then getData sql else pure ()
  investPrint sql
