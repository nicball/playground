module Main where

import Cli ( parseCommandLine, CliArgs(Edit, Dump, Load) )
import Dump ( dump )
import Load ( load )
import Edit ( edit )

main :: IO ()
main = do
  cliArgs <- parseCommandLine
  case cliArgs of
    Dump args -> dump args
    Load args -> load args
    Edit args -> edit args
