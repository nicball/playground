module Load
  ( load
  ) where

import Cli (LoadArgs(..))

load :: LoadArgs -> IO ()
load _ = pure ()

