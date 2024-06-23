module Load
  ( load
  ) where

import Cli ( LoadArgs(..) )
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy ( Text )
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy ( ByteString )
import qualified System.IO as IO

load :: LoadArgs -> IO ()
-- load args = case loadInputFile args of
--   Nothing -> XXDToBytes =<< BS.getContents
--   Just path -> IO.withBinaryFile path IO.ReadMode
load = undefined

patchXXD :: Text -> IO.Handle -> IO ()
patchXXD = undefined

xxdToBytes :: Text -> ByteString
xxdToBytes = undefined
