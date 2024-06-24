{-# LANGUAGE BlockArguments #-}

module Edit
  ( edit
  ) where

import Cli ( EditArgs(..) )
import Dump ( bytesToXXD )
import Load ( xxdToBytes, patchXXD, parseXXD )
import qualified System.FilePath as Path
import Control.Arrow ( (&&&) )
import qualified System.IO as IO
import System.Environment ( getEnv )
import System.Process ( callProcess )
import System.Posix.Files ( rename, removeLink )
import Control.Exception ( finally, onException )
import qualified Data.Text.Lazy.IO as Text
import qualified Data.ByteString.Lazy as BS

edit :: EditArgs -> IO ()
edit args = do
  let
    inputPath = editInputFile args
    (fileName, directory) = (Path.takeFileName &&& Path.takeDirectory) inputPath

  withBinaryTempFile directory fileName \dumpPath dumpFile -> do
    Text.hPutStr dumpFile
      . bytesToXXD (fromIntegral . editNumColumns $ args) (fromIntegral . editGroupSize $ args) 0
      =<< BS.readFile inputPath
    IO.hFlush dumpFile
    editor <- getEnv "EDITOR"
    callProcess editor [ dumpPath ]
    withBinaryTempFileToPersist directory fileName inputPath \_ loadFile -> do
      IO.hSeek dumpFile IO.AbsoluteSeek 0
      xxd <- parseXXD <$> Text.hGetContents dumpFile
      if editPatchMode args
        then patchXXD loadFile xxd
        else BS.hPut loadFile . xxdToBytes $ xxd

withBinaryTempFile :: FilePath -> String -> (FilePath -> IO.Handle -> IO a) -> IO a
withBinaryTempFile dir template action = do
  (path, hdl) <- IO.openBinaryTempFile dir template
  action path hdl `finally` removeLink path `finally` IO.hClose hdl

withBinaryTempFileToPersist :: FilePath -> String -> FilePath -> (FilePath -> IO.Handle -> IO a) -> IO a
withBinaryTempFileToPersist dir template persistPath action = do
  (path, hdl) <- IO.openBinaryTempFile dir template
  (action path hdl <* rename path persistPath) `onException` removeLink path `finally` IO.hClose hdl
