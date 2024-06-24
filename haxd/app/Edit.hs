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
import qualified System.Posix.Files as Posix
import Control.Exception ( finally, onException )
import qualified Data.Text.Lazy.IO as Text
import qualified Data.ByteString.Lazy as BS

edit :: EditArgs -> IO ()
edit args = do
  let inputPath = editInputFile args
  withBinaryTempFile inputPath \dumpPath dumpFile -> do
    Text.hPutStr dumpFile
      . bytesToXXD (fromIntegral . editNumColumns $ args) (fromIntegral . editGroupSize $ args) 0
      =<< BS.readFile inputPath
    IO.hFlush dumpFile
    editor <- getEnv "EDITOR"
    callProcess editor [ dumpPath ]
    withBinaryTempFileToPersist inputPath \_ loadFile -> do
      IO.hSeek dumpFile IO.AbsoluteSeek 0
      xxd <- parseXXD <$> Text.hGetContents dumpFile
      if editPatchMode args
        then patchXXD loadFile xxd
        else BS.hPut loadFile . xxdToBytes $ xxd

withBinaryTempFile :: FilePath -> (FilePath -> IO.Handle -> IO a) -> IO a
withBinaryTempFile path action = do
  let (dir, template) = (Path.takeDirectory &&& Path.takeFileName) path
  (tmpPath, hdl) <- IO.openBinaryTempFile dir template
  action tmpPath hdl `finally` Posix.removeLink tmpPath `finally` IO.hClose hdl

withBinaryTempFileToPersist :: FilePath -> (FilePath -> IO.Handle -> IO a) -> IO a
withBinaryTempFileToPersist path action = do
  let (dir, template) = (Path.takeDirectory &&& Path.takeFileName) path
  (tmpPath, hdl) <- IO.openBinaryTempFile dir template
  (action tmpPath hdl <* persist tmpPath) `onException` Posix.removeLink tmpPath `finally` IO.hClose hdl
  where
    persist tmpPath = do
      mode <- Posix.fileMode <$> Posix.getFileStatus path
      Posix.setFileMode tmpPath mode
      Posix.rename tmpPath path
