{-# LANGUAGE BlockArguments #-}

module Edit
  ( edit
  ) where

import Cli ( EditArgs(..), DumpArgs(..), LoadArgs(..) )
import Dump ( dump )
import Load ( load )
import qualified System.FilePath as Path
import Control.Arrow ( (&&&) )
import qualified System.IO as IO
import System.Environment ( getEnv )
import System.Process ( callProcess )
import System.Posix.Files ( rename, removeLink )
import Control.Exception ( finally, onException )

edit :: EditArgs -> IO ()
edit args = do
  let
    inputPath = editInputFile args
    (fileName, directory) = (Path.takeFileName &&& Path.takeDirectory) inputPath
  withBinaryTempFile directory fileName \dumpPath -> do
    dump DumpArgs
      { dumpOutputFile = Just dumpPath
      , dumpOffset     = 0
      , dumpNumColumns = editNumColumns args
      , dumpLength     = Nothing
      , dumpInputFile  = Just . editInputFile $ args
      , dumpGroupSize  = editGroupSize args
      }
    editor <- getEnv "EDITOR"
    callProcess editor [ dumpPath ]
    withBinaryTempFileToPersist directory fileName inputPath \loadPath ->
      load LoadArgs
        { loadOffset = 0
        , loadPatchMode = False
        , loadInputFile = Just dumpPath
        , loadOutputFile = Just loadPath
        }

withBinaryTempFile :: FilePath -> String -> (FilePath -> IO a) -> IO a
withBinaryTempFile dir template action = do
  (path, hdl) <- IO.openBinaryTempFile dir template
  IO.hClose hdl
  action path `finally` removeLink path

withBinaryTempFileToPersist :: FilePath -> String -> FilePath -> (FilePath -> IO a) -> IO a
withBinaryTempFileToPersist dir template persistPath action = do
  (path, hdl) <- IO.openBinaryTempFile dir template
  IO.hClose hdl
  (action path <* rename path persistPath) `onException` removeLink path
