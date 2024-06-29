{-# LANGUAGE OverloadedStrings #-}

module Load
  ( load
  , patchXXD
  , parseXXD
  , xxdToBytes
  ) where

import Cli ( LoadArgs(..) )
import Control.Monad ( when )
import Data.ByteString.Lazy ( ByteString )
import Data.Char ( isSpace )
import Data.Foldable ( traverse_ )
import Data.Int ( Int64 )
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.UTF8 as UTF8S
import qualified System.IO as IO
import Utils ( groupNS, integralFromByteString, integralToByteString, padLeft )

load :: LoadArgs -> IO ()
load args = output . map addOffset . parseXXD =<< input
  where
    input = maybe BS.getContents BS.readFile . loadInputFile $ args
    addOffset (Line offset content) = Line (offset + additionalOffset) content
    additionalOffset = loadOffset args
    transform file
      = if loadPatchMode args
        then patchXXD file
        else BS.hPut file . xxdToBytes
    output = case loadOutputFile args of
      Just path -> IO.withBinaryFile path IO.WriteMode . flip transform
      Nothing -> transform IO.stdout

patchXXD :: IO.Handle -> [Line] -> IO ()
patchXXD file = traverse_ execLine
  where
    execLine (Line offset content) = do
      IO.hSeek file IO.AbsoluteSeek (fromIntegral offset)
      currPos <- fromIntegral <$> IO.hTell file
      when (currPos < offset)
        . BS.hPut file
        . BS.replicate (offset - currPos)
        $ 0
      BS.hPut file content

xxdToBytes :: [Line] -> ByteString
xxdToBytes = BS.concat . go 0
  where
    go offset arg@(Line nextOffset content : rest)
      = if offset > nextOffset
        then error (UTF8S.toString (padLeft '0' 8 (integralToByteString nextOffset)) <> ": overlapping not allowed")
        else if offset < nextOffset
          then BS.replicate (nextOffset - offset) 0 : go nextOffset arg
          else content : go (offset + BS.length content) rest
    go _ [] = []

parseXXD :: ByteString -> [Line]
parseXXD = map parseLine . filter (not . BSC.all isSpace) . BSC.lines

data Line = Line
  { lineOffset :: Int64
  , lineContent :: ByteString
  }

{-# INLINE parseLine #-}
parseLine :: ByteString -> Line
parseLine line = Line offset content
  where
    (offsetPart, rest) = BSSC.breakSubstring ": " . BS.toStrict $ line
    offset = integralFromByteString  offsetPart
    hexPart = fst . BSSC.breakSubstring "  " . BSS.drop 2 $ rest
    content = BS.pack . map integralFromByteString . groupNS 2 . BSSC.filter (/= ' ') $ hexPart
