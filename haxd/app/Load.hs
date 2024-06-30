{-# LANGUAGE OverloadedStrings #-}

module Load
  ( load
  , patchXXD
  , parseXXD
  , xxdToBytes
  ) where

import Cli ( LoadArgs(..) )
import Control.Monad ( when )
import Data.Char ( isSpace )
import Data.Foldable ( traverse_ )
import Data.Int ( Int64 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8 as UTF8L
import qualified Data.ByteString.Builder as BB
import qualified System.IO as IO
import Utils ( chunksOfBS, integralFromByteString )

load :: LoadArgs -> IO ()
load args = output . map addOffset . parseXXD =<< input
  where
    input = maybe BL.getContents BL.readFile . loadInputFile $ args
    addOffset (Line offset content) = Line (offset + additionalOffset) content
    additionalOffset = loadOffset args
    transform file
      = if loadPatchMode args
        then patchXXD file
        else BL.hPut file . xxdToBytes
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
        . BL.hPut file
        . BL.replicate (offset - currPos)
        $ 0
      BL.hPut file content

xxdToBytes :: [Line] -> BL.ByteString
xxdToBytes = BB.toLazyByteString . mconcat . go 0
  where
    go offset arg@(Line nextOffset content : rest)
      = if offset > nextOffset
        then error (UTF8L.toString (BB.toLazyByteString (BB.int64HexFixed nextOffset)) <> ": overlapping not allowed")
        else if offset < nextOffset
          then mconcat (replicate (fromIntegral (nextOffset - offset)) (BB.int8 0)) : go nextOffset arg
          else BB.lazyByteString content : go (offset + BL.length content) rest
    go _ [] = mempty

parseXXD :: BL.ByteString -> [Line]
parseXXD = map parseLine . filter (not . BLC.all isSpace) . BLC.lines

data Line = Line
  { lineOffset :: Int64
  , lineContent :: BL.ByteString
  }

{-# INLINE parseLine #-}
parseLine :: BL.ByteString -> Line
parseLine line = Line offset content
  where
    (offsetPart, rest) = BSC.breakSubstring ": " . BL.toStrict $ line
    offset = integralFromByteString offsetPart
    hexPart = fst . BSC.breakSubstring "  " . BS.drop 2 $ rest
    content = BL.pack . map integralFromByteString . chunksOfBS 2 . BSC.filter (/= ' ') $ hexPart
