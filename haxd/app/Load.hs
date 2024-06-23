{-# LANGUAGE OverloadedStrings #-}

module Load
  ( load
  ) where

import Cli ( LoadArgs(..) )
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy.IO as Text
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy ( ByteString )
import qualified System.IO as IO
import Data.Int ( Int64 )
import qualified Data.Attoparsec.Text.Lazy as Parsec
import Data.Bits (Bits)
import Text.Printf ( printf )
import Data.Foldable ( traverse_ )
import Control.Monad ( when )

load :: LoadArgs -> IO ()
load args = output =<< input
  where
    input = map addOffset . parseXXD <$> inputText
    inputText = maybe Text.getContents Text.readFile . loadInputFile $ args
    addOffset (Line offset content) = Line (offset + additionalOffset) content
    additionalOffset = fromIntegral . loadOffset $ args
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
      currPos <- IO.hTell file
      when (currPos < fromIntegral offset)
        . BS.hPut file
        . BS.replicate (offset - fromIntegral currPos)
        $ 0
      BS.hPut file content

xxdToBytes :: [Line] -> ByteString
xxdToBytes = BS.concat . go 0
  where
    go offset arg@(Line nextOffset content : rest)
      = if offset > nextOffset
        then error (printf "%08x: overlapping not allowed" nextOffset)
        else if offset < nextOffset
          then BS.replicate (nextOffset - offset) 0 : go nextOffset arg
          else content : go (offset + BS.length content) rest
    go _ [] = []

parseXXD :: Text -> [Line]
parseXXD = map parseLine . Text.lines

data Line = Line
  { lineOffset :: Int64
  , lineContent :: ByteString
  }

parseLine :: Text -> Line
parseLine line = Line offset content
  where
    (offsetPart, rest) = Text.breakOn ": " line
    offset = parseHex offsetPart
    hexPart = fst . Text.breakOn "  " . Text.drop 2 $ rest
    content = BS.pack . map parseHex . Text.chunksOf 2 . Text.filter (/= ' ') $ hexPart

    fromRight (Left s) = error s
    fromRight (Right a) = a
    parseHex :: (Integral a, Bits a) => Text -> a
    parseHex = fromRight . Parsec.parseOnly Parsec.hexadecimal
