{-# LANGUAGE OverloadedStrings #-}

module Dump
  ( dump
  , bytesToXXD
  ) where

import Cli ( DumpArgs(..) )
import Data.ByteString.Lazy ( ByteString )
import qualified Data.ByteString.Lazy as BS
import Data.Char ( chr )
import Data.Word ( Word8 )
import Data.Int ( Int64 )
import Text.Printf ( printf )
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

dump :: DumpArgs -> IO ()
dump args = output =<< bytesToXXD numColumns groupSize offset . trunc <$> input
  where
   input      = maybe BS.getContents BS.readFile  . dumpInputFile  $ args
   output     = maybe Text.putStr Text.writeFile  . dumpOutputFile $ args
   trunc      = maybe id (BS.take . fromIntegral) . dumpLength     $ args
   numColumns = fromIntegral                      . dumpNumColumns $ args
   groupSize  = fromIntegral                      . dumpGroupSize  $ args
   offset     = fromIntegral                      . dumpOffset     $ args

bytesToXXD :: Int -> Int -> Int64 -> ByteString -> Text
bytesToXXD numColumns groupSize initialOffset = loop initialOffset . groupN (fromIntegral numColumns)
  where
    loop :: Int64 -> [ByteString] -> Text
    loop offset (line : rest) = displayLine offset line <> loop (offset + BS.length line) rest
    loop _ [] = ""

    hex :: ByteString -> Text
    hex = Text.intercalate " " . map (displayBy (Text.pack . printf "%02X")) . groupN (fromIntegral groupSize)

    ascii :: ByteString -> Text
    ascii = displayBy displayByte

    displayLine :: Int64 -> ByteString -> Text
    displayLine offset line
      = Text.pack $ printf "%08x: %-*s  %s\n" offset hexWidth (hex line) (ascii line)
      where
        hexWidth = numColumns * 2 + numGroups - 1
        numGroups = - (numColumns `div` (- groupSize)) -- truncate to +inf

    displayBy :: (Word8 -> Text) -> ByteString -> Text
    displayBy f bytes = Text.concat . map f . BS.unpack $ bytes

    groupN :: Int64 -> ByteString -> [ByteString]
    groupN n str
      = if BS.null curr
        then []
        else curr : groupN n rest
      where
        (curr, rest) = BS.splitAt n str

    displayByte :: Word8 -> Text
    displayByte b
      = if isAsciiGraphic b then Text.singleton (chr . fromIntegral $ b) else "."

    isAsciiGraphic :: Word8 -> Bool
    isAsciiGraphic i = 0x21 <= i && i <= 0x7e || i == 0x20
