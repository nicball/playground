{-# LANGUAGE OverloadedStrings #-}

module Dump
  ( dump
  , bytesToXXD
  ) where

import Cli ( DumpArgs(..) )
import Data.ByteString.Lazy ( ByteString )
import qualified Data.ByteString.Lazy as BS
-- import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Char ( chr )
import Data.Word ( Word8 )
import Data.Int ( Int64 )
-- import Text.Printf ( printf )
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Bits ( shift, (.&.), Bits )

dump :: DumpArgs -> IO ()
dump args = output =<< bytesToXXD numColumns groupSize offset . trunc <$> input
  where
   input      = maybe BS.getContents BS.readFile . dumpInputFile  $ args
   output     = maybe Text.putStr Text.writeFile . dumpOutputFile $ args
   trunc      = maybe id BS.take                 . dumpLength     $ args
   numColumns = dumpNumColumns args
   groupSize  = min (dumpGroupSize args) numColumns
   offset     = dumpOffset     args

bytesToXXD :: Int64 -> Int64 -> Int64 -> ByteString -> Text
bytesToXXD numColumns groupSize initialOffset = loop initialOffset . groupN numColumns
  where
    loop :: Int64 -> [ByteString] -> Text
    loop offset (line : rest) = displayLine offset line <> loop (offset + BS.length line) rest
    loop _ [] = ""

    hex :: ByteString -> Text
    -- hex = Text.intercalate " " . map (displayBy (Text.pack . printf "%02X")) . groupN groupSize
    hex = Text.intercalate " " . map (displayBy (padLeft '0' 2 . integralToStr)) . groupN groupSize

    integralToStr :: (Integral a, Bits a) => a -> Text
    integralToStr 0 = ""
    integralToStr n = integralToStr (shift n (-4)) <> digitToText (n .&. 0xF)
      where
        digitToText 0 = "0"
        digitToText 1 = "1"
        digitToText 2 = "2"
        digitToText 3 = "3"
        digitToText 4 = "4"
        digitToText 5 = "5"
        digitToText 6 = "6"
        digitToText 7 = "7"
        digitToText 8 = "8"
        digitToText 9 = "9"
        digitToText 10 = "A"
        digitToText 11 = "B"
        digitToText 12 = "C"
        digitToText 13 = "D"
        digitToText 14 = "E"
        digitToText 15 = "F"
        digitToText _ = error "what the fuck?"

    padLeft :: Char -> Int64 -> Text -> Text
    padLeft p width text
      = if len < width
        then Text.replicate (width - len) (Text.singleton p) <> text
        else text
      where
        len = Text.length text

    padRight :: Char -> Int64 -> Text -> Text
    padRight p width text
      = if len < width
        then text <> Text.replicate (width - len) (Text.singleton p)
        else text
      where
        len = Text.length text

    ascii :: ByteString -> Text
    ascii = displayBy displayByte

    displayLine :: Int64 -> ByteString -> Text
    displayLine offset line
      -- = Text.pack $ printf "%08x: %-*s  %s\n" offset hexWidth (hex line) (ascii line)
      = padLeft '0' 8 (integralToStr offset) <> ": " <> padRight ' ' hexWidth (hex line) <> "  " <> ascii line <> "\n"
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
      = if isXXDAscii b then Text.singleton (chr . fromIntegral $ b) else "."

    isXXDAscii :: Word8 -> Bool
    isXXDAscii i = 0x21 <= i && i <= 0x7e || i == 0x20
