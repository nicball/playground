{-# LANGUAGE OverloadedStrings #-}

module Dump
  ( dump
  , bytesToXXD
  ) where

import Cli ( DumpArgs(..) )
import Data.ByteString.Lazy ( ByteString )
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import Data.Char ( chr )
import Data.Word ( Word8 )
import Data.Int ( Int64 )
import Data.Bits ( Bits(shift, (.&.)) )

dump :: DumpArgs -> IO ()
dump args = output . bytesToXXD numColumns groupSize offset . trunc =<< input
  where
   input      = maybe BS.getContents BS.readFile . dumpInputFile  $ args
   output     = maybe BS.putStr BS.writeFile     . dumpOutputFile $ args
   trunc      = maybe id BS.take                 . dumpLength     $ args
   numColumns = dumpNumColumns args
   groupSize  = min (dumpGroupSize args) numColumns
   offset     = dumpOffset     args

bytesToXXD :: Int64 -> Int64 -> Int64 -> ByteString -> ByteString
bytesToXXD numColumns groupSize initialOffset = loop initialOffset . groupN numColumns
  where
    loop :: Int64 -> [BSS.ByteString] -> ByteString
    loop offset (line : rest) = BS.fromStrict (displayLine offset line) <> loop (offset + fromIntegral (BSS.length line)) rest
    loop _ [] = ""

    hex :: BSS.ByteString -> BSS.ByteString
    hex = BSS.intercalate " " . map (BSS.concatMap (padLeft '0' 2 . integralToByteString)) . groupNS (fromIntegral groupSize)

    integralToByteString :: (Integral a, Bits a) => a -> BSS.ByteString
    integralToByteString 0 = ""
    integralToByteString n = integralToByteString (shift n (-4)) <> digitToByteString (n .&. 0xF)
      where
        digitToByteString 0 = "0"
        digitToByteString 1 = "1"
        digitToByteString 2 = "2"
        digitToByteString 3 = "3"
        digitToByteString 4 = "4"
        digitToByteString 5 = "5"
        digitToByteString 6 = "6"
        digitToByteString 7 = "7"
        digitToByteString 8 = "8"
        digitToByteString 9 = "9"
        digitToByteString 10 = "A"
        digitToByteString 11 = "B"
        digitToByteString 12 = "C"
        digitToByteString 13 = "D"
        digitToByteString 14 = "E"
        digitToByteString 15 = "F"
        digitToByteString _ = error "what the fuck?"

    padLeft :: Char -> Int -> BSS.ByteString -> BSS.ByteString
    padLeft p width text
      = if len < width
        then BSSC.replicate (width - len) p <> text
        else text
      where
        len = BSS.length text

    padRight :: Char -> Int -> BSS.ByteString -> BSS.ByteString
    padRight p width text
      = if len < width
        then text <> BSSC.replicate (width - len) p
        else text
      where
        len = BSS.length text

    ascii :: BSS.ByteString -> BSS.ByteString
    ascii = BSS.concatMap displayByte

    displayLine :: Int64 -> BSS.ByteString -> BSS.ByteString
    displayLine offset line
      = padLeft '0' 8 (integralToByteString offset) <> ": " <> padRight ' ' hexWidth (hex line) <> "  " <> ascii line <> "\n"
      where
        hexWidth = fromIntegral (numColumns * 2 + numGroups - 1)
        numGroups = - (numColumns `div` (- groupSize)) -- truncate to +inf

    groupN :: Int64 -> ByteString -> [BSS.ByteString]
    groupN n str
      = if BS.null curr
        then []
        else BS.toStrict curr : groupN n rest
      where
        (curr, rest) = BS.splitAt n str

    groupNS :: Int -> BSS.ByteString -> [BSS.ByteString]
    groupNS n str
      = if BSS.null curr
        then []
        else curr : groupNS n rest
      where
        (curr, rest) = BSS.splitAt n str

    displayByte :: Word8 -> BSS.ByteString
    displayByte b
      = if isXXDAscii b then BSSC.singleton (chr . fromIntegral $ b) else "."

    isXXDAscii :: Word8 -> Bool
    isXXDAscii i = 0x21 <= i && i <= 0x7e || i == 0x20
