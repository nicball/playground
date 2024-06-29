{-# LANGUAGE OverloadedStrings #-}

module Dump
  ( dump
  , bytesToXXD
  ) where

import Cli ( DumpArgs(..) )
import Data.ByteString.Lazy ( ByteString )
import Data.Char ( chr )
import Data.Int ( Int64 )
import Data.Word ( Word8 )
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BS
import Utils ( padLeft, padRight, integralToByteString, groupN, groupNS )

dump :: DumpArgs -> IO ()
dump args = output . bytesToXXD numColumns groupSize offset . trunc =<< input
  where
   input      = maybe BS.getContents BS.readFile . dumpInputFile  $ args
   output     = maybe BS.putStr BS.writeFile     . dumpOutputFile $ args
   trunc      = maybe id BS.take                 . dumpLength     $ args
   numColumns = dumpNumColumns args
   groupSize  = min (dumpGroupSize args) numColumns
   offset     = dumpOffset args

bytesToXXD :: Int64 -> Int64 -> Int64 -> ByteString -> ByteString
bytesToXXD numColumns groupSize initialOffset = loop initialOffset . groupN numColumns
  where
    {-# INLINE loop #-}
    loop :: Int64 -> [BSS.ByteString] -> ByteString
    loop offset (line : rest) = BS.fromStrict (displayLine offset line) <> loop (offset + fromIntegral (BSS.length line)) rest
    loop _ [] = ""

    {-# INLINE hex #-}
    hex :: BSS.ByteString -> BSS.ByteString
    hex = BSS.intercalate " " . map (BSS.concatMap (padLeft '0' 2 . integralToByteString)) . groupNS (fromIntegral groupSize)

    {-# INLINE ascii #-}
    ascii :: BSS.ByteString -> BSS.ByteString
    ascii = BSS.concatMap displayByte

    {-# INLINE displayLine #-}
    displayLine :: Int64 -> BSS.ByteString -> BSS.ByteString
    displayLine offset line
      = padLeft '0' 8 (integralToByteString offset) <> ": " <> padRight ' ' hexWidth (hex line) <> "  " <> ascii line <> "\n"
      where
        hexWidth = fromIntegral (numColumns * 2 + numGroups - 1)
        numGroups = - (numColumns `div` (- groupSize)) -- truncate to +inf

    {-# INLINE displayByte #-}
    displayByte :: Word8 -> BSS.ByteString
    displayByte b
      = if isXXDAscii b then BSSC.singleton (chr . fromIntegral $ b) else "."

    {-# INLINE isXXDAscii #-}
    isXXDAscii :: Word8 -> Bool
    isXXDAscii i = 0x21 <= i && i <= 0x7e || i == 0x20
