{-# LANGUAGE OverloadedStrings #-}

module Dump
  ( dump
  , bytesToXXD
  ) where

import Cli ( DumpArgs(..) )
import Data.Int ( Int64 )
import Data.Word ( Word8 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.List ( intercalate )
import Utils ( chunksOf, chunksOfBL )
import System.IO ( stdout )

dump :: DumpArgs -> IO ()
dump args = output . bytesToXXD numColumns groupSize offset . trunc =<< input
  where
   input      = maybe BL.getContents BL.readFile . dumpInputFile $ args
   output     = maybe (BB.hPutBuilder stdout) BB.writeFile . dumpOutputFile $ args
   trunc      = maybe id BL.take . dumpLength $ args
   numColumns = dumpNumColumns args
   groupSize  = min (dumpGroupSize args) numColumns
   offset     = dumpOffset args

bytesToXXD :: Int64 -> Int64 -> Int64 -> BL.ByteString -> BB.Builder
bytesToXXD numColumns groupSize initialOffset = loop initialOffset . chunksOfBL numColumns
  where
    loop :: Int64 -> [BS.ByteString] -> BB.Builder
    loop offset (line : rest) = displayLine offset line <> loop (offset + fromIntegral (BS.length line)) rest
    loop _ [] = ""

    {-# INLINE displayLine #-}
    displayLine :: Int64 -> BS.ByteString -> BB.Builder
    displayLine offset line
      = BB.int32HexFixed (fromIntegral offset) <> BB.stringUtf8 ": " <> hex line <> padding <> "  " <> ascii line <> "\n"
      where
        hexWidth = numColumns * 2 + numGroups - 1
        numGroups = - (numColumns `div` (- groupSize)) -- truncate to +inf
        lineLen = fromIntegral . BS.length $ line
        lineGroups = - (lineLen `div` (- groupSize))
        lineWidth = lineLen * 2 + lineGroups - 1
        padding = mconcat . replicate (fromIntegral (hexWidth - lineWidth)) . BB.char7 $ ' '

    {-# INLINE hex #-}
    hex :: BS.ByteString -> BB.Builder
    hex = mconcat . intercalate [BB.char7 ' '] . chunksOf (fromIntegral groupSize) . map BB.word8HexFixed . BS.unpack

    {-# INLINE ascii #-}
    ascii :: BS.ByteString -> BB.Builder
    ascii = foldMap displayByte . BS.unpack

    {-# INLINE displayByte #-}
    displayByte :: Word8 -> BB.Builder
    displayByte b
      = if isXXDAscii b then BB.word8 b else BB.char7 '.'

    {-# INLINE isXXDAscii #-}
    isXXDAscii :: Word8 -> Bool
    isXXDAscii i = 0x21 <= i && i <= 0x7e || i == 0x20
