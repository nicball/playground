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
import Conduit
import Data.Char ( ord )
import Data.Foldable ( traverse_ )

dump :: DumpArgs -> IO ()
dump args
  = runConduitRes $ input .| chunksOfCE (fromIntegral . dumpNumColumns $ args) .| line .| output
  where
    input :: MonadResource m => ConduitT () BS.ByteString m ()
    input = maybe stdinC sourceFile . dumpInputFile $ args

    output :: MonadResource m => ConduitT BS.ByteString Void m ()
    output = maybe stdoutC sinkFile . dumpOutputFile $ args

    line :: PrimMonad m => ConduitT BS.ByteString BS.ByteString m ()
    line = (getZipConduit . traverse_ ZipConduit $ [offset, mapC (const ": "), hex, mapC (const "  "), ascii, mapC (const "\n")])

    offset :: PrimMonad m => ConduitT BS.ByteString BS.ByteString m ()
    offset = scanlC ((. fromIntegral . BS.length) . (+)) (dumpOffset args) .| mapC (BB.int32HexFixed . fromIntegral) .| builderToByteString

    hexGroup :: PrimMonad m => ConduitT BS.ByteString BS.ByteString m ()
    hexGroup = concatMapCE BB.word8HexFixed .| builderToByteString

    hex :: PrimMonad m => ConduitT BS.ByteString BS.ByteString m ()
    hex = chunksOfCE (fromIntegral . dumpGroupSize $ args) .| hexGroup .| intersperseC " "

    ascii :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
    ascii = omapCE (\c -> if isXXDAscii c then c else fromIntegral . ord $ '.')

    isXXDAscii :: Word8 -> Bool
    isXXDAscii i = 0x21 <= i && i <= 0x7e || i == 0x20

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
