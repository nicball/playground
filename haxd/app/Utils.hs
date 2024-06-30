{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( integralFromByteString
  , chunksOf
  , chunksOfBS
  , chunksOfBL
  ) where

import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BL
import Data.Bits ( Bits( shift ) )
import Data.Int ( Int64 )

{-# INLINE chunksOf #-}
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chunk : chunksOf n rest
  where
    (chunk, rest) = splitAt n xs

{-# INLINE chunksOfBL #-}
chunksOfBL :: Int64 -> BL.ByteString -> [BSS.ByteString]
chunksOfBL n str
  = if BL.null curr
    then []
    else BL.toStrict curr : chunksOfBL n rest
  where
    (curr, rest) = BL.splitAt n str

{-# INLINE chunksOfBS #-}
chunksOfBS :: Int -> BSS.ByteString -> [BSS.ByteString]
chunksOfBS n str
  = if BSS.null curr
    then []
    else curr : chunksOfBS n rest
  where
    (curr, rest) = BSS.splitAt n str

{-# INLINE integralFromByteString #-}
integralFromByteString :: (Integral a, Bits a) => BSS.ByteString -> a
integralFromByteString = go 0 . BSSC.unpack
  where
    go n [] = n
    go n (d : ds) = go (shift n 4 + fromDigit d) ds
    fromDigit d = case d of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      '9' -> 9
      'a' -> 10
      'b' -> 11
      'c' -> 12
      'd' -> 13
      'e' -> 14
      'f' -> 15
      'A' -> 10
      'B' -> 11
      'C' -> 12
      'D' -> 13
      'E' -> 14
      'F' -> 15
      _ -> error "invalid hex digit"
