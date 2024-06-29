{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( integralToByteString
  , integralFromByteString
  , padLeft
  , padRight
  , groupN
  , groupNS
  ) where

import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BS
import Data.Bits ( Bits( shift, (.&.) ) )
import Data.Int ( Int64 )

{-# INLINE integralToByteString #-}
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

{-# INLINE padLeft #-}
padLeft :: Char -> Int -> BSS.ByteString -> BSS.ByteString
padLeft p width text
  = if len < width
    then BSSC.replicate (width - len) p <> text
    else text
  where
    len = BSS.length text

{-# INLINE padRight #-}
padRight :: Char -> Int -> BSS.ByteString -> BSS.ByteString
padRight p width text
  = if len < width
    then text <> BSSC.replicate (width - len) p
    else text
  where
    len = BSS.length text

{-# INLINE groupN #-}
groupN :: Int64 -> BS.ByteString -> [BSS.ByteString]
groupN n str
  = if BS.null curr
    then []
    else BS.toStrict curr : groupN n rest
  where
    (curr, rest) = BS.splitAt n str

{-# INLINE groupNS #-}
groupNS :: Int -> BSS.ByteString -> [BSS.ByteString]
groupNS n str
  = if BSS.null curr
    then []
    else curr : groupNS n rest
  where
    (curr, rest) = BSS.splitAt n str
