module Internal.Bytes where

import qualified Data.ByteString.Char8 as C (singleton)
import qualified Data.ByteString as B (ByteString, head, length, cons)
import Data.Char (chr, ord)
import Data.Word (Word8)

import Data.Int (Int8)

-- Here we define some of our customized byte interpretation
-- and packaging among other things

-- A Word8 with clamping addtion, subtraction and multiplication
newtype U8 = U8
  { u8 :: Word8
  } deriving (Eq, Ord, Enum)

instance Num U8 where
  (*) (U8 x) (U8 y) = let z = x * y in
                          if z < x
                             then U8 255
                             else U8 z
  abs = U8 . abs . u8
  signum = U8 . signum . u8
  fromInteger = U8 . fromInteger
  (+) (U8 x) (U8 y) = let z = x + y in
                          if z < x
                             then U8 255
                             else U8 z
  (-) (U8 x) (U8 y) = let z = x - y in
                          if x < z
                             then U8 0
                             else U8 z

instance Show U8 where
  show = show . fromEnum . u8

-- Only defined because of some weird stuff going on. I would have presumed that
-- deriving Enum would have given the same functionality
enumToU8 :: Enum a => a -> U8
enumToU8 = toEnum . fromEnum

u8ToEnum :: Enum a => U8 -> a
u8ToEnum = toEnum . fromEnum

charToU8 :: Char -> U8
charToU8 = U8 . B.head . C.singleton
