{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data where

import Data.Bits
import Data.Ix
import Data.Word

newtype GBByte = Byte Word8 deriving ( Bits, Enum, Eq, Integral, Num, Ord, Real )

newtype GBWord = Word Word16 deriving ( Bits, Enum, Eq, Ix, Integral, Num, Ord, Real )
   