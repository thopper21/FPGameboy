{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Processor where

import Data
import Data.Bits

newtype EightBitRegister = EightBitRegister GBByte deriving ( Bits, Enum, Eq, Integral, Num, Ord, Real )

newtype SixteenBitRegister = SixteenBitRegister GBWord

data RegisterSet = RegisterSet
   {
      a :: EightBitRegister,
      b :: EightBitRegister,
      c :: EightBitRegister,
      d :: EightBitRegister,
      e :: EightBitRegister,
      h :: EightBitRegister,
      l :: EightBitRegister,
      f :: EightBitRegister,
      pc :: SixteenBitRegister,
      sp :: SixteenBitRegister
   }

sixteenBitRegister getHighRegister getLowRegister registerSet =
   let
      highRegister = getHighRegister registerSet
      lowRegister = getLowRegister registerSet
      high = shift (fromIntegral highRegister) 8
      low = fromIntegral lowRegister
   in SixteenBitRegister $ high .|. low
   
bc = sixteenBitRegister b c

de = sixteenBitRegister d c

hl = sixteenBitRegister h l

getFlag bit registerSet = flip testBit bit $ f registerSet

setFlag bit registerSet = registerSet { f = flip setBit bit $ f registerSet }

zero = getFlag 7

setZero = setFlag 7

subtraction = getFlag 6

setSubtraction = setFlag 6

halfCarry = getFlag 5

setHalfCarry = setFlag 5

carry = getFlag 4

setCarry = setFlag 4