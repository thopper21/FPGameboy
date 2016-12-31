module Processor where

import Data.Int
import Data.Bits

type EightBitRegister = Int8
type SixteenBitRegister = Int16

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
   } deriving (Show)

sixteenBitRegister highRegister lowRegister =
   let
      high = shift (fromIntegral highRegister) 8
      low = fromIntegral lowRegister
   in high .|. low

bc registerSet = sixteenBitRegister (b registerSet) (c registerSet)

de registerSet = sixteenBitRegister (d registerSet) (c registerSet)

hl registerSet = sixteenBitRegister (h registerSet) (l registerSet)

getFlag registerSet = testBit $ f registerSet

setFlag registerSet bit =
   let
      newFlags = setBit (f registerSet) bit
   in registerSet { f = newFlags }

zero registerSet = getFlag registerSet 7

setZero registerSet = setFlag registerSet 7

subtraction registerSet = getFlag registerSet 6

setSubtraction registerSet = setFlag registerSet 6

halfCarry registerSet = getFlag registerSet 5

setHalfCarry registerSet = setFlag registerSet 5

carry registerSet = getFlag registerSet 4

setCarry registerSet = setFlag registerSet 4