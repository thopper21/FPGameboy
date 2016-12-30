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

combineRegisters highRegister lowRegister =
   let
      high = shift (fromIntegral highRegister) 8
      low = fromIntegral lowRegister
   in high .|. low

bc registerSet = combineRegisters (b registerSet) (c registerSet)

de registerSet = combineRegisters (d registerSet) (c registerSet)

hl registerSet = combineRegisters (h registerSet) (l registerSet)