{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Register
   (EightBitRegister,
   SixteenBitRegister,
   RegisterSet,
   emptyRegisterSet,
   a, b, c, d, e, h, l, f,
   setA, setB, setC, setD, setE, setH, setL,
   getA, getB, getC, getD, getE, getH, getL,
   bc, de, hl, pc, sp,
   setPC, setSP,
   zero, setZero,
   subtraction, setSubtraction,
   halfCarry, setHalfCarry,
   carry, setCarry,
   toAddress)
where

import Data
import Data.Bits
import Memory

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

emptyEightBitRegister = EightBitRegister . Byte $ 0

emptySixteenBitRegister = SixteenBitRegister . Word $ 0
   
emptyRegisterSet =
   RegisterSet
   {
      a = emptyEightBitRegister,
      b = emptyEightBitRegister,
      c = emptyEightBitRegister,
      d = emptyEightBitRegister,
      e = emptyEightBitRegister,
      h = emptyEightBitRegister,
      l = emptyEightBitRegister,
      f = emptyEightBitRegister,
      pc = emptySixteenBitRegister,
      sp = emptySixteenBitRegister
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

setA registerSet byte = registerSet { a = EightBitRegister byte }

setB registerSet byte = registerSet { b = EightBitRegister byte }

setC registerSet byte = registerSet { c = EightBitRegister byte }

setD registerSet byte = registerSet { d = EightBitRegister byte }

setE registerSet byte = registerSet { e = EightBitRegister byte }

setH registerSet byte = registerSet { h = EightBitRegister byte }

setL registerSet byte = registerSet { l = EightBitRegister byte }

setPC registerSet word = registerSet { pc = SixteenBitRegister word }

setSP registerSet word = registerSet { sp = SixteenBitRegister word }

getByte (EightBitRegister byte) = byte

getWord (SixteenBitRegister word) = word

getA = getByte . a

getB = getByte . b

getC = getByte . c

getD = getByte . d

getE = getByte . e

getH = getByte . h

getL = getByte . l

getBC = getWord . bc

getDE = getWord . de

getHL = getWord . hl

getPC = getWord . pc

getSP = getWord . sp

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

toAddress (SixteenBitRegister word) = Address word