module Processor where

import Data
import Memory
import Register

data ProcessorState =
   ProcessorState
   {
      registers :: RegisterSet,
      memory :: MemoryMap,
      time :: Int
   }
   
immediateByte state = readByte (memory state) (toAddress (pc . registers $ state))

loadRegister state setRegister getByte = state { registers = setRegister (registers state) (getByte state) }

performInstruction state (Byte opCode) =
   case opCode of
      0x06 -> loadRegister state setB immediateByte
      0x0E -> loadRegister state setC immediateByte
      0x16 -> loadRegister state setD immediateByte
      0x1E -> loadRegister state setE immediateByte
      0x26 -> loadRegister state setH immediateByte
      0x2E -> loadRegister state setL immediateByte