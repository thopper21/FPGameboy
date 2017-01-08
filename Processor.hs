module Processor where

import Data
import Memory
import Register

data Argument = A | B | C | D | E | H | L | HL | ImmediateByte

data Instruction =
   LD Argument Argument

newtype Time = Time Int   
   
data Operation = Operation Time Instruction

operation (Byte opCode) =
   case opCode of
      0x06 -> LD B ImmediateByte
      0x0E -> LD C ImmediateByte
      0x16 -> LD D ImmediateByte
      0x1E -> LD E ImmediateByte
      0x26 -> LD H ImmediateByte
      0x2E -> LD L ImmediateByte
      0x7F -> LD A A
      0x78 -> LD A B
      0x79 -> LD A C
      0x7A -> LD A D
      0x7B -> LD A E
      0x7C -> LD A H
      0x7D -> LD A L
      0x7E -> LD A HL