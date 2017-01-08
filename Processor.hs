module Processor where

import Data
import Memory
import Register

data ByteArg = A | B | C | D | E | H | L | Pointer WordArg | HighRam ByteArg | ImmediateByte
   
data WordArg = BC | DE | HL | SP | ImmediateWord

data Instruction =
   LD ByteArg ByteArg |
   LDD ByteArg ByteArg |
   LDI ByteArg ByteArg |
   LDH ByteArg ByteArg

newtype Time = Time Int   
   
data Operation = Operation Time Instruction

instruction (Byte opCode) =
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
      0x0A -> LD A (Pointer BC)
      0x1A -> LD A (Pointer DE)
      0x7E -> LD A (Pointer HL)
      0xFA -> LD A (Pointer ImmediateWord)
      0x3E -> LD A ImmediateByte
      0x47 -> LD B A
      0x40 -> LD B B
      0x41 -> LD B C
      0x42 -> LD B D
      0x43 -> LD B E
      0x44 -> LD B H
      0x45 -> LD B L
      0x46 -> LD B (Pointer HL)
      0x4F -> LD C A
      0x48 -> LD C B
      0x49 -> LD C C
      0x4A -> LD C D
      0x4B -> LD C E
      0x4C -> LD C H
      0x4D -> LD C L
      0x4E -> LD C (Pointer HL)
      0x57 -> LD D A
      0x50 -> LD D B
      0x51 -> LD D C
      0x52 -> LD D D
      0x53 -> LD D E
      0x54 -> LD D H
      0x55 -> LD D L
      0x56 -> LD D (Pointer HL)
      0x5F -> LD E A
      0x58 -> LD E B
      0x59 -> LD E C
      0x5A -> LD E D
      0x5B -> LD E E
      0x5C -> LD E H
      0x5D -> LD E L
      0x5E -> LD E (Pointer HL)
      0x67 -> LD H A
      0x60 -> LD H B
      0x61 -> LD H C
      0x62 -> LD H D
      0x63 -> LD H E
      0x64 -> LD H H
      0x65 -> LD H L
      0x66 -> LD H (Pointer HL)
      0x6F -> LD L A
      0x68 -> LD L B
      0x69 -> LD L C
      0x6A -> LD L D
      0x6B -> LD L E
      0x6C -> LD L H
      0x6D -> LD L L
      0x6E -> LD L (Pointer HL)
      0x70 -> LD (Pointer HL) B
      0x71 -> LD (Pointer HL) C
      0x72 -> LD (Pointer HL) D
      0x73 -> LD (Pointer HL) E
      0x74 -> LD (Pointer HL) H
      0x75 -> LD (Pointer HL) L
      0x36 -> LD (Pointer HL) ImmediateByte
      0x02 -> LD (Pointer BC) A
      0x12 -> LD (Pointer DE) A
      0x77 -> LD (Pointer HL) A
      0xEA -> LD (Pointer ImmediateWord) A
      0xF2 -> LD A (HighRam C)
      0xE2 -> LD (HighRam C) A
      0x3A -> LDD A (Pointer HL)
      0x32 -> LDD (Pointer HL) A
      0x2A -> LDI A (Pointer HL)
      0x22 -> LDI (Pointer HL) A
      0xE0 -> LDH (HighRam ImmediateByte) A
      0xF0 -> LDH A (HighRam ImmediateByte)