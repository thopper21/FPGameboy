module Processor where

import Data
import Memory
import Register

data ByteArg = A | B | C | D | E | H | L | BytePointer WordArg | HighRam ByteArg | ImmediateByte
   
data WordArg = AF | BC | DE | HL | SP | WordPointer WordArg | ImmediateWord

data Instruction =
   LD ByteArg ByteArg |
   LDD ByteArg ByteArg |
   LDI ByteArg ByteArg |
   LDH ByteArg ByteArg |
   LD16 WordArg WordArg |
   LDHL WordArg ByteArg |
   PUSH WordArg |
   POP WordArg |
   ADD ByteArg |
   ADC ByteArg |
   SUB ByteArg |
   SBC ByteArg |
   AND ByteArg |
   OR ByteArg |
   XOR ByteArg |
   CP ByteArg |
   INC ByteArg |
   DEC ByteArg |
   ADD16 WordArg |
   ADDSP ByteArg |
   INC16 WordArg |
   DEC16 WordArg |
   SWAP |
   DAA |
   CPL |
   CCF |
   SCF |
   NOP |
   HALT |
   STOP |
   DI |
   EI

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
      0x0A -> LD A (BytePointer BC)
      0x1A -> LD A (BytePointer DE)
      0x7E -> LD A (BytePointer HL)
      0xFA -> LD A (BytePointer ImmediateWord)
      0x3E -> LD A ImmediateByte
      0x47 -> LD B A
      0x40 -> LD B B
      0x41 -> LD B C
      0x42 -> LD B D
      0x43 -> LD B E
      0x44 -> LD B H
      0x45 -> LD B L
      0x46 -> LD B (BytePointer HL)
      0x4F -> LD C A
      0x48 -> LD C B
      0x49 -> LD C C
      0x4A -> LD C D
      0x4B -> LD C E
      0x4C -> LD C H
      0x4D -> LD C L
      0x4E -> LD C (BytePointer HL)
      0x57 -> LD D A
      0x50 -> LD D B
      0x51 -> LD D C
      0x52 -> LD D D
      0x53 -> LD D E
      0x54 -> LD D H
      0x55 -> LD D L
      0x56 -> LD D (BytePointer HL)
      0x5F -> LD E A
      0x58 -> LD E B
      0x59 -> LD E C
      0x5A -> LD E D
      0x5B -> LD E E
      0x5C -> LD E H
      0x5D -> LD E L
      0x5E -> LD E (BytePointer HL)
      0x67 -> LD H A
      0x60 -> LD H B
      0x61 -> LD H C
      0x62 -> LD H D
      0x63 -> LD H E
      0x64 -> LD H H
      0x65 -> LD H L
      0x66 -> LD H (BytePointer HL)
      0x6F -> LD L A
      0x68 -> LD L B
      0x69 -> LD L C
      0x6A -> LD L D
      0x6B -> LD L E
      0x6C -> LD L H
      0x6D -> LD L L
      0x6E -> LD L (BytePointer HL)
      0x70 -> LD (BytePointer HL) B
      0x71 -> LD (BytePointer HL) C
      0x72 -> LD (BytePointer HL) D
      0x73 -> LD (BytePointer HL) E
      0x74 -> LD (BytePointer HL) H
      0x75 -> LD (BytePointer HL) L
      0x36 -> LD (BytePointer HL) ImmediateByte
      0x02 -> LD (BytePointer BC) A
      0x12 -> LD (BytePointer DE) A
      0x77 -> LD (BytePointer HL) A
      0xEA -> LD (BytePointer ImmediateWord) A
      0xF2 -> LD A (HighRam C)
      0xE2 -> LD (HighRam C) A
      0x3A -> LDD A (BytePointer HL)
      0x32 -> LDD (BytePointer HL) A
      0x2A -> LDI A (BytePointer HL)
      0x22 -> LDI (BytePointer HL) A
      0xE0 -> LDH (HighRam ImmediateByte) A
      0xF0 -> LDH A (HighRam ImmediateByte)
      0x01 -> LD16 BC ImmediateWord
      0x11 -> LD16 DE ImmediateWord
      0x21 -> LD16 HL ImmediateWord
      0x31 -> LD16 SP ImmediateWord
      0xF9 -> LD16 SP HL
      0xF8 -> LDHL SP ImmediateByte
      0x08 -> LD16 (WordPointer ImmediateWord) SP
      0xF5 -> PUSH AF
      0xC5 -> PUSH BC
      0xD5 -> PUSH DE
      0xE5 -> PUSH HL
      0xF1 -> POP AF
      0xC1 -> POP BC
      0xD1 -> POP DE
      0xE1 -> POP HL
      0x87 -> ADD A
      0x80 -> ADD B
      0x81 -> ADD C
      0x82 -> ADD D
      0x83 -> ADD E
      0x84 -> ADD H
      0x85 -> ADD L
      0x86 -> ADD (BytePointer HL)
      0xC6 -> ADD ImmediateByte
      0x8F -> ADC A
      0x88 -> ADC B
      0x89 -> ADC C
      0x8A -> ADC D
      0x8B -> ADC E
      0x8C -> ADC H
      0x8D -> ADC L
      0x8E -> ADC (BytePointer HL)
      0xCE -> ADC ImmediateByte
      0x97 -> SUB A
      0x90 -> SUB B
      0x91 -> SUB C
      0x92 -> SUB D
      0x93 -> SUB E
      0x94 -> SUB H
      0x95 -> SUB L
      0x96 -> SUB (BytePointer HL)
      0xD6 -> SUB ImmediateByte
      0x9F -> SBC A
      0x98 -> SBC B
      0x99 -> SBC C
      0x9A -> SBC D
      0x9B -> SBC E
      0x9C -> SBC H
      0x9D -> SBC L
      0x9E -> SBC (BytePointer HL)
      0xDE -> SBC ImmediateByte
      0xA7 -> AND A
      0xA0 -> AND B
      0xA1 -> AND C
      0xA2 -> AND D
      0xA3 -> AND E
      0xA4 -> AND H
      0xA5 -> AND L
      0xA6 -> AND (BytePointer HL)
      0xE6 -> AND ImmediateByte
      0xB7 -> OR A
      0xB0 -> OR B
      0xB1 -> OR C
      0xB2 -> OR D
      0xB3 -> OR E
      0xB4 -> OR H
      0xB5 -> OR L
      0xB6 -> OR (BytePointer HL)
      0xF6 -> OR ImmediateByte
      0xAF -> XOR A
      0xA8 -> XOR B
      0xA9 -> XOR C
      0xAA -> XOR D
      0xAB -> XOR E
      0xAC -> XOR H
      0xAD -> XOR L
      0xAE -> XOR (BytePointer HL)
      0xEE -> XOR ImmediateByte
      0xBF -> CP A
      0xB8 -> CP B
      0xB9 -> CP C
      0xBA -> CP D
      0xBB -> CP E
      0xBC -> CP H
      0xBD -> CP L
      0xBE -> CP (BytePointer HL)
      0xFE -> CP ImmediateByte
      0x3C -> INC A
      0x04 -> INC B
      0x0C -> INC C
      0x14 -> INC D
      0x1C -> INC E
      0x24 -> INC H
      0x2C -> INC L
      0x34 -> INC (BytePointer HL)
      0x3D -> DEC A
      0x05 -> DEC B
      0x0D -> DEC C
      0x15 -> DEC D
      0x1D -> DEC E
      0x25 -> DEC H
      0x2D -> DEC L
      0x35 -> DEC (BytePointer HL)
      0x09 -> ADD16 BC
      0x19 -> ADD16 DE
      0x29 -> ADD16 HL
      0x39 -> ADD16 SP
      0xE8 -> ADDSP ImmediateByte
      0x03 -> INC16 BC
      0x13 -> INC16 DE
      0x23 -> INC16 HL
      0x33 -> INC16 SP
      0x0B -> DEC16 BC
      0x1B -> DEC16 DE
      0x2B -> DEC16 HL
      0x3B -> DEC16 SP
      0xCB -> SWAP
      0x27 -> DAA
      0x2F -> CPL
      0x3F -> CCF
      0x37 -> SCF
      0x00 -> NOP
      0x76 -> HALT
      0x10 -> STOP
      0xF3 -> DI
      0xFB -> EI