module Processor where

import Data
import Data.Word
import Memory
import Register

data ByteArg = A | B | C | D | E | H | L | BytePointer WordArg | HighRam ByteArg | ImmediateByte
   
data WordArg = AF | BC | DE | HL | SP | WordPointer WordArg | ImmediateWord

data JumpCondition = JumpNZ | JumpZ | JumpNC | JumpC

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
   SWAP ByteArg |
   DAA |
   CPL |
   CCF |
   SCF |
   NOP |
   HALT |
   STOP |
   DI |
   EI |
   RLCA |
   RLA |
   RRCA |
   RRA |
   RLC ByteArg |
   RL ByteArg |
   RRC ByteArg |
   RR ByteArg |
   SLA ByteArg |
   SRA ByteArg |
   SRL ByteArg |
   BIT Word8 ByteArg |
   SET Word8 ByteArg |
   RES Word8 ByteArg |
   JP WordArg |
   JPC JumpCondition WordArg |
   JR ByteArg |
   JRC JumpCondition ByteArg |
   CALL WordArg |
   CALLC JumpCondition WordArg
   

newtype Time = Time Int   
   
data Operation = Operation Time Instruction

instruction (Byte opCode) secondInstruction =
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
      0xCB -> complexInstruction secondInstruction
      0x27 -> DAA
      0x2F -> CPL
      0x3F -> CCF
      0x37 -> SCF
      0x00 -> NOP
      0x76 -> HALT
      0x10 -> STOP
      0xF3 -> DI
      0xFB -> EI
      0x07 -> RLCA
      0x17 -> RLA
      0x0F -> RRCA
      0x1F -> RRA
      0xC3 -> JP ImmediateWord
      0xC2 -> JPC JumpNZ ImmediateWord
      0xCA -> JPC JumpZ ImmediateWord
      0xD2 -> JPC JumpNC ImmediateWord
      0xDA -> JPC JumpC ImmediateWord
      0xE9 -> JP (WordPointer HL)
      0x18 -> JR ImmediateByte
      0x20 -> JRC JumpNZ ImmediateByte
      0x28 -> JRC JumpZ ImmediateByte
      0x30 -> JRC JumpNC ImmediateByte
      0x38 -> JRC JumpC ImmediateByte
      0xCD -> CALL ImmediateWord
      0xC4 -> CALLC JumpNZ ImmediateWord
      0xCC -> CALLC JumpZ ImmediateWord
      0xD4 -> CALLC JumpNC ImmediateWord
      0xDC -> CALLC JumpC ImmediateWord
      
complexInstruction (Byte opCode) =
   case opCode of
      0x37 -> SWAP A
      0x30 -> SWAP B
      0x31 -> SWAP C
      0x32 -> SWAP D
      0x33 -> SWAP E
      0x34 -> SWAP H
      0x35 -> SWAP L
      0x36 -> SWAP (BytePointer HL)
      0x07 -> RLC A
      0x00 -> RLC B
      0x01 -> RLC C
      0x02 -> RLC D
      0x03 -> RLC E
      0x04 -> RLC H
      0x05 -> RLC L
      0x06 -> RLC (BytePointer HL)
      0x17 -> RL A
      0x10 -> RL B
      0x11 -> RL C
      0x12 -> RL D
      0x13 -> RL E
      0x14 -> RL H
      0x15 -> RL L
      0x16 -> RL (BytePointer HL)
      0x0F -> RRC A
      0x08 -> RRC B
      0x09 -> RRC C
      0x0A -> RRC D
      0x0B -> RRC E
      0x0C -> RRC H
      0x0D -> RRC L
      0x0E -> RRC (BytePointer HL)
      0x1F -> RR A
      0x18 -> RR B
      0x19 -> RR C
      0x1A -> RR D
      0x1B -> RR E
      0x1C -> RR H
      0x1D -> RR L
      0x1E -> RR (BytePointer HL)
      0x27 -> SLA A
      0x20 -> SLA B
      0x21 -> SLA C
      0x22 -> SLA D
      0x23 -> SLA E
      0x24 -> SLA H
      0x25 -> SLA L
      0x26 -> SLA (BytePointer HL)
      0x2F -> SRA A
      0x28 -> SRA B
      0x29 -> SRA C
      0x2A -> SRA D
      0x2B -> SRA E
      0x2C -> SRA H
      0x2D -> SRA L
      0x2E -> SRA (BytePointer HL)
      0x3F -> SRL A
      0x38 -> SRL B
      0x39 -> SRL C
      0x3A -> SRL D
      0x3B -> SRL E
      0x3C -> SRL H
      0x3D -> SRL L
      0x3E -> SRL (BytePointer HL)
      0x47 -> BIT 0 A
      0x40 -> BIT 0 B
      0x41 -> BIT 0 C
      0x42 -> BIT 0 D
      0x43 -> BIT 0 E
      0x44 -> BIT 0 H
      0x45 -> BIT 0 L
      0x46 -> BIT 0 (BytePointer HL)
      0x4F -> BIT 1 A
      0x48 -> BIT 1 B
      0x49 -> BIT 1 C
      0x4A -> BIT 1 D
      0x4B -> BIT 1 E
      0x4C -> BIT 1 H
      0x4D -> BIT 1 L
      0x4E -> BIT 1 (BytePointer HL)
      0x57 -> BIT 2 A
      0x50 -> BIT 2 B
      0x51 -> BIT 2 C
      0x52 -> BIT 2 D
      0x53 -> BIT 2 E
      0x54 -> BIT 2 H
      0x55 -> BIT 2 L
      0x56 -> BIT 2 (BytePointer HL)
      0x5F -> BIT 3 A
      0x58 -> BIT 3 B
      0x59 -> BIT 3 C
      0x5A -> BIT 3 D
      0x5B -> BIT 3 E
      0x5C -> BIT 3 H
      0x5D -> BIT 3 L
      0x5E -> BIT 3 (BytePointer HL)
      0x67 -> BIT 4 A
      0x60 -> BIT 4 B
      0x61 -> BIT 4 C
      0x62 -> BIT 4 D
      0x63 -> BIT 4 E
      0x64 -> BIT 4 H
      0x65 -> BIT 4 L
      0x66 -> BIT 4 (BytePointer HL)
      0x6F -> BIT 5 A
      0x68 -> BIT 5 B
      0x69 -> BIT 5 C
      0x6A -> BIT 5 D
      0x6B -> BIT 5 E
      0x6C -> BIT 5 H
      0x6D -> BIT 5 L
      0x6E -> BIT 5 (BytePointer HL)
      0x77 -> BIT 6 A
      0x70 -> BIT 6 B
      0x71 -> BIT 6 C
      0x72 -> BIT 6 D
      0x73 -> BIT 6 E
      0x74 -> BIT 6 H
      0x75 -> BIT 6 L
      0x76 -> BIT 6 (BytePointer HL)
      0x7F -> BIT 7 A
      0x78 -> BIT 7 B
      0x79 -> BIT 7 C
      0x7A -> BIT 7 D
      0x7B -> BIT 7 E
      0x7C -> BIT 7 H
      0x7D -> BIT 7 L
      0x7E -> BIT 7 (BytePointer HL)
      0x87 -> RES 0 A
      0x80 -> RES 0 B
      0x81 -> RES 0 C
      0x82 -> RES 0 D
      0x83 -> RES 0 E
      0x84 -> RES 0 H
      0x85 -> RES 0 L
      0x86 -> RES 0 (BytePointer HL)
      0x8F -> RES 1 A
      0x88 -> RES 1 B
      0x89 -> RES 1 C
      0x8A -> RES 1 D
      0x8B -> RES 1 E
      0x8C -> RES 1 H
      0x8D -> RES 1 L
      0x8E -> RES 1 (BytePointer HL)
      0x97 -> RES 2 A
      0x90 -> RES 2 B
      0x91 -> RES 2 C
      0x92 -> RES 2 D
      0x93 -> RES 2 E
      0x94 -> RES 2 H
      0x95 -> RES 2 L
      0x96 -> RES 2 (BytePointer HL)
      0x9F -> RES 3 A
      0x98 -> RES 3 B
      0x99 -> RES 3 C
      0x9A -> RES 3 D
      0x9B -> RES 3 E
      0x9C -> RES 3 H
      0x9D -> RES 3 L
      0x9E -> RES 3 (BytePointer HL)
      0xA7 -> RES 4 A
      0xA0 -> RES 4 B
      0xA1 -> RES 4 C
      0xA2 -> RES 4 D
      0xA3 -> RES 4 E
      0xA4 -> RES 4 H
      0xA5 -> RES 4 L
      0xA6 -> RES 4 (BytePointer HL)
      0xAF -> RES 5 A
      0xA8 -> RES 5 B
      0xA9 -> RES 5 C
      0xAA -> RES 5 D
      0xAB -> RES 5 E
      0xAC -> RES 5 H
      0xAD -> RES 5 L
      0xAE -> RES 5 (BytePointer HL)
      0xB7 -> RES 6 A
      0xB0 -> RES 6 B
      0xB1 -> RES 6 C
      0xB2 -> RES 6 D
      0xB3 -> RES 6 E
      0xB4 -> RES 6 H
      0xB5 -> RES 6 L
      0xB6 -> RES 6 (BytePointer HL)
      0xBF -> RES 7 A
      0xB8 -> RES 7 B
      0xB9 -> RES 7 C
      0xBA -> RES 7 D
      0xBB -> RES 7 E
      0xBC -> RES 7 H
      0xBD -> RES 7 L
      0xBE -> RES 7 (BytePointer HL)
      0xC7 -> SET 0 A
      0xC0 -> SET 0 B
      0xC1 -> SET 0 C
      0xC2 -> SET 0 D
      0xC3 -> SET 0 E
      0xC4 -> SET 0 H
      0xC5 -> SET 0 L
      0xC6 -> SET 0 (BytePointer HL)
      0xCF -> SET 1 A
      0xC8 -> SET 1 B
      0xC9 -> SET 1 C
      0xCA -> SET 1 D
      0xCB -> SET 1 E
      0xCC -> SET 1 H
      0xCD -> SET 1 L
      0xCE -> SET 1 (BytePointer HL)
      0xD7 -> SET 2 A
      0xD0 -> SET 2 B
      0xD1 -> SET 2 C
      0xD2 -> SET 2 D
      0xD3 -> SET 2 E
      0xD4 -> SET 2 H
      0xD5 -> SET 2 L
      0xD6 -> SET 2 (BytePointer HL)
      0xDF -> SET 3 A
      0xD8 -> SET 3 B
      0xD9 -> SET 3 C
      0xDA -> SET 3 D
      0xDB -> SET 3 E
      0xDC -> SET 3 H
      0xDD -> SET 3 L
      0xDE -> SET 3 (BytePointer HL)
      0xE7 -> SET 4 A
      0xE0 -> SET 4 B
      0xE1 -> SET 4 C
      0xE2 -> SET 4 D
      0xE3 -> SET 4 E
      0xE4 -> SET 4 H
      0xE5 -> SET 4 L
      0xE6 -> SET 4 (BytePointer HL)
      0xEF -> SET 5 A
      0xE8 -> SET 5 B
      0xE9 -> SET 5 C
      0xEA -> SET 5 D
      0xEB -> SET 5 E
      0xEC -> SET 5 H
      0xED -> SET 5 L
      0xEE -> SET 5 (BytePointer HL)
      0xF7 -> SET 6 A
      0xF0 -> SET 6 B
      0xF1 -> SET 6 C
      0xF2 -> SET 6 D
      0xF3 -> SET 6 E
      0xF4 -> SET 6 H
      0xF5 -> SET 6 L
      0xF6 -> SET 6 (BytePointer HL)
      0xFF -> SET 7 A
      0xF8 -> SET 7 B
      0xF9 -> SET 7 C
      0xFA -> SET 7 D
      0xFB -> SET 7 E
      0xFC -> SET 7 H
      0xFD -> SET 7 L
      0xFE -> SET 7 (BytePointer HL)