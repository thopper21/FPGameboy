{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Memory where

import Data
import Data.Array
import Data.Bits

newtype MemoryBank = MemoryBank (Array GBWord GBByte)

newtype Address = Address GBWord deriving ( Eq, Num, Ord )

data MappedAddress =
   FixedRomBank Address |
   SwitchableRomBank Address |
   VideoRam Address |
   ExternalRam Address |
   FixedWorkingRam Address |
   SwitchableWorkingRam Address |
   ObjectAttributeMemory Address |
   Unusable |
   IOPorts Address |
   HighRam Address |
   InterruptEnableRegister

data MemoryMap = MemoryMap
   {
      romBanks :: [MemoryBank],
      videoRam :: MemoryBank,
      externalRamBanks :: [MemoryBank],
      workingRamBanks :: [MemoryBank],
      spriteAttributeTable :: MemoryBank,
      ioPorts :: MemoryBank,
      highRam :: MemoryBank,
      interruptEnableRegister :: GBByte
   }

kb size = shift 1 10

createBank size = MemoryBank $ listArray (Word 0, Word $ fromIntegral $ size - 1) (replicate (size - 1) (Byte 0))

createBanks count size = replicate count $ createBank size

newMemoryMap romBankCount externalRamBankCount workingRamBankCount = MemoryMap
   {
      romBanks = createBanks romBankCount $ kb 16,
      videoRam = createBank $ kb 8,
      externalRamBanks = createBanks externalRamBankCount $ kb 8,
      workingRamBanks = createBanks workingRamBankCount $ kb 4,
      spriteAttributeTable = createBank 160,
      ioPorts = createBank 128,
      highRam = createBank 127,
      interruptEnableRegister = Byte 0
   }

createMappedAddress address
   | address < 0x4000 = FixedRomBank address
   | address < 0x8000 = SwitchableRomBank $ address - 0x4000
   | address < 0xA000 = VideoRam $ address - 0x8000
   | address < 0xC000 = ExternalRam $ address - 0xA000
   | address < 0xD000 = FixedWorkingRam $ address - 0xC000
   | address < 0xE000 = SwitchableWorkingRam $ address - 0xD000
   | address < 0xFE00 = createMappedAddress $ address - 0x2000
   | address < 0xFEA0 = ObjectAttributeMemory $ address - 0xFE00
   | address < 0xFF00 = Unusable
   | address < 0xFF80 = IOPorts $ address - 0xFF00
   | address < 0xFFFF = HighRam $ address - 0xFF80
   | otherwise = InterruptEnableRegister

readByteFromBank (MemoryBank bankData) (Address address) = bankData ! address

readByteFromMappedAddress memoryMap mappedAddress = case mappedAddress of
   FixedRomBank address -> readByteFromBank (head $ romBanks memoryMap) address
   SwitchableRomBank address -> readByteFromBank (romBanks memoryMap !! 1) address
   VideoRam address -> readByteFromBank (videoRam memoryMap) address
   ExternalRam address -> readByteFromBank (head $ externalRamBanks memoryMap) address
   FixedWorkingRam address -> readByteFromBank (head $ workingRamBanks memoryMap) address
   SwitchableWorkingRam address -> readByteFromBank (workingRamBanks memoryMap !! 1) address
   ObjectAttributeMemory address -> readByteFromBank (spriteAttributeTable memoryMap) address
   Unusable -> 0
   IOPorts address -> readByteFromBank (ioPorts memoryMap) address
   HighRam address -> readByteFromBank (highRam memoryMap) address
   InterruptEnableRegister -> interruptEnableRegister memoryMap

readByte memoryMap address = readByteFromMappedAddress memoryMap $ createMappedAddress address

writeByteToBank (MemoryBank bankData) (Address address) byte = MemoryBank (bankData // [(address, byte)])

writeByteToBankAtIndex memoryBanks index address byte = case index of
   0 -> writeByteToBank (head memoryBanks) address byte : tail memoryBanks
   _ -> head memoryBanks : writeByteToBankAtIndex (tail memoryBanks) (index - 1) address byte

writeByteToMappedAddress memoryMap mappedAddress byte = case mappedAddress of
   FixedRomBank address -> let
         newRomBanks = writeByteToBankAtIndex (romBanks memoryMap) 0 address byte
      in memoryMap { romBanks = newRomBanks }
   SwitchableRomBank address -> let
         newRomBanks = writeByteToBankAtIndex (romBanks memoryMap) 1 address byte
      in memoryMap { romBanks = newRomBanks }
   VideoRam address -> let
         newVideoRam = writeByteToBank (videoRam memoryMap) address byte
      in memoryMap { videoRam = newVideoRam }
   ExternalRam address -> let
         newExternalRamBanks = writeByteToBankAtIndex (externalRamBanks memoryMap) 0 address byte
      in memoryMap { externalRamBanks = newExternalRamBanks }
   FixedWorkingRam address -> let
         newWorkingRamBanks = writeByteToBankAtIndex (workingRamBanks memoryMap) 0 address byte
      in memoryMap { workingRamBanks = newWorkingRamBanks }
   SwitchableWorkingRam address -> let
         newWorkingRamBanks = writeByteToBankAtIndex (workingRamBanks memoryMap) 1 address byte
      in memoryMap { workingRamBanks = newWorkingRamBanks }
   ObjectAttributeMemory address -> let
         newSpriteAttributeTable = writeByteToBank (spriteAttributeTable memoryMap) address byte
      in memoryMap { spriteAttributeTable = newSpriteAttributeTable }
   Unusable -> memoryMap
   IOPorts address -> let
         newIOPorts = writeByteToBank (ioPorts memoryMap) address byte
      in memoryMap { ioPorts = newIOPorts }
   HighRam address -> let
         newHighRam = writeByteToBank (highRam memoryMap) address byte
      in memoryMap { highRam = newHighRam }
   InterruptEnableRegister -> memoryMap { interruptEnableRegister = byte }

writeByte memoryMap address = writeByteToMappedAddress memoryMap $ createMappedAddress address