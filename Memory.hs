{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Memory where

import Data
import Data.Array
import Data.Bits

newtype Address = Address GBWord deriving ( Eq, Ix, Num, Ord )

newtype MemoryBank = MemoryBank (Array Address GBByte)

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

kb = (*) $ shift 1 10

createBank size = MemoryBank $ listArray (Address . Word $ 0, Address . Word . fromIntegral $ size - 1) (replicate (size - 1) (Byte 0))

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
   
minus = flip (-)
   
createMappedAddress address =
   let
      factory
         | address < 0x4000 = FixedRomBank
         | address < 0x8000 = SwitchableRomBank . minus 0x4000
         | address < 0xA000 = VideoRam . minus 0x8000
         | address < 0xC000 = ExternalRam . minus 0xA000
         | address < 0xD000 = FixedWorkingRam . minus 0xC000
         | address < 0xE000 = SwitchableWorkingRam . minus 0xD000
         | address < 0xFE00 = createMappedAddress . minus 0x2000
         | address < 0xFEA0 = ObjectAttributeMemory . minus 0xFE00
         | address < 0xFF00 = const Unusable
         | address < 0xFF80 = IOPorts . minus 0xFF00
         | address < 0xFFFF = HighRam . minus 0xFF80
         | otherwise = const InterruptEnableRegister
   in
      factory address

readByteFromBank (MemoryBank bankData) address = bankData ! address

readByteFromMappedAddress memoryMap mappedAddress =
   case mappedAddress of
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

writeByteToBank (MemoryBank bankData) address byte = MemoryBank (bankData // [(address, byte)])

writeByteToBankAtIndex memoryBanks index address byte =
   case index of
      0 -> writeByteToBank (head memoryBanks) address byte : tail memoryBanks
      _ -> head memoryBanks : writeByteToBankAtIndex (tail memoryBanks) (index - 1) address byte

writeByteToMappedAddress memoryMap mappedAddress byte = case mappedAddress of
   FixedRomBank address ->
      let newRomBanks = writeByteToBankAtIndex (romBanks memoryMap) 0 address byte
      in memoryMap { romBanks = newRomBanks }
   SwitchableRomBank address ->
      let newRomBanks = writeByteToBankAtIndex (romBanks memoryMap) 1 address byte
      in memoryMap { romBanks = newRomBanks }
   VideoRam address ->
      let newVideoRam = writeByteToBank (videoRam memoryMap) address byte
      in memoryMap { videoRam = newVideoRam }
   ExternalRam address ->
      let newExternalRamBanks = writeByteToBankAtIndex (externalRamBanks memoryMap) 0 address byte
      in memoryMap { externalRamBanks = newExternalRamBanks }
   FixedWorkingRam address ->
      let newWorkingRamBanks = writeByteToBankAtIndex (workingRamBanks memoryMap) 0 address byte
      in memoryMap { workingRamBanks = newWorkingRamBanks }
   SwitchableWorkingRam address ->
      let newWorkingRamBanks = writeByteToBankAtIndex (workingRamBanks memoryMap) 1 address byte
      in memoryMap { workingRamBanks = newWorkingRamBanks }
   ObjectAttributeMemory address ->
      let newSpriteAttributeTable = writeByteToBank (spriteAttributeTable memoryMap) address byte
      in memoryMap { spriteAttributeTable = newSpriteAttributeTable }
   Unusable -> memoryMap
   IOPorts address ->
      let newIOPorts = writeByteToBank (ioPorts memoryMap) address byte
      in memoryMap { ioPorts = newIOPorts }
   HighRam address ->
      let newHighRam = writeByteToBank (highRam memoryMap) address byte
      in memoryMap { highRam = newHighRam }
   InterruptEnableRegister -> memoryMap { interruptEnableRegister = byte }

writeByte memoryMap address = writeByteToMappedAddress memoryMap $ createMappedAddress address