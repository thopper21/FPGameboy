module Memory where

import Data.Array
import Data.Bits
import Data.Word

newtype MemoryBank = MemoryBank (Array Word16 Word8)

newtype Address = Address Word16

data MemoryMap = MemoryMap
   {
      romBanks :: [MemoryBank],
      videoRam :: MemoryBank,
      externalRamBanks :: [MemoryBank],
      workingRamBanks :: [MemoryBank],
      highRam :: MemoryBank
   }

kb size = shift 1 10

createBank size = MemoryBank $ array (0, size - 1) [(i, 0) | i <- [0..size - 1]]

createBanks count size = replicate count $ createBank size

newMemoryMap romBankCount externalRamBankCount workingRamBankCount = MemoryMap
   {
      romBanks = createBanks romBankCount $ kb 16,
      videoRam = createBank $ kb 8,
      externalRamBanks = createBanks externalRamBankCount $ kb 8,
      workingRamBanks = createBanks workingRamBankCount $ kb 4,
      highRam = createBank 127
   }

readByteFromBank (MemoryBank bankData) address = bankData ! address

readByte memoryMap (Address address)
   | address < 0x4000 = readByteFromBank (head $ romBanks memoryMap) address
   | address < 0x8000 = readByteFromBank (romBanks memoryMap !! 1) (address - 0x4000)
   | address < 0xA000 = readByteFromBank (videoRam memoryMap) (address - 0x8000)
   | address < 0xC000 = readByteFromBank (head $ externalRamBanks memoryMap) (address - 0xA000)
   | address < 0xD000 = readByteFromBank (head $ workingRamBanks memoryMap) (address - 0xC000)
   | address < 0xE000 = readByteFromBank (workingRamBanks memoryMap !! 1) (address - 0xD000)
   | address < 0xFE00 = readByte memoryMap $ Address (address - 0x2000)
   | address < 0xFEA0 = 0
   | address < 0xFF00 = 0
   | address < 0xFF80 = 0
   | address < 0xFFFF = readByteFromBank (highRam memoryMap) (address - 0xFF00)
   | otherwise = 0

writeByteToBank (MemoryBank bankData) address byte = MemoryBank (bankData // [(address, byte)])

writeByte memoryMap (Address address) byte
   | address < 0x4000 = let
         oldRomBanks = romBanks memoryMap
         newRomBank = writeByteToBank (head oldRomBanks) address byte
         newRomBanks = newRomBank : tail oldRomBanks
      in memoryMap { romBanks = newRomBanks }
   | address < 0x8000 = let
         oldRomBanks = romBanks memoryMap
         newRomBank = writeByteToBank (oldRomBanks !! 1) (address - 0x4000) byte
         newRomBanks = head oldRomBanks : newRomBank : drop 2 oldRomBanks
      in memoryMap { romBanks = newRomBanks }
   | address < 0xA000 = let
         newVideoRam = writeByteToBank (videoRam memoryMap) (address - 0x8000) byte
      in memoryMap { videoRam = newVideoRam }
   | address < 0xC000 = let
         oldExternalRamBanks = externalRamBanks memoryMap
         newExternalRamBank = writeByteToBank (head oldExternalRamBanks) (address - 0xA000) byte
         newExternalRamBanks = newExternalRamBank : tail oldExternalRamBanks
      in memoryMap { externalRamBanks = newExternalRamBanks }
   | address < 0xD000 = let
         oldWorkingRamBanks = workingRamBanks memoryMap
         newWorkingRamBank = writeByteToBank (head oldWorkingRamBanks) (address - 0xC000) byte
         newWorkingRamBanks = newWorkingRamBank : tail oldWorkingRamBanks
      in memoryMap { workingRamBanks = newWorkingRamBanks }
   | address < 0xE000 = let
         oldWorkingRamBanks = workingRamBanks memoryMap
         newWorkingRamBank = writeByteToBank (oldWorkingRamBanks !! 1) (address - 0x4000) byte
         newWorkingRamBanks = head oldWorkingRamBanks : newWorkingRamBank : drop 2 oldWorkingRamBanks
      in memoryMap { workingRamBanks = newWorkingRamBanks }
   | address < 0xFE00 = writeByte memoryMap (Address (address - 0x2000)) byte
   | address < 0xFEA0 = memoryMap
   | address < 0xFF00 = memoryMap
   | address < 0xFF80 = memoryMap
   | address < 0xFFFF = let
         newHighRam = writeByteToBank (highRam memoryMap) (address - 0xFF00) byte
      in memoryMap { highRam = newHighRam }
   | otherwise = memoryMap