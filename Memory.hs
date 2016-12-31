module Memory where

import Data.Array
import Data.Bits
import Data.Int

type MemoryBank = Array Int16 Int8

data MemoryMap = MemoryMap
   {
      romBanks :: [MemoryBank],
      videoRam :: MemoryBank,
      externalRam :: MemoryBank,
      workingRamBanks :: [MemoryBank],
      highRam :: MemoryBank
   }

kb size = shift 1 10

createBank size = array (0, size - 1) [(i, 0) | i <- [0..size - 1]]

createBanks count size = replicate count $ createBank size

newMemoryMap romBankCount workingRamBankCount = MemoryMap
   {
      romBanks = createBanks romBankCount $ kb 16,
      videoRam = createBank $ kb 8,
      externalRam = createBank $ kb 8,
      workingRamBanks = createBanks workingRamBankCount $ kb 4,
      highRam = createBank 127
   }