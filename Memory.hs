module Memory where

import Data.Int
import Data.Array

type MemoryBank = Array Int16 Int8

data MemoryMap = MemoryMap
   {
      romBanks :: [MemoryBank],
      videoRam :: MemoryBank,
      externalRam :: MemoryBank,
      workingRamBanks :: [MemoryBank],
      spriteAttributes :: MemoryBank,
      highRam :: MemoryBank
   }

