module GameBoy where

import Memory
import Register

data GameBoy = GameBoy
   {
      memory :: MemoryMap,
      registers :: RegisterSet
   }