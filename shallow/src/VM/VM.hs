module VM.VM
  where

import Memory.Machine
import Memory.Memory
import VM.Instruction

import Data.Bits

spinCycle :: Instruction ()
spinCycle = do
  w <- currIp
  case w `shiftR` 28 of
    0 -> do
      undefined
      regA # (setReg .= (getReg =<< regB))
--      b <- getReg regB
--      regA # (fmap setReg (getReg regB))

