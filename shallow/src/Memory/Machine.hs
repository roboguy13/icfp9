{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Memory.Machine
  (regA
  ,regB
  ,regC
  )
  where

import Data.Bits
import Data.Word

import Control.Monad.State

import VM.Instruction
import VM.Type
import Memory.Memory

ipSegment :: (Word32 -> a) -> Instruction a
ipSegment f = do
  theIp <- currIp
  w     <- zero # getPlatter theIp
  return (f w)

regA, regB, regC :: Instruction RegisterNum
regA = ipSegment $ \w -> RegisterNum . fromIntegral $ (w `shiftR` 6) .&. 7
regB = ipSegment $ \w -> RegisterNum . fromIntegral $ (w `shiftR` 3) .&. 7
regC = ipSegment $ \w -> RegisterNum . fromIntegral $  w             .&. 7

