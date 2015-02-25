{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Memory.Machine
  (regA
  ,regB
  ,regC

  ,orthographyReg
  ,orthographyVal

  ,allocArray
  ,freeArray
  )
  where

import Data.Bits
import Data.Word

import Control.Monad.State

import VM.Instruction
import VM.Type
import Memory.Memory

import qualified Data.IntMap.Strict as I

import Debug.Trace

ipSegment :: (Word32 -> a) -> Instruction a
ipSegment f = do
  theIp <- currIp
  w     <- return zero # getPlatter theIp
  return (f w)

regA, regB, regC :: Instruction RegisterNum
regA = ipSegment $ \w ->
  RegisterNum . fromIntegral $ (w `shiftR` 6) .&. 7
regB = ipSegment $ \w ->
  RegisterNum . fromIntegral $ (w `shiftR` 3) .&. 7
regC = ipSegment $ \w ->
  RegisterNum . fromIntegral $  w             .&. 7

orthographyReg :: Instruction RegisterNum
orthographyReg = ipSegment $ \w ->
  RegisterNum . fromIntegral $ (w `shiftR` 25) .&. 7

orthographyVal :: Instruction Platter
orthographyVal = ipSegment $ \w ->
  w              .&. (2^(25::Int) - 1)


allocArray :: Platter -> Instruction ArrayNum
allocArray size = do
  machine <- get
  ix <- case freeList machine of
             [] ->
                 return
                  . fromIntegral
                  . (+1)
                  . fst
                  . I.findMax
                  $ arrays machine
             (ArrayNum 0:_) -> error "Array #0 is in free list"
             (ArrayNum i:rest) -> do
               modify (\machine ->
                 machine { freeList = rest })
               return i

  modify (\machine ->
        machine
          { arrays
              = I.insert (fromIntegral ix)
                         (I.fromList
                          (zip [0..fromIntegral size-1]
                               (repeat 0)))
                         (arrays machine) })
  return $ array ix

freeArray :: ArrayNum -> Instruction ()
freeArray arrNum = do
  modify (\machine ->
    machine { freeList = arrNum : freeList machine })

