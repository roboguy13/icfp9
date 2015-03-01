{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}
module VM.VM
  where

import Memory.Machine
import Memory.Memory

import VM.Instruction

import Data.Bits

import System.Exit

import Control.Exception

import Data.Char

import Control.Monad.State.Strict

liftBinaryOp :: (Platter -> Platter -> Platter)
             -> Instruction ()
liftBinaryOp op = do
  b <- regB # getReg
  c <- regC # getReg
  regA # setReg (b `op` c)

notAnd :: Platter -> Platter -> Platter
notAnd a b
  = complement (a .&. b)

spinCycle :: Instruction ()
spinCycle = do
  w <- (`getPlatter` 0) =<< currIp
  let instr = w `shiftR` 28
  if | instr == 0 -> do  -- Conditional move
      c <- regC # getReg
      if c /= 0
        then do
          b <- regB # getReg
          regA # setReg b
        else return ()

     | instr == 1 -> do  -- Array index
      b <- regB # getReg
      c <- regC # getReg
      x <- getPlatter c b
      regA # setReg x

     | instr == 2 -> do  -- Array amendment
      a <- regA # getReg
      b <- regB # getReg
      c <- regC # getReg
      setPlatter c b a

     | instr == 3 ->     -- Addition
      liftBinaryOp (+)

     | instr == 4 ->     -- Multiplication
      liftBinaryOp (*)

     | instr == 5 ->     -- Division
      liftBinaryOp quot

     | instr == 6 ->     -- Not-And
      liftBinaryOp notAnd

     | instr == 7 ->     -- Halt
      liftIO exitSuccess

     | instr == 8 -> do   -- Allocation
      c <- regC # getReg
      arrNum <- allocArray c
      regB # setReg arrNum

     | instr == 9 -> do   -- Abandonment
      c <- regC # getReg
      freeArray c

     | instr == 10 -> do  -- Output
      c <- regC # getReg
      liftIO . putChar . chr $ fromIntegral c

     | instr == 11 -> do  -- Input
      c <- liftIO
         . catch getChar
         $ \(_ :: IOError) -> return (chr 0xff)
      regC # setReg (fromIntegral (ord c))

     | instr == 12 -> do  -- Load program
      b <- regB # getReg
      c <- regC # getReg
      loadZero b
      jmp (c-1)

     | instr == 13 -> do  -- Orthography
      val <- orthographyVal
      orthographyReg # setReg val

     | otherwise -> error $ "Invalid instruction: " ++ show instr

  incrIp
