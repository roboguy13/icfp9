{-# LANGUAGE ScopedTypeVariables #-}
module VM.VM
  where

import Memory.Machine
import Memory.Memory

import VM.Instruction
import VM.Type

import Data.Bits
import Data.IntMap.Strict ((!))

import System.Exit

import Control.Exception

import Data.Char

import Control.Monad.State

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
  w <- (`getPlatter` array 0) =<< currIp
--  liftIO $ print (w `shiftR` 28)
  case w `shiftR` 28 of
    0 -> do  -- Conditional move
      c <- regC # getReg
      if c /= 0
        then do
          b <- regB # getReg
          regA # setReg b
        else return ()

    1 -> do  -- Array index
      b <- regB # getReg
      c <- regC # getReg
      x <- getPlatter c (array b)
      regA # setReg x

    2 -> do  -- Array amendment
      a <- regA # getReg
      b <- regB # getReg
      c <- regC # getReg
      setPlatter c b (array a)

    3 ->     -- Addition
      liftBinaryOp (+)

    4 ->     -- Multiplication
      liftBinaryOp (*)

    5 ->     -- Division
      liftBinaryOp quot

    6 ->     -- Not-And
      liftBinaryOp notAnd

    7 ->     -- Halt
      liftIO exitSuccess

    8 -> do   -- Allocation
      c <- regC # getReg
      ArrayNum arrNum <- allocArray c
      regB # setReg arrNum

    9 -> do   -- Abandonment
      c <- regC # getReg
      freeArray (array c)

    10 -> do  -- Output
      c <- regC # getReg
      liftIO . putChar . chr $ fromIntegral c

    11 -> do  -- Input
      c <- liftIO
         . catch getChar
         $ \(e :: IOError) -> return (chr 0xff)
      regC # setReg (fromIntegral (ord c))

    12 -> do  -- Load program
      b <- regB # getReg
      c <- regC # getReg
      loadZero (array b)
      jmp (c-1)

    13 -> do  -- Orthography
      val <- orthographyVal
      orthographyReg # setReg val

  incrIp
