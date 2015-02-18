{-# LANGUAGE BangPatterns #-}
module Instruction.Read
  (readInstruction
  ,programWords
  )
  where

import Instruction.Instruction
import Data.Bits
import Data.Word

import Data.Binary.Strict.Get
import Data.ByteString (ByteString)
import Control.Applicative

programWords :: ByteString -> [MachineWord]
programWords b = case fst $ runGet go b of
                   Right ws -> ws
                   Left s -> error s
  where
    go :: Get [MachineWord]
    go = {-# SCC "programWords.go" #-} do
      done <- isEmpty

      if done
        then return []
        else (:) <$> getWord32be <*> go
{-# INLINE programWords #-}

readRegisterTriplet :: MachineWord -> RegisterNumTriplet
readRegisterTriplet w =
  let !c = fromIntegral $  w             .&. 7
      !b = fromIntegral $ (w `shiftR` 3) .&. 7
      !a = fromIntegral $ (w `shiftR` 6) .&. 7
  in
  regNumTriplet a b c

readOpcode :: MachineWord -> Word8
readOpcode w =
  fromIntegral $ (w `shiftR` 28) .&. 0xf

readOrthographyArgs :: MachineWord -> (RegisterNum, MachineWord)
readOrthographyArgs w =
  let !value =  w              .&. (2^(25::Int) - 1)
      !a     = (w `shiftR` 25) .&. 7
  in
  (fromIntegral a, fromIntegral value)

readInstruction :: MachineWord -> Instruction
readInstruction w = {-# SCC "readInstruction" #-}
  case readOpcode w of
    0  ->         CondMove       (readRegisterTriplet w)
    1  ->         ArrayIndex     (readRegisterTriplet w)
    2  ->         ArrayAmendment (readRegisterTriplet w)
    3  ->         Addition       (readRegisterTriplet w)
    4  ->         Multiplication (readRegisterTriplet w)
    5  ->         Division       (readRegisterTriplet w)
    6  ->         NotAnd         (readRegisterTriplet w)

    7  ->         Halt

    8  ->         Allocation     (bRegNum $ readRegisterTriplet w) (cRegNum $ readRegisterTriplet w)
    9  ->         Abandonment                                      (cRegNum $ readRegisterTriplet w)
    10 ->         Output                                           (cRegNum $ readRegisterTriplet w)
    11 ->         Input                                            (cRegNum $ readRegisterTriplet w)
    12 ->         LoadProgram    (bRegNum $ readRegisterTriplet w) (cRegNum $ readRegisterTriplet w)

    13 -> uncurry Orthography    (readOrthographyArgs w)
    n  -> UndefinedInstruction n w
{-# INLINE readInstruction #-}

