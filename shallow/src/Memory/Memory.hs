{-# LANGUAGE FlexibleContexts #-}
module Memory.Memory
  (setReg
  ,getReg
  ,setPlatter
  ,getPlatter
  ,currIp
  ,jmp
  ,incrIp
  ,zero
  ,array
  ,loadZero
  ,ArrayNum
  ,RegisterNum (..)
  ,Platter
  )
  where

import Data.Word
import Data.IntMap.Strict (IntMap)
import Control.Monad.State

import qualified Data.IntMap.Strict as I
import           Data.IntMap.Strict ((!))

import VM.Type

newtype RegisterNum = RegisterNum Word8
type ArrayIndex     = Word32

setReg :: Platter -> RegisterNum -> Instruction ()
setReg w (RegisterNum reg)
  = modifyRegisters (I.insert (fromIntegral reg) w)

getReg :: RegisterNum -> Instruction Register
getReg (RegisterNum reg)
  = withMachine $ (! fromIntegral reg) . registers

setPlatter :: Platter
           -> ArrayIndex
           -> ArrayNum
           -> Instruction ()
setPlatter w arrIx (ArrayNum 0)
  = modifyZeroArray (I.insert (fromIntegral arrIx) w)

setPlatter w arrIx (ArrayNum arrNum)
  = modifyArrays
  $ I.adjust
     (I.insert (fromIntegral arrIx)
               w)
     (fromIntegral arrNum)

getPlatter :: ArrayIndex
           -> ArrayNum
           -> Instruction Platter
getPlatter arrIx (ArrayNum 0)
  = withMachine $ (! fromIntegral arrIx) . zeroArray

currIp :: Instruction Platter
currIp = fmap ip get

jmp :: ArrayIndex -> Instruction ()
jmp loc = modify (\machine -> machine { ip = loc })

incrIp :: Instruction ()
incrIp = modify (\machine ->
  machine { ip = succ (ip machine) })

zero :: ArrayNum
zero = ArrayNum 0

array :: Platter -> ArrayNum
array = ArrayNum

withMachine :: (Machine -> a) -> Instruction a
withMachine f = fmap f get

loadZero :: ArrayNum -> StateT Machine IO ()
loadZero (ArrayNum arrNum) = do
  arr <- fmap zeroArray get
  modifyZeroArray (const arr)


modifyRegisters :: (IntMap Platter -> IntMap Platter)
                -> StateT Machine IO ()
modifyRegisters f
  = modify (\machine ->
      machine { registers = f (registers machine) })

modifyZeroArray :: MonadState Machine m
                => (IntMap Platter -> IntMap Platter)
                -> m ()
modifyZeroArray f
  = modify (\machine ->
      machine { zeroArray = f (zeroArray machine) })

modifyArrays :: MonadState Machine m
             => (IntMap (IntMap Platter)
                 -> IntMap (IntMap Platter))
             -> m ()
modifyArrays f
  = modify (\machine ->
      machine { arrays = f (arrays machine) })

