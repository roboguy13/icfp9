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
  ,loadZero
  ,ArrayNum
  ,RegisterNum (..)
  ,Platter
  )
  where

import Data.Word
import Data.IntMap.Strict (IntMap)
import Control.Monad.State.Strict

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
setPlatter w arrIx 0
  = modifyZeroArray (I.insert (fromIntegral arrIx) w)

setPlatter w arrIx arrNum
  = modifyArrays
  $ I.adjust
     (I.insert (fromIntegral arrIx)
               w)
     (fromIntegral arrNum)

getPlatter :: ArrayIndex
           -> ArrayNum
           -> Instruction Platter
getPlatter arrIx 0
  = withMachine $ (! fromIntegral arrIx) . zeroArray

getPlatter arrIx arrNum
  = withMachine
    $ indexArr
    . arrays
  where
    indexArr arr = (arr ! fromIntegral arrNum)
                          ! fromIntegral arrIx


currIp :: Instruction Platter
currIp = fmap ip get

jmp :: ArrayIndex -> Instruction ()
jmp loc = modify (\machine -> machine { ip = loc })

incrIp :: Instruction ()
incrIp = modify (\machine ->
  machine { ip = ip machine + 1 })

zero :: ArrayNum
zero = 0

withMachine :: (Machine -> a) -> Instruction a
withMachine f = {-# SCC "withMachine" #-} fmap f get
{-# INLINE withMachine #-}

loadZero :: ArrayNum -> StateT Machine IO ()
loadZero 0      = return ()
loadZero arrNum = do
  arr <- fmap ((! fromIntegral arrNum) . arrays) get
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

