{-# LANGUAGE BangPatterns #-}
module VM.Machine
  (module Control.Monad.State
  ,Platter
  ,Register
  ,RegisterId
  ,Machine
  ,Runtime

  ,loadMachine

  ,printRegisters

  ,jmp
  ,incrFinger
  ,currInstruction

  ,allocateArray
  ,freeArray

  ,getRegister
  ,setRegister

  ,getPlatter
  ,setPlatter

  ,loadZeroArray
  )
  where

import           VM.Memory
import           Instruction.Instruction
import           Instruction.Read

import           Control.Monad.State

import           Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as I

import           Data.ByteString (ByteString)

import           Data.Word

type Register   = Platter
type RegisterId = Word32

data Machine = Machine
  { memory    :: Memory
  , finger    :: !Platter
  , registers :: IntMap Register
  } deriving Show

type Runtime a = StateT Machine IO a

loadMachine :: ByteString -> Machine
loadMachine program
  = Machine
    { memory    = loadIntoMemory (programWords program)
    , finger    = 0
    , registers = I.fromList $ zip [0..7] (repeat 0)
    }

jmp :: ArrayIx -> Runtime ()
jmp ix = modify (\m -> m { finger = ix })

incrFinger :: Runtime ()
incrFinger = modify (\m -> m { finger = finger m + 1 })

printRegisters :: Runtime ()
printRegisters = do
  machine <- get
  liftIO $ print (registers machine)

currInstruction :: Runtime Instruction
currInstruction = {-# SCC "currInstruction" #-} do
  machine <- get
  fmap readInstruction $ getPlatter 0 (finger machine)
{-# INLINE currInstruction #-}

allocateArray :: Word32 -> Runtime ArrayId
allocateArray size = do
  (newId, newMem) <- fmap (allocateMemArray size . memory) get
  modifyMemory (const newMem)
  return newId

freeArray :: ArrayId -> Runtime ()
freeArray arrId =
  modifyMemory (freeMemArray arrId)

getRegister :: RegisterId -> Runtime Register
getRegister regId = do
  machine <- get
  return $ registers machine ! fromIntegral regId

setRegister :: RegisterId -> Register -> Runtime ()
setRegister regId newVal
  = modify (\m -> m { registers = I.insert (fromIntegral regId) newVal (registers m) })

getPlatter :: ArrayId -> ArrayIx -> Runtime Platter
getPlatter arrId ix
  = {-# SCC "getPlatter" #-} fmap (getMemPlatter arrId ix . memory) get
{-# INLINE getPlatter #-}

setPlatter :: ArrayId -> ArrayIx -> Platter -> Runtime ()
setPlatter arrId ix newVal
  = modifyMemory (setMemPlatter arrId ix newVal)

loadZeroArray :: ArrayId -> Runtime ()
loadZeroArray arrId = modifyMemory (loadMemZeroArray arrId)

modifyMemory :: (Memory -> Memory) -> Runtime ()
modifyMemory f
  = {-# SCC "modifyMemory" #-} modify (\machine -> machine { memory = f (memory machine)})
{-# INLINE modifyMemory #-}

