module VM.VM
  (runProgram
  )
  where

import           VM.Machine
import           Instruction.Instruction

import           Data.ByteString (ByteString)

import           Data.Bits

import           System.Exit

import           Data.Char (chr, ord)

runProgram :: ByteString -> IO ()
runProgram = evalStateT (forever spinCycle) . loadMachine

spinCycle :: Runtime ()
spinCycle = {-# SCC "spinCycle" #-} do
  machine <- get
  instr <- currInstruction
--  liftIO $ print instr
--  printRegisters
  runInstruction instr
  incrFinger
{-# INLINE spinCycle #-}

runInstruction :: Instruction -> Runtime ()
runInstruction instruction
  = {-# SCC "runInstruction" #-}
    case instruction of
      CondMove   args              -> condMove args
      ArrayIndex args              -> arrayIndex args
      ArrayAmendment args          -> arrayAmendment args
      Addition   args              -> addition args
      Multiplication args          -> multiplication args
      Division args                -> division args
      NotAnd args                  -> notAnd args

      Halt                         -> halt
      Allocation bRegNum cRegNum   -> allocation bRegNum cRegNum
      Abandonment cRegNum          -> abandonment cRegNum
      Output cRegNum               -> output cRegNum
      Input cRegNum                -> input cRegNum
      LoadProgram bRegNum cRegNum  -> loadProgram bRegNum cRegNum
      Orthography aRegNum val      -> orthography aRegNum val
      i@(UndefinedInstruction _ _) -> error $ "Tried to run instruction " ++ show i
{-# INLINE runInstruction #-}

condMove :: RegisterNumTriplet -> Runtime ()
condMove args = do
  cReg <- getRegister $ cRegNum args
  bReg <- getRegister $ bRegNum args
  if cReg == 0
    then return ()
    else setRegister (aRegNum args) bReg

arrayIndex :: RegisterNumTriplet -> Runtime ()
arrayIndex args = do
  ix    <- getRegister $ cRegNum args
  arrId <- getRegister $ bRegNum args
  val   <- getPlatter arrId ix

  setRegister (aRegNum args) val

arrayAmendment :: RegisterNumTriplet -> Runtime ()
arrayAmendment args = do
  arrId <- getRegister $ aRegNum args
  ix    <- getRegister $ bRegNum args
  val   <- getRegister $ cRegNum args

  setPlatter arrId ix val

liftBinOp :: (Platter -> Platter -> Platter) -> RegisterNumTriplet -> Runtime ()
liftBinOp op args = do
  bReg <- getRegister $ bRegNum args
  cReg <- getRegister $ cRegNum args

  setRegister (aRegNum args) (bReg `op` cReg)


addition, multiplication, division, notAnd :: RegisterNumTriplet -> Runtime ()
addition       = liftBinOp (+)
multiplication = liftBinOp (*)
division       = liftBinOp quot
notAnd         = liftBinOp (\a b -> complement (a .&. b))

halt :: Runtime ()
halt = liftIO exitSuccess

allocation :: RegisterNum -> RegisterNum -> Runtime ()
allocation bRegNum cRegNum = do
  size  <- getRegister cRegNum
  newId <- allocateArray size

  setRegister bRegNum newId

abandonment :: RegisterNum -> Runtime ()
abandonment cRegNum = do
  arrId <- getRegister cRegNum

  freeArray arrId

output :: RegisterNum -> Runtime ()
output cRegNum = do
  c <- getRegister cRegNum
  liftIO . putChar . chr $ fromIntegral c

-- TODO: Add EOF support
input :: RegisterNum -> Runtime ()
input cRegNum = do
  c <- liftIO getChar
  setRegister cRegNum . fromIntegral $ ord c

loadProgram :: RegisterNum -> RegisterNum -> Runtime ()
loadProgram bRegNum cRegNum = do
  machine <- get
  arrId <- getRegister bRegNum
  ix    <- getRegister cRegNum

  jmp (ix - 1)
  loadZeroArray arrId

orthography :: RegisterNum -> MachineWord -> Runtime ()
orthography aRegNum val = setRegister aRegNum val

