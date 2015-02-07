module VM.VM
  (module Control.Monad.State
  ,Machine (..)
  ,VM
  )
  where

import VM.Memory
import Control.Monad.State

type Register = Platter

data Machine = Machine
  { memory    :: Memory
  , ip        :: Platter
  , r0        :: Register
  , r1        :: Register
  , r2        :: Register
  , r3        :: Register
  , r4        :: Register
  , r5        :: Register
  , r6        :: Register
  , r7        :: Register
  }

type VM = State Machine

incrIp :: VM ()
incrIp = modify (\m -> m { ip = ip m + 1 })

