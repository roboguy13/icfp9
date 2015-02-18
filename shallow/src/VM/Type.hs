module VM.Type
  where

import Data.IntMap.Strict (IntMap)
import Control.Monad.State

import Data.Word

type Instruction a = StateT Machine IO a

type Platter  = Word32
type Register = Platter

data Machine =
  Machine
    { registers :: IntMap Register
    , zeroArray :: IntMap Platter
    , arrays    :: IntMap (IntMap Platter)
    , ip        :: Platter
    }


