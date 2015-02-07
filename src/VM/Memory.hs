{-# LANGUAGE BangPatterns #-}
module VM.Memory
  where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.IntMap.Strict  (IntMap, delete)
import qualified Data.IntMap.Strict as I
import Data.Word

type Platter = Word32
type Page    = Vector Platter

data Memory = Memory
  { freePages :: [Int]
  , pagePool  :: IntMap Page
  , lastAlloc :: !Int
  }

freePage :: Int -> Memory -> Memory
freePage ix memory =
  memory
    { freePages = ix : freePages memory
    , pagePool  = delete ix (pagePool memory)
    }

allocPage :: Memory -> (Int, Memory)
allocPage memory =
  let newIx = lastAlloc memory + 1
  in
  (newIx
  ,memory { lastAlloc = newIx }
  )

memIx :: Int -> Int -> Memory -> Platter
memIx i j memory = (pagePool memory I.! j) V.! i
{-# INLINE memIx #-}

setPlatter :: Int -> Int -> Platter -> Memory -> Memory
setPlatter i j platter memory = undefined
{-# INLINE setPlatter #-}

