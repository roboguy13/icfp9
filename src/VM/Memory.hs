module VM.Memory
  (Platter
  ,ArrayIx
  ,ArrayId
  ,Memory

  ,loadIntoMemory

  ,loadMemZeroArray

  ,getMemPlatter
  ,setMemPlatter

  ,allocateMemArray
  ,freeMemArray
  )
  where

import           Data.IntMap.Strict  (IntMap, (!))
import qualified Data.IntMap.Strict as I

import           Data.Word

import           Data.ByteString (ByteString)

import Debug.Trace

type Platter   = Word32
type ArrayId   = Word32
type ArrayIx   = Word32

data Memory = Memory
  { freeList  :: [Int]
  , arrayPool :: IntMap (IntMap Platter)
  }
  deriving Show

-- | Loads a program into 0th array.
loadIntoMemory :: [Platter] -> Memory
loadIntoMemory prog
  = Memory
    { freeList  = [1]
    , arrayPool = I.fromList [(0, loadPlatters prog)]
    }

getArray :: ArrayId -> Memory -> IntMap Platter
getArray arrId mem = arrayPool mem ! fromIntegral arrId

loadMemZeroArray :: ArrayId -> Memory -> Memory
loadMemZeroArray arrId mem
  = mem { arrayPool = I.insert 0 (getArray arrId mem) (arrayPool mem) }

loadPlatters :: [Platter] -> IntMap Platter
loadPlatters = I.fromList . zip [0..]


getMemPlatter :: ArrayId -> ArrayIx -> Memory -> Platter
getMemPlatter arrId ix mem
  = {-# SCC "getMemPlatter" #-} (arrayPool mem ! fromIntegral arrId) ! fromIntegral ix
{-# INLINE getMemPlatter #-}

setMemPlatter :: ArrayId -> ArrayIx -> Platter -> Memory -> Memory
setMemPlatter arrId ix newVal mem
  = mem
    { arrayPool = I.adjust (\arr -> I.insert (fromIntegral ix) newVal arr)
                           (fromIntegral arrId)
                           (arrayPool mem)
    }

allocateMemArray :: Word32 -> Memory -> (ArrayId, Memory)
allocateMemArray size mem
  = (newId, mem { freeList  = drop 1 (freeList mem)
                , arrayPool = I.insert (fromIntegral newId)
                                       (I.fromList . zip [0..fromIntegral size] $ repeat 0)
                                       (arrayPool mem)
                })
  where
    newId = case freeList mem of
      (arrId:_) -> fromIntegral arrId
      []        -> succ . fromIntegral . fst . I.findMax $ arrayPool mem

freeMemArray :: ArrayId -> Memory -> Memory
freeMemArray arrId mem
  = mem
    { freeList  = arrIdInt : freeList mem
    , arrayPool = I.delete arrIdInt (arrayPool mem)
    }
  where
    arrIdInt :: Int
    arrIdInt = fromIntegral arrId

