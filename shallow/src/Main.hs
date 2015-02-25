import VM.VM
import VM.Type

import Control.Monad.State

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.IntMap as I

import Data.Binary.Strict.Get

import Control.Applicative

import Data.Monoid

import System.Environment

runProgram :: ByteString -> IO ()
runProgram = evalStateT (forever spinCycle) . loadMachine

loadMachine :: ByteString -> Machine
loadMachine program =
  Machine
    { registers = I.fromList . zip [0..7] $ repeat 0
    , zeroArray = I.fromList . zip [0..] $ platters program
    , arrays    = mempty
    , freeList  = [1]
    , ip        = 0
    }

platters :: ByteString -> [Platter]
platters b = case fst $ runGet go b of
                   Right ws -> ws
                   Left s -> error s
  where
    go :: Get [Platter]
    go = {-# SCC "platters.go" #-} do
      done <- isEmpty

      if done
        then return []
        else (:) <$> getWord32be <*> go

main :: IO ()
main = do
  [fileName] <- getArgs

  file <- B.readFile fileName

  runProgram file

