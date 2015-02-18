module Main
  where

import Instruction.Instruction
import Instruction.Read

import qualified Data.ByteString as BS

import           System.Environment


main :: IO ()
main = do
  [programName] <- getArgs

  program <- BS.readFile programName

  mapM_ print . map readInstruction $ programWords program

