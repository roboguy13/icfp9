module Instruction.Instruction
  (MachineWord
  ,RegisterNum
  ,RegisterNumTriplet
  ,aRegNum, bRegNum, cRegNum
  ,regNumTriplet
  ,Instruction (..)
  )
  where

import Data.Word

import Numeric (showHex)

type MachineWord        = Word32
type RegisterNum        = MachineWord
data RegisterNumTriplet = RegisterNumTriplet RegisterNum RegisterNum RegisterNum
  deriving Show

aRegNum, bRegNum, cRegNum :: RegisterNumTriplet -> RegisterNum
aRegNum (RegisterNumTriplet a _ _) = a
bRegNum (RegisterNumTriplet _ b _) = b
cRegNum (RegisterNumTriplet _ _ c) = c

regNumTriplet :: RegisterNum -> RegisterNum -> RegisterNum -> RegisterNumTriplet
regNumTriplet = RegisterNumTriplet

data Instruction =
  -- *** Standard Operators ***
    CondMove       RegisterNumTriplet

  | ArrayIndex     RegisterNumTriplet
  | ArrayAmendment RegisterNumTriplet

  | Addition       RegisterNumTriplet
  | Multiplication RegisterNumTriplet
  | Division       RegisterNumTriplet
  | NotAnd         RegisterNumTriplet

  -- *** Other Operators ***
  | Halt

  | Allocation     RegisterNum RegisterNum
  | Abandonment    RegisterNum
  | Output         RegisterNum
  | Input          RegisterNum
  | LoadProgram    RegisterNum RegisterNum

  -- *** Special Operators ***
  | Orthography    RegisterNum MachineWord

  | UndefinedInstruction Word8 MachineWord

show0x :: (Show a, Integral a) => a -> String
show0x n = "0x" ++ showHex n ""

showTriplet :: RegisterNumTriplet -> String
showTriplet (RegisterNumTriplet a b c)
  = unwords $ [show0x a
              ,show0x b
              ,show0x c
              ]

instance Show Instruction where
  show (CondMove       triplet) = "condmov   " ++ showTriplet triplet
  show (ArrayIndex     triplet) = "index     " ++ showTriplet triplet
  show (ArrayAmendment triplet) = "amendment " ++ showTriplet triplet
  show (Addition       triplet) = "add       " ++ showTriplet triplet
  show (Multiplication triplet) = "mul       " ++ showTriplet triplet
  show (Division       triplet) = "div       " ++ showTriplet triplet
  show (NotAnd         triplet) = "nand      " ++ showTriplet triplet

  show  Halt                    = "halt"

  show (Allocation  a b       ) = "alloc     " ++ show0x a ++ " " ++ show0x b
  show (Abandonment a         ) = "abandon   " ++ show0x a
  show (Output      a         ) = "output    " ++ show0x a
  show (Input       a         ) = "input     " ++ show0x a
  show (LoadProgram a b       ) = "load      " ++ show0x a ++ " " ++ show0x b
  show (Orthography a val     ) = "ortho     " ++ show0x a ++ " " ++ show0x val
  show (UndefinedInstruction instr word) = "undefined " ++ show0x instr ++ " " ++ show0x word

-- TODO: Test
instance Read Instruction where
  readsPrec _ = go . words
    where
      tripletInstr :: (RegisterNumTriplet -> Instruction)
                      -> String
                      -> String
                      -> String
                      -> [String]
                      -> [(Instruction, String)]
      tripletInstr instr a b c rest = [(instr (regNumTriplet (read a) (read b) (read c)), unwords rest)]

      go :: [String] -> [(Instruction, String)]
      go ("condmov"  :a:b:c:rest) = tripletInstr CondMove       a b c rest
      go ("index"    :a:b:c:rest) = tripletInstr ArrayIndex     a b c rest
      go ("amendment":a:b:c:rest) = tripletInstr ArrayAmendment a b c rest
      go ("add"      :a:b:c:rest) = tripletInstr Addition       a b c rest
      go ("sub"      :a:b:c:rest) = tripletInstr Division       a b c rest
      go ("mul"      :a:b:c:rest) = tripletInstr Multiplication a b c rest
      go ("div"      :a:b:c:rest) = tripletInstr Division       a b c rest
      go ("nand"     :a:b:c:rest) = tripletInstr NotAnd         a b c rest

      go ("halt"           :rest) = [(Halt, unwords rest)]

      go ("alloc"    :a:b  :rest) = [(Allocation           (read a) (read b), unwords rest)]
      go ("abandon"  :a    :rest) = [(Abandonment          (read a),          unwords rest)]
      go ("output"   :a    :rest) = [(Output               (read a),          unwords rest)]
      go ("input"    :a    :rest) = [(Input                (read a),          unwords rest)]
      go ("load"     :a:b  :rest) = [(LoadProgram          (read a) (read b), unwords rest)]
      go ("ortho"    :a:v  :rest) = [(Orthography          (read a) (read v), unwords rest)]
      go ("undefined":a:b  :rest) = [(UndefinedInstruction (read a) (read b), unwords rest)]

  readList = map read . lines

