module ISAx8664(Instruction,
                showInstrs,
                DataItem,
                call,
                movqIR,
                subqIR,
                Reg(..)) where

import Data.List as L

data Instruction
  = Movq MVal MVal
  | Subq MVal MVal
  | Call String
    deriving (Eq, Ord, Show)

call str = Call str
movqIR imm r = Movq (Immediate imm) (Register r)
subqIR imm r = Subq (Immediate imm) (Register r)

showInstrs instrs = L.concat $ L.intersperse "\n" $ L.map show instrs

data MVal
  = Register Reg
  | Deref Offset Reg
  | Immediate Int
    deriving (Eq, Ord, Show)

data Reg
  = RAX
  | RDX
  | RSP
  | RBP
  | RDI
    deriving (Eq, Ord, Show)

data Offset
  = ImmediateOffSet Int
  | LabelOffSet String
    deriving (Eq, Ord, Show)

data DataItem
  = StringConst String
    deriving (Eq, Ord, Show)
