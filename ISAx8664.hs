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
    deriving (Eq, Ord)

instance Show Instruction where
  show (Movq l r) = "movq\t" ++ show l ++ ", " ++ show r
  show (Subq l r) = "subq\t" ++ show l ++ ", " ++ show r
  show (Call str) = "call\t" ++ str

call str = Call str
movqIR imm r = Movq (Immediate imm) (Register r)
subqIR imm r = Subq (Immediate imm) (Register r)

showInstrs instrs = L.concat $ L.intersperse "\n\t" $ ("\t" : (L.map show instrs))

data MVal
  = Register Reg
  | Deref Offset Reg
  | Immediate Int
    deriving (Eq, Ord)

instance Show MVal where
  show (Register r) = show r
  show (Deref off r) = show off ++ "(" ++ show r ++ ")"
  show (Immediate i) = "$" ++ show i

data Reg
  = RAX
  | RDX
  | RSP
  | RBP
  | RDI
    deriving (Eq, Ord)

instance Show Reg where
  show RAX = "%rax"
  show RDX = "%rdx"
  show RSP = "%rsp"
  show RBP = "%rbp"
  show RDI = "%rdi"

data Offset
  = ImmediateOffSet Int
  | LabelOffSet String
    deriving (Eq, Ord, Show)

data DataItem
  = StringConst String
    deriving (Eq, Ord, Show)
