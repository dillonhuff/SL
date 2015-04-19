module ISAx8664(Instruction,
                showInstrs,
                DataItem,
                call,
                movbIR,
                movqIR,
                movqRR,
                movqLOR,
                subqIR,
                leaqLOR,
                Reg(..),
                stringConstant,
                getStringConstantName) where

import Data.List as L

data Instruction
  = Movq MVal MVal
  | Movb MVal MVal
  | Subq MVal MVal
  | Leaq MVal MVal
  | Call String
    deriving (Eq, Ord)

instance Show Instruction where
  show (Movb l r) = "movb\t" ++ show l ++ ", " ++ show r
  show (Movq l r) = "movq\t" ++ show l ++ ", " ++ show r
  show (Subq l r) = "subq\t" ++ show l ++ ", " ++ show r
  show (Leaq l r) = "leaq\t" ++ show l ++ ", " ++ show r
  show (Call str) = "call\t" ++ str

call str = Call str
movbIR imm r = Movb (Immediate imm) (Register r)
movqIR imm r = Movq (Immediate imm) (Register r)
movqRR l r = Movq (Register l) (Register r)
movqLOR label l r = Movq (Deref (LabelOffset label) l) (Register r)
leaqLOR label l r = Leaq (Deref (LabelOffset label) l) (Register r)
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
  | RIP
  | RSP
  | RBP
  | RDI
  | AL
    deriving (Eq, Ord)

instance Show Reg where
  show RAX = "%rax"
  show RDX = "%rdx"
  show RSP = "%rsp"
  show RBP = "%rbp"
  show RDI = "%rdi"
  show RIP = "%rip"
  show AL = "%al"

data Offset
  = ImmediateOffset Int
  | LabelOffset String
    deriving (Eq, Ord)

instance Show Offset where
  show (ImmediateOffset i) = show i
  show (LabelOffset n) = n

data DataItem
  = StringConst String String
    deriving (Eq, Ord)

instance Show DataItem where
  show (StringConst l c) = l ++ ":\n\t.asciz " ++ show c ++ "\n"

stringConstant i str = StringConst ("_str_const_" ++ show i) str

getStringConstantName (StringConst l _) = l
