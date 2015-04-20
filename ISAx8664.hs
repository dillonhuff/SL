module ISAx8664(Instruction,
                showInstrs,
                DataItem,
                call,
                leave,
                ret,
                enterII,
                movbIR,
                movqIR,
                movqRIO,
                movqIOR,
                movqRR,
                movqLOR,
                subqIR,
                subqRR,
                shrqIR,
                leaqLOR,
                js,
                jmp,
                labelInstr,
                Reg(..),
                stringConstant,
                getStringConstantName) where

import Data.List as L

data Instruction
  = Movq MVal MVal
  | Movb MVal MVal
  | Subq MVal MVal
  | Leaq MVal MVal
  | Shrq MVal MVal
  | Call String
  | Js String
  | Jmp String
  | LabelInstr String
  | Enter MVal MVal
  | Leave
  | Ret
    deriving (Eq, Ord)

instance Show Instruction where
  show (Enter l r) = "\tenter\t" ++ show l ++ ", " ++ show r
  show (Movb l r) = "\tmovb\t" ++ show l ++ ", " ++ show r
  show (Movq l r) = "\tmovq\t" ++ show l ++ ", " ++ show r
  show (Subq l r) = "\tsubq\t" ++ show l ++ ", " ++ show r
  show (Leaq l r) = "\tleaq\t" ++ show l ++ ", " ++ show r
  show (Shrq l r) = "\tshrq\t" ++ show l ++ ", " ++ show r
  show (Call str) = "\tcall\t" ++ str
  show (Js str) = "\tjs\t" ++ str
  show (Jmp str) = "\tjmp\t" ++ str
  show (LabelInstr str) = str ++ ":"
  show Leave = "\tleave\t"
  show Ret = "\tret\t"

leave = Leave
ret = Ret
call str = Call str
enterII il ir = Enter (Immediate il) (Immediate ir)
movbIR imm r = Movb (Immediate imm) (Register r)
movqIR imm r = Movq (Immediate imm) (Register r)
movqIOR imm l r = Movq (Deref (ImmediateOffset imm) l) (Register r)
movqRIO l imm r = Movq (Register l) (Deref (ImmediateOffset imm) r)
movqRR l r = Movq (Register l) (Register r)
movqLOR label l r = Movq (Deref (LabelOffset label) l) (Register r)
leaqLOR label l r = Leaq (Deref (LabelOffset label) l) (Register r)
subqIR imm r = Subq (Immediate imm) (Register r)
subqRR l r = Subq (Register l) (Register r)
shrqIR imm l = Shrq (Immediate imm) (Register l)
js = Js
jmp = Jmp
labelInstr s = LabelInstr s

showInstrs instrs = L.concat $ L.intersperse "\n" $ ("\t" : (L.map show instrs))

data MVal
  = Register Reg
  | Deref Offset Reg
  | Immediate Integer
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
  = ImmediateOffset Integer
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

stringConstant prefix i str = StringConst ("_" ++ prefix ++ "_str_const_" ++ show i) str

getStringConstantName (StringConst l _) = l
