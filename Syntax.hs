module Syntax(Expr) where

data Program = Program [Decl]
               deriving (Eq, Ord, Show)

data Decl = Func String [FormalParam] Expr
            deriving (Eq, Ord, Show)

data FormalParam = FormalParam Type String
                   deriving (Eq, Ord, Show)

data Expr
  = Literal Lit
  | BOp Expr
  | ITE Expr Expr Expr
  | Name String
  | Funcall String [Expr]
  | Print Expr
    deriving (Eq, Ord, Show)

data Lit
  = StringLit String
  | IntLit Int
    deriving (Eq, Ord, Show)

data Type
  = IntT
  | StringT
    deriving (Eq, Ord, Show)
