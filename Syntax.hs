module Syntax(Program,
              getDecls,
              Decl,
              isMain,
              getFuncBody,
              Expr,
              ExprType(..),
              Lit,
              exprType,
              getFunctionName,
              getFunctionArgs,
              getLiteral,
              getBinop,
              getLeftOperand,
              getRightOperand,
              getTest,
              getIfExpr,
              getElseExpr,
              getName,
              getPrintExpr) where

data Program = Program [Decl]
               deriving (Eq, Ord, Show)

getDecls (Program ds) = ds

data Decl = Func String [FormalParam] Expr
            deriving (Eq, Ord, Show)

isMain (Func "main" _ _) = True
isMain _ = False

getFuncBody (Func _ _ b) = b

data FormalParam = FormalParam Type String
                   deriving (Eq, Ord, Show)

data Expr
  = Literal Lit
  | BOp Binop Expr Expr
  | Ite Expr Expr Expr
  | Name String
  | Funcall String [Expr]
  | Print Expr
    deriving (Eq, Ord, Show)

exprType (Literal _) = LITERAL
exprType (BOp _ _ _) = BINOP

getFunctionName (Funcall n _) = n
getFunctionArgs (Funcall _ a) = a
getLiteral (Literal l) = l
getBinop (BOp b _ _) = b
getLeftOperand (BOp _ l _) = l
getRightOperand (BOp _ _ r) = r
getTest (Ite t _ _) = t
getIfExpr (Ite _ i _) = i
getElseExpr (Ite _ _ e) = e
getName (Name n) = n
getPrintExpr (Print e) = e

data ExprType
  = LITERAL
  | BINOP
  | ITE
  | NAME
  | FUNCALL
  | PRINT
    deriving (Eq, Ord, Show)

data Lit
  = StringLit String
  | IntLit Int
    deriving (Eq, Ord, Show)

data Binop
  = IntPlus
  | IntMinus
  | IntEquals
    deriving (Eq, Ord, Show)

data Type
  = IntT
  | StringT
    deriving (Eq, Ord, Show)