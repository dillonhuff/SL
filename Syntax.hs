module Syntax(Program,
              getDecls,
              Decl,
              func,
              isMain,
              getFuncBody,
              getFuncName,
              Expr,
              printExpr,
              intGEQ,
              iteExpr,
              ExprType(..),
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
              getPrintExpr,
              Lit,
              LitType(..),
              literalType,
              strLit,
              intLit,
              getInt,
              getString,
              Binop,
              binopType,
              BinopType(..)) where

data Program = Program [Decl]
               deriving (Eq, Ord, Show)

getDecls (Program ds) = ds

data Decl = Func String [FormalParam] Expr
            deriving (Eq, Ord, Show)

func n ps expr = Func n ps expr

isMain (Func "main" _ _) = True
isMain _ = False

getFuncName (Func n _ _) = n
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
exprType (Print _) = PRINT
exprType (Ite _ _ _) = ITE

iteExpr t i e = Ite t i e
intGEQ l r = BOp IntGEQ (intLit l) (intLit r)
strLit s = Literal $ StringLit s
intLit i = Literal $ IntLit i
printExpr e = Print e

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

literalType (StringLit _) = STRINGLIT
literalType (IntLit _) = INTLIT

getInt (IntLit i) = i
getString (StringLit s) = s

data LitType
  = STRINGLIT
  | INTLIT
    deriving (Eq, Ord, Show)

data Binop
  = IntPlus
  | IntMinus
  | IntEq
  | IntGEQ
    deriving (Eq, Ord, Show)

binopType IntGEQ = INTGEQ

data BinopType
  = INTGEQ
    deriving (Eq, Ord, Show)

data Type
  = IntT
  | StringT
    deriving (Eq, Ord, Show)
