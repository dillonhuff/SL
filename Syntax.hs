module Syntax(Module,
              slModule,
              getDecls,
              Decl,
              func,
              isMain,
              getFuncArgs,
              getFuncBody,
              getFuncName,
              Expr,
              printExpr,
              nameExpr,
              intGEQ,
              iteExpr,
              binopExpr,
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
              iGEQ,
              BinopType(..)) where

data Module = Module [Decl]
               deriving (Eq, Ord, Show)

slModule ds = Module ds

getDecls (Module ds) = ds

data Decl = Func String [String] Expr
            deriving (Eq, Ord, Show)

func n ps expr = Func n ps expr

isMain (Func "main" _ _) = True
isMain _ = False

getFuncName (Func n _ _) = n
getFuncBody (Func _ _ b) = b
getFuncArgs (Func _ args _) = args

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
exprType (Name _) = NAME

nameExpr n = Name n
binopExpr op l r = BOp op l r
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
  | IntLit Integer
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

iGEQ = IntGEQ

data BinopType
  = INTGEQ
    deriving (Eq, Ord, Show)

data Type
  = IntT
  | StringT
    deriving (Eq, Ord, Show)
