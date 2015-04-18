module CodeGen() where

import Data.List as L

import ISAx8664
import Syntax

genProgramCode program =
  L.concat $ L.intersperse "\n" $ L.map genDeclCode $ getDecls program

genDeclCode decl =
  case isMain decl of
    True -> genMainDeclCode decl
    False -> error $ "non main decl code not implemented"

genMainDeclCode decl =
  (mainPrelude stkSize) ++ instrs ++ mainConclusion ++ dataSeg
  where
    codeGenS = genExprCode $ getFuncBody decl
    instrs = L.concat $ L.intersperse "\n" $ L.map show $ instructions codeGenS
    dataSeg = L.concat $ L.intersperse "\n" $ L.map show $ dataItems codeGenS
    stkSize = stackSize codeGenS

mainPrelude stkSize = showInstrs [subqIR stkSize RSP]

mainConclusion = showInstrs [movqIR 0 RDI, call "_exit"]
    
data CodeGenState a
  = CodeGenState {
    stackSize :: Int,
    instructions :: [Instruction],
    dataItems :: [DataItem],
    annotation :: a
    } deriving (Eq, Ord, Show)

genExprCode :: Expr -> CodeGenState String
genExprCode e =
  case exprType e of
    LITERAL -> genLiteralCode $ getLiteral e
    BINOP -> genBinopCode (getBinop e) (getLeftOperand e) (getRightOperand e)
    ITE -> genITECode (getTest e) (getIfExpr e) (getElseExpr e)
    NAME -> genNameCode $ getName e
    FUNCALL -> genFuncallCode (getFunctionName e) (getFunctionArgs e)
    PRINT -> genPrintCode $ getPrintExpr e
    
genLiteralCode :: Lit -> CodeGenState String
genLiteralCode l = error "genLiteralCode not implemented"

genBinopCode b l r = error "genBinopCode not implemented"

genITECode t i e = error "genITECode not implemented"

genNameCode n = error "genNameCode not implemented"

genFuncallCode n args = error "genFuncallCode not implemented"

genPrintCode e = error "genPrintCode not implemented"
