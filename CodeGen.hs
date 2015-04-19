module CodeGen(runGenDeclCode,
               prettyPrintCode) where

import Data.List as L
import Control.Applicative
import Control.Monad.State

import ISAx8664
import Syntax

{-genProgramCode program =
  L.concat $ L.intersperse "\n" $ L.map genDeclCode $ getDecls program

genDeclCode decl =
  case isMain decl of
    True -> genMainDeclCode decl
    False -> error $ "non main decl code not implemented"

genMainDeclCode decl =
  (mainPrelude stkSize) ++ instrs ++ mainConclusion ++ dataSeg
  where
    codeGenS = execState (genExprCode $ getFuncBody decl) (startingCodeGenState decl)
    instrs = showInstrs $ instructions codeGenS
    dataSeg = ".data\n" ++ (L.concat $ L.intersperse "\n" $ L.map show $ dataItems codeGenS)
    stkSize = stackSize codeGenS-}

mainPrelude stkSize = ".text\n.globl _main\n" ++ showInstrs [subqIR stkSize RSP]

mainConclusion = showInstrs [movqIR 0 RDI, call "_exit"]

runGenDeclCode :: Decl -> CodeGenState
runGenDeclCode decl =
  let bodyCode = execState (genExprCode $ getFuncBody decl) (startingCodeGenState decl) in
  appendPrelude decl $ appendConclusion decl bodyCode

appendPrelude decl cgs =
  case isMain decl of
    True -> addInstrToStart (subqIR (stackSize cgs) RSP) cgs
    False -> error "append prelude not implemented for non-main functions"

appendConclusion decl cgs =
  case isMain decl of
    True -> addInstr (call "_exit") $ addInstr (movqIR 0 RDI) cgs
    False -> error "addConclusion not yet implemented for non-main functions"

data CodeGenState
  = CodeGenState {
    funcName :: String,
    stackSize :: Int,
    instructions :: [Instruction],
    dataItems :: [DataItem]
    } deriving (Eq, Ord, Show)

addInstr :: Instruction -> CodeGenState -> CodeGenState
addInstr ir (CodeGenState n ss is ds) =
  CodeGenState n ss (ir:is) ds

addInstrToStart :: Instruction -> CodeGenState -> CodeGenState
addInstrToStart ir (CodeGenState n ss is ds) =
  CodeGenState n ss (is ++ [ir]) ds

instr :: Instruction -> State CodeGenState ()
instr ir = do
  st <- get
  let resSt = addInstr ir st in
    put resSt

prettyPrintCode :: CodeGenState -> String
prettyPrintCode (CodeGenState n ss instrs dataSegItems) =
  ".text\n.globl _" ++ n ++ "\n_" ++ n ++ ":\n" ++ (showInstrs $ reverse instrs)  ++ "\n\n.data\n" ++ (L.concat $ L.intersperse "\n" $ L.map show $ dataSegItems)

startingCodeGenState :: Decl -> CodeGenState
startingCodeGenState decl = CodeGenState (getFuncName decl) 8 [] []

genExprCode :: Expr -> State CodeGenState ()
genExprCode e =
  case exprType e of
    LITERAL -> genLiteralCode $ getLiteral e
    BINOP -> genBinopCode (getBinop e) (getLeftOperand e) (getRightOperand e)
    ITE -> genITECode (getTest e) (getIfExpr e) (getElseExpr e)
    NAME -> genNameCode $ getName e
    FUNCALL -> genFuncallCode (getFunctionName e) (getFunctionArgs e)
    PRINT -> genPrintCode $ getPrintExpr e

genLiteralCode :: Lit -> State CodeGenState ()
genLiteralCode l =
  case literalType l of
    INTLIT -> genIntLitCode $ getInt l
    STRINGLIT -> error "stringLit not implemented"

genIntLitCode :: Int -> State CodeGenState ()
genIntLitCode i = instr $ movqIR i RAX

genBinopCode b l r = error "genBinopCode not implemented"

genITECode t i e = error "genITECode not implemented"

genNameCode n = error "genNameCode not implemented"

genFuncallCode n args = error "genFuncallCode not implemented"

genPrintCode e = error "genPrintCode not implemented"
