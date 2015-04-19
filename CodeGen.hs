module CodeGen(runGenDeclCode,
               prettyPrintCode) where

import Data.List as L
import Control.Applicative
import Control.Monad.State

import ISAx8664
import Syntax

runGenDeclCode :: Decl -> CodeGenState
runGenDeclCode decl =
  let bodyCode = execState (genExprCode $ getFuncBody decl) (startingCodeGenState decl) in
  appendPrelude decl $ appendConclusion decl bodyCode

appendPrelude decl cgs =
  case isMain decl of
    True -> addInstrToStart (subqIR (adjustTo16ByteAligned $ stackSize cgs) RSP) cgs
    False -> error "append prelude not implemented for non-main functions"

adjustTo16ByteAligned :: Int -> Int
adjustTo16ByteAligned stkSize =
  case (mod stkSize 16) == 8 of
    True -> stkSize
    False -> stkSize + 8

appendConclusion decl cgs =
  case isMain decl of
    True -> addInstr (call "_exit") $ addInstr (movqIR 0 RDI) cgs
    False -> error "addConclusion not yet implemented for non-main functions"

data CodeGenState
  = CodeGenState {
    funcName :: String,
    stackSize :: Int,
    nextInt :: Int,
    instructions :: [Instruction],
    dataItems :: [DataItem]
    } deriving (Eq, Ord, Show)

addInstr :: Instruction -> CodeGenState -> CodeGenState
addInstr ir (CodeGenState n ss v is ds) =
  CodeGenState n ss v (ir:is) ds

addInstrToStart :: Instruction -> CodeGenState -> CodeGenState
addInstrToStart ir (CodeGenState n ss v is ds) =
  CodeGenState n ss v (is ++ [ir]) ds

addStringConstant :: String -> CodeGenState -> (String, CodeGenState)
addStringConstant s (CodeGenState n ss v is ds) =
  (getStringConstantName newStrConst, CodeGenState n ss (v+1) is (newStrConst:ds))
  where
    newStrConst = stringConstant v s

nextLabel :: CodeGenState -> (String, CodeGenState)
nextLabel (CodeGenState n ss v is ds) =
  ("label_" ++ show v, CodeGenState n ss (v+1) is ds)

nextSP :: CodeGenState -> (Int, CodeGenState)
nextSP (CodeGenState n ss v is ds) =
  (ss, CodeGenState n (ss+8) v is ds)

instr :: Instruction -> State CodeGenState ()
instr ir = do
  st <- get
  let resSt = addInstr ir st in
    put resSt

freshStringConstant :: String -> State CodeGenState String
freshStringConstant str = do
  st <- get
  let (strConstName, resSt) = addStringConstant str st in
    do
      put resSt
      return strConstName

freshLabel :: State CodeGenState String
freshLabel = do
  st <- get
  let (newLabel, resSt) = nextLabel st in
    do
      put resSt
      return newLabel

freshStackLoc :: State CodeGenState Int
freshStackLoc = do
  st <- get
  let (newStackLoc, resSt) = nextSP st in
    do
      put resSt
      return $ (-1) * newStackLoc

prettyPrintCode :: CodeGenState -> String
prettyPrintCode (CodeGenState n ss v instrs dataSegItems) =
  ".text\n.globl _" ++ n ++ "\n_" ++ n ++ ":\n" ++ (showInstrs $ reverse instrs)  ++ "\n\n.data\n" ++ (L.concat $ L.intersperse "\n" $ L.map show $ dataSegItems) ++ "\n"

startingCodeGenState :: Decl -> CodeGenState
startingCodeGenState decl = CodeGenState (getFuncName decl) 8 0 [] []

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
    STRINGLIT -> genStringLitCode $ getString l

genIntLitCode :: Int -> State CodeGenState ()
genIntLitCode i = instr $ movqIR i RAX

genStringLitCode :: String -> State CodeGenState ()
genStringLitCode s = do
  l <- freshStringConstant s
  instr $ leaqLOR l RIP RAX

genBinopCode b l r = do
  genExprCode l
  lResSaveLoc <- genRegSaveCode RAX
  genExprCode r
  instr $ movqIOR lResSaveLoc RBP RDX
  genBOpCode b RDX RAX

genRegSaveCode r = do
  nextStackLoc <- freshStackLoc
  instr $ movqRIO r nextStackLoc RBP
  return nextStackLoc

genBOpCode b l r =
  case binopType b of
    INTGEQ -> do
      instr $ subqRR l r
      instr $ shrqIR 63 r

genITECode t i e = do
  l1 <- genBoolTestCode t
  genExprCode i
  l2 <- freshLabel
  instr $ jmp l2
  instr $ labelInstr l1
  genExprCode e
  instr $ labelInstr l2

genBoolTestCode e = do
  genExprCode e
  l1 <- freshLabel
  instr $ subqIR 1 RAX
  instr $ js l1
  return l1

genNameCode n = error "genNameCode not implemented"

genFuncallCode n args = error "genFuncallCode not implemented"

genPrintCode e = do
  genExprCode e
  instr $ movqRR RAX RDI
  instr $ movbIR 0 AL
  instr $ call "_printf"
  instr $ movqIR 0 RAX
