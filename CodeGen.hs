module CodeGen(runGenDeclCode,
               moduleAsm,
               prettyPrintCode) where

import Data.Foldable hiding (mapM_)
import Data.List as L
import Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (mapM_)

import ISAx8664
import Syntax

moduleAsm :: Module -> String
moduleAsm mod =
  L.concatMap (prettyPrintCode . runGenDeclCode) $ getDecls mod

runGenDeclCode :: Decl -> CodeGenState
runGenDeclCode decl =
  let bodyCode = execState (genExprCode $ getFuncBody decl) (startingCodeGenState decl) in
  appendPrelude decl $ appendConclusion decl bodyCode

appendPrelude decl cgs =
  case isMain decl of
    True -> addInstrToStart (subqIR (adjustTo16ByteAligned $ stackSize cgs) RSP) cgs
    False -> stdFuncPrelude decl cgs

stdFuncPrelude decl cgs =
  addInstrToStart (enterII 0 0) $ addInstrToStart (subqIR (stackSize cgs + (mod (stackSize cgs) 16)) RSP) cgs

adjustTo16ByteAligned :: Integer -> Integer
adjustTo16ByteAligned stkSize =
  case (mod stkSize 16) == 8 of
    True -> stkSize
    False -> stkSize + 8

appendConclusion decl cgs =
  case isMain decl of
    True -> addInstr (call "_exit") $ addInstr (movqIR 0 RDI) cgs
    False -> addInstr ret $ addInstr leave cgs

data CodeGenState
  = CodeGenState {
    funcName :: String,
    stackSize :: Integer,
    nextInt :: Integer,
    instructions :: [Instruction],
    dataItems :: [DataItem],
    stackOffsets :: Map String Integer
    } deriving (Eq, Ord, Show)

stackOffset :: String -> CodeGenState -> Integer
stackOffset n cgs@(CodeGenState _ _ _ _ _ so) =
  case M.lookup n so of
    Just offset -> offset
    Nothing -> error $ "variable " ++ show n ++ " does not exist in CodeGenState " ++ show cgs

getStackOffset :: String -> State CodeGenState Integer
getStackOffset n = do
  st <- get
  return $ stackOffset n st

addInstr :: Instruction -> CodeGenState -> CodeGenState
addInstr ir (CodeGenState n ss v is ds so) =
  CodeGenState n ss v (ir:is) ds so

addInstrToStart :: Instruction -> CodeGenState -> CodeGenState
addInstrToStart ir (CodeGenState n ss v is ds so) =
  CodeGenState n ss v (is ++ [ir]) ds so

addStringConstant :: String -> CodeGenState -> (String, CodeGenState)
addStringConstant s (CodeGenState n ss v is ds so) =
  (getStringConstantName newStrConst, CodeGenState n ss (v+1) is (newStrConst:ds) so)
  where
    newStrConst = stringConstant n v s

nextLabel :: CodeGenState -> (String, CodeGenState)
nextLabel (CodeGenState n ss v is ds so) =
  (n ++ "_label_" ++ show v, CodeGenState n ss (v+1) is ds so)

nextSP :: CodeGenState -> (Integer, CodeGenState)
nextSP (CodeGenState n ss v is ds so) =
  (ss, CodeGenState n (ss+8) v is ds so)

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

freshStackLoc :: State CodeGenState Integer
freshStackLoc = do
  st <- get
  let (newStackLoc, resSt) = nextSP st in
    do
      put resSt
      return $ (-1) * newStackLoc

prettyPrintCode :: CodeGenState -> String
prettyPrintCode (CodeGenState n ss v instrs dataSegItems so) =
  ".text\n.globl _" ++ n ++ "\n_" ++ n ++ ":" ++ (showInstrs $ reverse instrs)  ++ "\n\n.data\n" ++ (L.concat $ L.intersperse "\n" $ L.map show $ dataSegItems) ++ "\n"

startingCodeGenState :: Decl -> CodeGenState
startingCodeGenState decl = CodeGenState (getFuncName decl) 8 0 [] [] (argumentStackOffsets decl)

argumentStackOffsets :: Decl -> Map String Integer
argumentStackOffsets decl =
  let args = getFuncArgs decl in
  M.fromList $ L.zip args $ L.map (\x -> 8*x + 16) [0..((toInteger $ length args) - 1)]

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

genIntLitCode :: Integer -> State CodeGenState ()
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

genNameCode n = do
  off <- getStackOffset n
  instr $ movqIOR off RBP RAX

computeArgRes :: Expr -> [Integer] -> State CodeGenState [Integer]
computeArgRes e offs = do
  genExprCode e
  resStoreLoc <- freshStackLoc
  instr $ movqRIO RAX resStoreLoc RSP
  return $ resStoreLoc : offs

genPushArgCode :: Integer -> State CodeGenState ()
genPushArgCode off = do
  instr $ movqIOR off RSP RAX
  l <- freshStackLoc
  instr $ pushq RAX

genFuncallCode n args = do
  argResOffsets <- foldrM computeArgRes [] args
  mapM_ genPushArgCode $ L.reverse argResOffsets
  instr $ call $ "_" ++ n

genPrintCode e = do
  genExprCode e
  instr $ movqRR RAX RDI
  instr $ movbIR 0 AL
  instr $ call "_printf"
  instr $ movqIR 0 RAX
