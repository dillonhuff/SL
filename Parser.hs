module Parser(parseExpr,
              parseFunc,
              parseModule) where

import Data.List as L
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

import Syntax
import Token

parseModule :: String -> [Token] -> Either String Module
parseModule srcFileName toks =
  case parse pModule srcFileName toks of
    Left err -> Left $ show err
    Right mod -> Right mod

parseFunc :: String -> [Token] -> Either String Decl
parseFunc srcFileName toks =
  case parse pFunc srcFileName toks of
    Left err -> Left $ show err
    Right expr -> Right expr

parseExpr :: String -> [Token] -> Either String Expr
parseExpr srcFileName toks =
  case parse pExpr srcFileName toks of
    Left err -> Left $ show err
    Right expr -> Right expr

pModule = do
  decls <- many pFunc
  return $ slModule decls

pFunc = do
  pResNameTok "func"
  fName <- pIdentTok
  args <- many pIdentTok
  pResNameTok "is"
  body <- pExpr
  pResNameTok "end"
  return $ func (identTokName fName) (L.map identTokName args) body

pExpr = buildExpressionParser table term

table =
  [[printOp],
   [intGEQOp]]

printOp = Prefix pPrintOp
intGEQOp = Infix pIntGEQ AssocLeft

pIntGEQ = do
  pResNameTok ">="
  return $ binopExpr iGEQ

pPrintOp = do
  pResNameTok "print"
  return $ printExpr

term = try (pParens pExpr)
     <|> pParens pFuncall
     <|> pStringLit
     <|> pIntLit
     <|> pITEExpr
     <|> pIdent

pParens otherParser = do
  pResNameTok "("
  other <- otherParser
  pResNameTok ")"
  return other

pFuncall = do
  position <- getPosition
  fName <- pIdentTok
  args <- many pExpr
  return $ funcallExpr (identTokName fName) args

pIdent = do
  position <- getPosition
  idT <- pIdentTok
  return $ nameExpr $ identTokName idT

pStringLit = do
  position <- getPosition
  strL <- pStringLitTok
  return $ strLit $ getStringLitValue strL

pIntLit = do
  position <- getPosition
  intL <- pIntLitTok
  return $ Syntax.intLit $ getIntLitValue intL

pITEExpr = do
  pResNameTok "if"
  testE <- pExpr
  pResNameTok "then"
  ifE <- pExpr
  pResNameTok "else"
  elseE <- pExpr
  return $ iteExpr testE ifE elseE

pStringLitTok :: (Monad m) => ParsecT [Token] u m Token
pStringLitTok = slTok (\t -> tokenType t == STRINGLITERAL)

pIntLitTok :: (Monad m) => ParsecT [Token] u m Token
pIntLitTok = slTok (\t -> tokenType t == INTEGERLITERAL)

pIdentTok :: (Monad m) => ParsecT [Token] u m Token
pIdentTok = slTok (\t -> tokenType t == IDENTIFIER)

pResNameTok :: (Monad m) => String -> ParsecT [Token] u m Token
pResNameTok name = slTok (\t -> (tokenType t == RESERVED) && (reservedTokName t == name))

slTok :: (Monad m) => (Token -> Bool) -> ParsecT [Token] u m Token
slTok condition = tokenPrim show updatePos meetsCond
  where
    meetsCond t = if condition t then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position
