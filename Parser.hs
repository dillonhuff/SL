module Parser(parseExpr,
              parseFunc) where

import Data.List as L
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

import Syntax
import Token

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
  [[printOp]]

printOp = Prefix pPrintOp

pPrintOp = do
  pResNameTok "print"
  return $ printExpr

term = pStringLit

pStringLit = do
  position <- getPosition
  strL <- pStringLitTok
  return $ strLit $ getStringLitValue strL

pStringLitTok :: (Monad m) => ParsecT [Token] u m Token
pStringLitTok = slTok (\t -> tokenType t == STRINGLITERAL)

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
