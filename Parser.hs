module Parser(parseExpr) where

import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

import Syntax
import Token

parseExpr :: String -> [Token] -> Either String Expr
parseExpr srcFileName toks =
  case parse pExpr srcFileName toks of
    Left err -> Left $ show err
    Right expr -> Right expr

pExpr = buildExpressionParser table term

table =
  [[printOp]]

printOp = Prefix pPrintOp

pPrintOp = do
  resNameTok "print"
  return $ printExpr

term = pStringLitTok

pStringLitTok = do
  position <- getPosition
  strL <- stringLitTok
  return $ strLit $ getStringLitValue strL

stringLitTok :: (Monad m) => ParsecT [Token] u m Token
stringLitTok = slTok (\t -> tokenType t == STRINGLITERAL)

resNameTok :: (Monad m) => String -> ParsecT [Token] u m Token
resNameTok name = slTok (\t -> (tokenType t == RESERVED) && (reservedTokName t == name))

slTok :: (Monad m) => (Token -> Bool) -> ParsecT [Token] u m Token
slTok condition = tokenPrim show updatePos meetsCond
  where
    meetsCond t = if condition t then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position
