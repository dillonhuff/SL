module Lexer(lexString) where

import Text.ParserCombinators.Parsec

import Token

lexString :: String -> String -> Either String [Token]
lexString sourceFileName str = case parse (sepBy pTok spaces) sourceFileName str of
  Left err -> Left $ show err
  Right toks -> Right toks

pTok :: Parser Token
pTok = pVarOrRes

pVarOrRes :: Parser Token
pVarOrRes = pResWord

pResWord :: Parser Token
pResWord = do
  pos <- getPosition
  resStr <- string "if"
         <|> string "then"
         <|> try (string "else")
         <|> string "end"
         <|> string "func"
         <|> string "print"
  return $ res pos resStr
