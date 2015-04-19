module Lexer(lexString) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as Tok

import Token

lexString :: String -> String -> Either String [Token]
lexString sourceFileName str = case parse (sepBy pTok spaces) sourceFileName str of
  Left err -> Left $ show err
  Right toks -> Right toks

languageDef =
  emptyDef { Tok.commentStart    = "/*",
             Tok.commentEnd      = "*/",
             Tok.commentLine     = "//",
             Tok.identStart      = lower,
             Tok.identLetter     = alphaNum,
             Tok.reservedNames   = [ "if", "is", "then", "else", "func", "end", "print"],
             Tok.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "||", "&&", "~"] }

lexer = Tok.makeTokenParser languageDef

pTok :: Parser Token
pTok = pVarOrRes
     <|> pDelim

pVarOrRes :: Parser Token
pVarOrRes = (try pIdentifier) <|> pResWord

pIdentifier = do
  pos <- getPosition
  first <- pVarName
  return $ ident pos first

pVarName = Tok.identifier lexer

pResWord :: Parser Token
pResWord = do
  pos <- getPosition
  resStr <- try (string "if")
         <|> string "is"
         <|> string "then"
         <|> try (string "else")
         <|> string "end"
         <|> string "func"
         <|> string "print"
  return $ res pos resStr

pDelim :: Parser Token
pDelim = do
  pos <- getPosition
  delimStr <- string "("
           <|> string ")"
           <|> string ","
  return $ delim pos delimStr
