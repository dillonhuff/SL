module LexerTests(allLexerTests) where

import Data.List as L

import Lexer
import TestUtils
import Token

allLexerTests = do
  testFunction (lexString "nofile.sl") keywordCases

keywordCases =
  L.map (\(x, y) -> (x, Right y))
  [("if", [dres "if"]),
   ("then", [dres "then"]),
   ("else", [dres "else"]),
   ("func", [dres "func"]),
   ("end", [dres "end"]),
   ("print", [dres "print"])]
