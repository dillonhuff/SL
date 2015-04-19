module LexerTests(allLexerTests) where

import Data.List as L

import Lexer
import TestUtils
import Token

allLexerTests = do
  testFunction (lexString "nofile.sl") keywordCases
  testFunction (lexString "nofile.sl") identifierCases
  testFunction (lexString "nofile.sl") delimiterCases

keywordCases =
  L.map (\(x, y) -> (x, Right y))
  [("if", [dres "if"]),
   ("then", [dres "then"]),
   ("else", [dres "else"]),
   ("func", [dres "func"]),
   ("end", [dres "end"]),
   ("print", [dres "print"]),
   ("is", [dres "is"])]

identifierCases =
  L.map (\(x, y) -> (x, Right y))
  [("yes12", [dident "yes12"]),
   ("printFunc", [dident "printFunc"]),
   ("en", [dident "en"])]

delimiterCases =
  L.map (\(x, y) -> (x, Right y))
  [("(", [ddelim "("]),
   (")", [ddelim ")"]),
   (",", [ddelim ","])]
