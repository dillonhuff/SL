module ParserTests(allParserTests) where

import Data.List as L

import Lexer
import Parser
import Syntax
import TestUtils

allParserTests = do
  testFunction lexAndParseExpr exprCases

lexAndParseExpr s = (lexString "noFile.sl" s) >>= (parseExpr "noFile.sl")

exprCases =
  L.map (\(x, y) -> (x, Right y))
  [("\"The quick brown fox jumped over the lazy dog.\"",
    strLit "The quick brown fox jumped over the lazy dog."),
   ("print \"hello\"",
    printExpr $ strLit "hello")]
