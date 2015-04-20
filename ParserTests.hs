module ParserTests(allParserTests) where

import Data.List as L

import Lexer
import Parser
import Syntax
import TestUtils

allParserTests = do
  testFunction lexAndParseExpr exprCases
  testFunction lexAndParseFunc funcCases

lexAndParseExpr s = (lexString "noFile.sl" s) >>= (parseExpr "noFile.sl")
lexAndParseFunc s = (lexString "noFile.sl" s) >>= (parseFunc "noFile.sl")

exprCases =
  L.map (\(x, y) -> (x, Right y))
  [("\"The quick brown fox jumped over the lazy dog.\"",
    strLit "The quick brown fox jumped over the lazy dog."),
   ("print \"hello\"",
    printExpr $ strLit "hello"),
   ("if 123 >= 15 then print \"Worked out man\" else print \"!(123 >= 15) ?\"",
    iteExpr (intGEQ 123 15) (printExpr $ strLit "Worked out man") (printExpr $ strLit "!(123 >= 15) ?")),
   ("a", nameExpr "a"),
   ("(f a b)", funcallExpr "f" [nameExpr "a", nameExpr "b"])]

funcCases =
  L.map (\(x, y) -> (x, Right y))
  [("func main is print \"this is a test\" end",
    func "main" [] $ printExpr $ strLit "this is a test")]
