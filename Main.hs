module Main(main) where

import CodeGen
import Lexer
import Parser
import Syntax

main = do
  srcCode <- readFile srcFile
  compileModule srcFile srcCode

tinyMainDeclInt = func "main" [] $ intLit 12
tinyMainDeclStr = func "main" [] $ strLit "Hello, world!\n"
helloWorldProg = func "main" [] $ printExpr $ strLit "Hello, world!\n"
iteProg = func "main" [] $ iteExpr (intGEQ (-122) (-1000)) (printExpr $ strLit "If expression\n") (printExpr $ strLit "Else expression\n")

srcFile = "MultiFuncProgram.sl"
asmFile = "MultiFuncProgram.s"

lexAndParseModule fileName s = (lexString fileName s) >>= (parseModule fileName)

compileModule fileName srcCode =
  case lexAndParseModule fileName srcCode of
    Left err -> putStrLn err
    Right moduleAst -> writeFile asmFile (moduleAsm moduleAst)
