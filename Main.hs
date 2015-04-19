module Main(main) where

import CodeGen
import Syntax

main =
  let asmCode = runGenDeclCode iteProg in
  writeFile asmFile (prettyPrintCode asmCode)

tinyMainDeclInt = func "main" [] $ intLit 12
tinyMainDeclStr = func "main" [] $ strLit "Hello, world!\n"
helloWorldProg = func "main" [] $ printExpr $ strLit "Hello, world!\n"
iteProg = func "main" [] $ iteExpr (intGEQ (-122) (-1000)) (printExpr $ strLit "If expression\n") (printExpr $ strLit "Else expression\n")

asmFile = "testCodeGen.s"
