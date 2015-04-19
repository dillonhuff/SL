module Main(main) where

import CodeGen
import Syntax

main =
  let asmCode = runGenDeclCode helloWorldProg in
  writeFile asmFile (prettyPrintCode asmCode)

tinyMainDeclInt = func "main" [] $ intLit 12
tinyMainDeclStr = func "main" [] $ strLit "Hello, world!\n"
helloWorldProg = func "main" [] $ printExpr $ strLit "Hello, world!\n"

asmFile = "testCodeGen.s"
