module Main(main) where

import CodeGen
import Syntax

main =
  let asmCode = runGenDeclCode tinyMainDeclInt in
  putStrLn $ prettyPrintCode asmCode

tinyMainDeclInt = func "main" [] $ intLit 12
tinyMainDeclStr = func "main" [] $ strLit "Hello, world!\n"
