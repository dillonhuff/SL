module CodeGen() where

import Syntax

data CodeBlock
  = CodeBlock
    deriving (Eq, Ord, Show)

newCodeBlock = CodeBlock

genExprCode :: Expr -> CodeBlock
genExprCode e = newCodeBlock
