module Token(Token,
             dres,
             res) where

import Text.Parsec.Pos

data Token
  = Token SourcePos TokenValue
    deriving (Ord, Show)

instance Eq Token where
  (==) (Token _ l) (Token _ r) = l == r

data TokenValue
  = Resword String
    deriving (Eq, Ord, Show)

res p n = Token p (Resword n)

dres n = Token dPos (Resword n)

dPos = newPos "DUMMY" 0 0
