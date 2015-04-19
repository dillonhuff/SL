module Token(Token,
             dres,
             ddelim,
             dident,
             delim,
             ident,
             res) where

import Text.Parsec.Pos

data Token
  = Token SourcePos TokenValue
    deriving (Ord, Show)

instance Eq Token where
  (==) (Token _ l) (Token _ r) = l == r

data TokenValue
  = Resword String
  | Identifier String
    deriving (Eq, Ord, Show)

res p n = Token p (Resword n)
ident p n = Token p (Identifier n)
delim p n = Token p (Resword n)

dres n = Token dPos (Resword n)
ddelim n = Token dPos (Resword n)
dident n = Token dPos (Identifier n)

dPos = newPos "DUMMY" 0 0
