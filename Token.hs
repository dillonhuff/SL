module Token(Token,
             reservedTokName,
             pos,
             dres,
             dbuiltin,
             ddelim,
             dident,
             dintLit,
             dstringLit,
             delim,
             ident,
             builtin,
             intLit,
             stringLit,
             getStringLitValue,
             getIntLitValue,
             res,
             TokenType(..),
             tokenType) where

import Text.Parsec.Pos

data Token
  = Token SourcePos TokenValue
    deriving (Ord, Show)

instance Eq Token where
  (==) (Token _ l) (Token _ r) = l == r

tokenType (Token _ (Resword _)) = RESERVED
tokenType (Token _ (StringLit _)) = STRINGLITERAL

data TokenValue
  = Resword String
  | Identifier String
  | IntLit Integer
  | StringLit String
    deriving (Eq, Ord, Show)

data TokenType
  = STRINGLITERAL
  | INTEGERLITERAL
  | IDENTIFIER
  | RESERVED
    deriving (Eq, Ord, Show)

reservedTokName (Token _ (Resword n)) = n
getStringLitValue (Token _ (StringLit s)) = s
getIntLitValue (Token _ (IntLit i)) = i

pos (Token p _) = p

res p n = Token p (Resword n)
builtin p n = Token p (Resword n)
ident p n = Token p (Identifier n)
delim p n = Token p (Resword n)
intLit p i = Token p (IntLit i)
stringLit p str = Token p (StringLit str)

dres n = Token dPos (Resword n)
dbuiltin n = Token dPos (Resword n)
ddelim n = Token dPos (Resword n)
dident n = Token dPos (Identifier n)
dintLit i = Token dPos (IntLit i)
dstringLit str = Token dPos (StringLit str)

dPos = newPos "DUMMY" 0 0
