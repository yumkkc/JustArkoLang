module Lisp where

data LispNum = Decimal Integer
             | Hexa  String
             | Octal Integer
             | Binary Integer
          deriving Show

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | String String
             | Bool Bool
        deriving Show
