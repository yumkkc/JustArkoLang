module Lisp where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Character Char
             | String String
             | Bool Bool
        deriving Show
