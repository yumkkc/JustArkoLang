module Lisp where

import Text.ParserCombinators.Parsec hiding (spaces)

import Data.IORef
import Error (IOThrowsError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Character Char
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> IOThrowsError)
             | Func { params :: [String]
                     , vararg :: Maybe String
                     , body :: [LispVal]
                     , closure :: Env }


showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List contents)   = "(" ++ unwordsList contents ++ ")"
showVal (DottedList listContents lastVal) = "(" ++
                                            unwordsList listContents ++
                                            " . " ++ showVal lastVal
                                            ++ ")"
showVal(PrimitiveFunc _ ) = "<primitive>"                                             
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message funName) = message ++ ": " ++ funName
showError (NumArgs expected found) = "Expected : " ++ show expected ++
                                        " args : found values" ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []