module Lisp where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.IORef
import Control.Monad.Except
import System.IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Character Char
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String]
                     , vararg :: Maybe String
                     , body :: [LispVal]
                     , closure :: Env }
             | Port Handle


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
showVal (PrimitiveFunc _ )  = "<primitive>"
showVal (Port _)            = "<IO Port>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where show = showVal

-- ADT for Error
data LispError = NumArgs Int [LispVal]
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


type IOThrowsError = ExceptT LispError IO

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either LispError a -> a
extractValue (Right val) = val
