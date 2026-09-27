module Eval where

import Lisp
import Control.Monad.Except
import Data.Functor
import Error
import Lisp (LispError(TypeMismatch))

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm                    = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args) (lookup func primitives)

-- unpacking
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n])  = unpackNum n
unpackNum notNum           = throwError $ TypeMismatch "number" notNum

punpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool b)   = return $ show b
unpackStr notString     = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- main functions
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params <&> Number . foldl1 op

checkSymbol :: [LispVal] -> ThrowsError LispVal
checkSymbol [Atom _] = return $ Bool True
checkSymbol _        = return $ Bool False


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args | length args /= 2 = throwError $ NumArgs 2 args
                          | otherwise        = do left <- unpacker $ args !! 0
                                                  right <- unpacker $ args !! 1
                                                  return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- function primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop(-)),
              ("*", numericBinop(*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", checkSymbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))]
