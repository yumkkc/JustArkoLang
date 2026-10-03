{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Eval where

import           Control.Monad.Except
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import           Error
import           Lisp

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _)             = return val
eval _ val@(Number _)             = return val
eval _ val@(Bool _)               = return val
eval env (Atom id)                = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", cond, tr, fl])       = evalCond env cond tr fl
eval env (List [Atom "set!", Atom var, form])   = eval env form >>= setVar env var
eval env (List (Atom "define" : List (Atom funName : params) : body))
  = defineFun funName params Nothing body env
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom funcName : args))  = do func <- getVar env funcName --- inside IOThrowsError Monad
                                             argsEvaled <- mapM (eval env) args
                                             apply func argsEvaled


eval _ badForm = throwError $ BadSpecialForm "Unrecognized Special Form " badForm


-- Applying of the functions
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) arg =  func arg
apply (Func params varargs body closure) args
   | length params /= length args && isNothing varargs = throwError $ NumArgs (length params) args
   | otherwise   = do env' <- liftIO $ bindVars closure fullBindings
                      last <$> mapM (eval env') body
      where 
         varBind = case varargs of
            Nothing     -> []
            Just varVar -> [(varVar, List $ drop (length params)  args)]
         fullBindings = zip params args ++ varBind
                                              
-- unpacking
unpackNum :: LispVal -> IOThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                            then throwError $ TypeMismatch "number " $ String n
                            else return $ fst $ head parsed
unpackNum (List [n])  = unpackNum n
unpackNum notNum           = throwError $ TypeMismatch "number " notNum

unpackStr :: LispVal -> IOThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool b)   = return $ show b
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> IOThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- main functions
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> IOThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params <&> Number . foldl1 op

checkSymbol :: [LispVal] -> IOThrowsError LispVal
checkSymbol [Atom _] = return $ Bool True
checkSymbol _        = return $ Bool False


boolBinop :: (LispVal -> IOThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> IOThrowsError LispVal
boolBinop unpacker op args | length args /= 2 = throwError $ NumArgs 2 args
                          | otherwise        = do left <- unpacker $ args !! 0
                                                  right <- unpacker $ args !! 1
                                                  return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- if: lets implement this not as a normal function as a normal function will be evaluated its arguments

evalCond :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalCond env cond tr fl = do condval <- eval env cond
                             case condval of
                               Bool False -> eval env fl
                               _          -> eval env tr

car :: [LispVal] -> IOThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> IOThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> IOThrowsError LispVal
cons [x1 , List []]           = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> IOThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]     | length arg1 /= length arg2 = return $ Bool False
                               | otherwise    =  Bool . and <$> mapM eqvPair (zip arg1 arg2)
                               where eqvPair (x1, x2) = do
                                       eqvRes <- eqv [x1, x2]
                                       return $ case eqvRes of
                                         Bool val  -> val
                                         _         -> False


eqv [_, _]  = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> IOThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> IOThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do unpacked1 <- unpacker arg1
                                                   unpacked2 <- unpacker arg2
                                                   return $ unpacked1 == unpacked2
                                                `catchError` (const $ return False)


equal :: [LispVal] -> IOThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList    = throwError $ NumArgs 2 badArgList

-- function primitives
primitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
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
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]


-- For Environment
isBound :: Env -> String -> IO Bool
isBound env var = isJust . lookup var <$> readIORef env

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef -- env will have IOThrowsError Env
                       case lookup var env of
                          Just val -> liftIO $ readIORef val
                          Nothing  -> throwError $ UnboundVar "Variable not found " var


-- for setVar we gotta check if its already bound
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             case lookup var env of
                                Just bounded  -> liftIO $ writeIORef bounded value
                                Nothing       -> throwError $ UnboundVar "Setting up unbounded variable " var
                             return value



defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
   isBounded <- liftIO $ isBound envRef var -- env has Bool
   if isBounded
   then setVar envRef var value -- wthis return IOThrowsError LispVal 
   else do
      env <- liftIO $ readIORef envRef       -- env have base [(String, IORef)]
      newVal <- liftIO $ newIORef value      -- newVal has IORef LispVal
      liftIO $ writeIORef envRef $ (var, newVal) : env
      return value       -- env is done


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do env <- readIORef envRef -- env :: [(String, IORef LispVal)]
                              newEnv <- (++ env) <$> mapM newVar bindings
                              newIORef newEnv
                        where
                          -- newVar ::  (String, LispVal) -> IO (String, IORef LispVal)
                          newVar (var, value) = (var, ) <$> newIORef value


-- build an new Environment with the primitives
-- Env :: IORef [(String, IORef LispVal)]
-- nullEnv :: IO ( IORef [(String, IORef LispVal)] )
primEnv :: IO Env
primEnv = do env <- nullEnv
             prim' <- mapM primhelper primitives
             writeIORef env prim'
             return env
        where
          primhelper (var, val) = newIORef (PrimitiveFunc val) >>= return . (var, )


checkAtom :: LispVal -> IOThrowsError String
checkAtom (Atom s) = return s
checkAtom v      = throwError $ TypeMismatch "param must be identifier" v


defineFun :: String -> [LispVal] -> Maybe String -> [LispVal] -> Env -> IOThrowsError LispVal
defineFun name paramval varparam body env = do
  params <- mapM checkAtom paramval
  let fundefn = Func params varparam body env
  defineVar env name fundefn
