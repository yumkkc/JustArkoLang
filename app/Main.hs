module Main where

import System.Environment
import System.IO

import Parser
import Eval
import Error
import Lisp

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> (readExpr expr >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = primEnv >>= flip evalAndPrint expr
runRepl :: IO ()

runRepl = primEnv >>= until_ (== "quit") (readPrompt ">> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    _ -> putStrLn "Program takes only 0 or 1 args"
main2 = getLine >>= runParserDirect . readExpr
