module Error where

import Lisp
import Control.Monad.Except

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- Monad Transformer

type IOThrowsError = ExceptT LispError IO

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)