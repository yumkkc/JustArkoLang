module MyLib  where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

import Lisp

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "bebba" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value : " ++ show val


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                 "#t"      -> Bool True
                 "#f"      -> Bool False
                 otherwise -> Atom atom


parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber


someFunc :: IO ()
someFunc = do args <- getArgs
              putStrLn $ readExpr $ args !! 0
