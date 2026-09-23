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


parseQuote :: Parser Char
parseQuote = char '\\' >> char '"'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ (try parseQuote <|> noneOf "\"")
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


parseRadixNotHex :: Parser Char
parseRadixNotHex = option 'd' (char '#' >> oneOf "dbo")

parseNumberWithRadixNotHex :: Parser LispVal
parseNumberWithRadixNotHex = do radix <- parseRadixNotHex
                                rest  <- many1 digit
                                return $ case radix of 
                                      'd' -> (Number . Decimal . read) rest
                                      'b' -> (Number . Binary . read) rest
                                      'o' -> (Number . Octal . read) rest
                                
hexVals :: Parser Char
hexVals = oneOf "abcdefABCDEF" <|> digit

parseHex :: Parser LispVal
parseHex = do _ <- char '#'
              _ <- char 'x'
              vals <- many1 hexVals
              return $ (Number . Hexa) vals

parseNumber :: Parser LispVal              
parseNumber = try parseHex <|> parseNumberWithRadixNotHex

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseNumber <|> parseAtom 


someFunc :: IO ()
someFunc = do args <- getArgs
              putStrLn $ readExpr $ args !! 0
