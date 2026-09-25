module MyLib  where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Functor

import Lisp
import Numeric
import Numeric (readOct, readHex)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space


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


parseRadix :: Parser Char
parseRadix = option 'd' (char '#' >> oneOf "dbox")

hexVals :: Parser Char
hexVals = oneOf "abcdefABCDEF" <|> digit

extractNumber :: ReadS a -> String -> a
extractNumber f = fst . head . f

parseNumber :: Parser LispVal
parseNumber = do redix <- parseRadix
                 Number <$> case redix of
                   'x' -> extractNumber readHex <$> many1 hexVals
                   'd' -> extractNumber readDec <$> many1 digit
                   'b' ->  extractNumber readBin <$> many1 digit
                   'o' -> extractNumber readOct <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> String
readExpr input = case parse parseNumber "bebba" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value : " ++ show val


someFunc :: IO ()
someFunc = do args <- getArgs
              putStrLn $ readExpr $ args !! 0
