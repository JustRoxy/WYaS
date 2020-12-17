module Parser where

import Control.Monad.Error (throwError)
import Data.Functor
import Data.Void
import Datatypes
import Errors.Error
import Numeric
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> try parseDecimal <|> parseNumber <|> parseQuoted <|> parseLists

parseLists :: Parser LispVal
parseLists = do
  char '('
  space
  x <- try parseDottedList <|> parseList
  space
  char ')'
  return x

parseDottedList :: Parser LispVal
parseDottedList = do
  x <- endBy parseExpr space
  y <- char '.' >> space >> parseExpr
  return $ DottedList x y

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = List <$> sepEndBy parseExpr space

escaped :: Parser Char
escaped = char '\\' >> oneOf "\\\"nrtbfv0"

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escaped <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letterChar <|> symbol
  rest <- many (letterChar <|> digitChar <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + mod i 10

parseBasedNumber :: (String -> Integer) -> Parser LispVal
parseBasedNumber base = Number . base <$> some digitChar

parseBase :: Parser LispVal
parseBase = do
  base <- char '#' >> letterChar
  parseBasedNumber $ case base of
    'b' -> bintodec . read
    'o' -> fst . head . readOct
    'x' -> fst . head . readHex
    _ -> read -- TODO Should be an error

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  r <- many (noneOf " ")
  (return . Char) r

parseNumber :: Parser LispVal
parseNumber = parseBase <|> parseBasedNumber read

-- TODO: Decimal exactness prefix, precision
parseDecimal :: Parser LispVal
parseDecimal = do
  num <- some digitChar
  char '.'
  floating <- some digitChar
  return . Decimal . fst . head . readFloat $ num ++ "." ++ floating

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"