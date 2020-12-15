module Parser where

import Control.Monad.Error (MonadError (throwError))
import Datatypes
import Errors.Error
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> try parseDecimal <|> parseNumber <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

isTypeOf :: String -> LispVal -> LispVal
isTypeOf "string" (String _) = Bool True
isTypeOf "symbol" (List (Atom "quote" : _)) = Bool True
isTypeOf "symbol" (Atom _) = Bool True
isTypeOf "list" (List _) = Bool True
isTypeOf "number" (Decimal _) = Bool True
isTypeOf "number" (Number _) = Bool True
isTypeOf _ _ = Bool False

parseTypeQuestion :: Parser LispVal
parseTypeQuestion = do
  question <- many1 letter <* char '?' <* many space
  isTypeOf question <$> parseExpr

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number . foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
--Excercise 2: remove next 6 lines
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then 0
        else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

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

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

escaped :: Parser Char
escaped = char '\\' *> oneOf "\\\"nrtbfv0"

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escaped <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + mod i 10

parseBasedNumber :: (String -> Integer) -> Parser LispVal
parseBasedNumber base = Number . base <$> many1 digit

parseBase :: Parser LispVal
parseBase = do
  base <- char '#' *> letter
  parseBasedNumber $ case base of
    'b' -> bintodec . read
    'o' -> fst . head . readOct
    'x' -> fst . head . readHex
    _ -> read

parseChar :: Parser LispVal
parseChar = string "#\\" >> many (noneOf " ") >>= (return . Char)

parseNumber :: Parser LispVal
parseNumber = parseBase <|> parseBasedNumber read

-- TODO: Decimal exactness prefix, precision
parseDecimal :: Parser LispVal
parseDecimal = do
  num <- many1 digit
  char '.'
  floating <- many1 digit
  return . Decimal . fst . head . readFloat $ num ++ "." ++ floating

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space