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
  parseAtom <|> parseString <|> try parseDecimal <|> parseNumber <|> parseQuoted
    <|> do
      char '('
      space
      x <- try parseList <|> parseDottedList
      space
      char ')'
      return x

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
  question <- some letterChar <* char '?' <* many space
  isTypeOf question <$> parseExpr

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (==)),
    (">", numBoolBinop (==)),
    ("/=", numBoolBinop (==)),
    (">=", numBoolBinop (==)),
    ("<=", numBoolBinop (==)),
    ("&&", boolBoolBinop (==)),
    ("||", boolBoolBinop (==)),
    ("string=?", strBoolBinop (==)),
    ("string?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=))
  ]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise =
    do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return . Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
--Excercise 2: remove next 6 lines
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr space
  tail <- char '.' >> space >> parseExpr
  return $ DottedList head tail

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
  num <- many digitChar
  char '.'
  floating <- many digitChar
  return . Decimal . fst . head . readFloat $ num ++ "." ++ floating

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"