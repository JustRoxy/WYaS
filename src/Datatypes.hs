module Datatypes where

import Data.Void
import Text.Megaparsec

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Decimal Float
  | String String
  | Bool Bool
  | Char String

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char contents) = "'" ++ contents ++ "'"
showVal (Decimal contents) = show contents
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser (ParseErrorBundle String Void)
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError