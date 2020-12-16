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
  deriving (Show)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser (ParseErrorBundle String Void)
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  deriving (Show)
