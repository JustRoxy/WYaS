module Lib where

import Control.Monad.Error (MonadError (throwError))
import Datatypes
import Errors.Error
import Parser
import Text.ParserCombinators.Parsec (parse)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
