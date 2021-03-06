module Lib where

import Control.Monad.Error (MonadError (throwError))
import qualified Data.Text as T
import Datatypes
import Errors.Error
import Parser
import Text.Megaparsec

readOrThrow :: Parser a -> T.Text -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr

readExprList = readOrThrow parseExprList
