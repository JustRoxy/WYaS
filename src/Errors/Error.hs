module Errors.Error where

import Control.Monad.Error
import Datatypes

type ThrowsError = Either LispError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

throwError = throwError