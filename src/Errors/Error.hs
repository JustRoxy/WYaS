module Errors.Error where

import Control.Monad.Error
import Datatypes

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val