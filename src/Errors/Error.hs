module Errors.Error where

import Control.Monad.Error
import qualified Data.Text as T
import Datatypes
import TextShow

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg str = Default $ T.pack str

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val