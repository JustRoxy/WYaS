module Datatypes where

import Control.Monad.Trans.Error
import Data.IORef
import qualified Data.Text as T
import Data.Void
import System.IO
import Text.Megaparsec
import TextShow

data LispVal
  = Atom T.Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Decimal Float
  | String T.Text
  | Bool Bool
  | Char T.Text
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [T.Text], vararg :: Maybe T.Text, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

type Env = IORef [(T.Text, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

type ThrowsError = Either LispError

showVal :: LispVal -> T.Text
showVal (String contents) = "\"" <> contents <> "\""
showVal (Char contents) = "'" <> contents <> "'"
showVal (Decimal contents) = showt contents
showVal (Atom name) = name
showVal (Number contents) = showt contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" <> unwordsList contents <> ")"
showVal (DottedList head tail) = "(" <> unwordsList head <> " . " <> showVal tail <> ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
  "(lambda (" <> T.unwords (map showt args)
    <> ( case varargs of
           Nothing -> ""
           Just args -> " . " <> args
       )
    <> ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

instance Show LispVal where show = T.unpack . showVal

instance TextShow LispVal where
  showb v = fromString (show v)

unwordsList :: [LispVal] -> T.Text
unwordsList = T.unwords . map showVal

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | Parser (ParseErrorBundle T.Text Void)
  | BadSpecialForm T.Text LispVal
  | NotFunction T.Text T.Text
  | UnboundVar T.Text T.Text
  | Default T.Text

showError :: LispError -> T.Text
showError (UnboundVar message varname) = message <> ": " <> varname
showError (BadSpecialForm message form) = message <> ": " <> showt form
showError (NotFunction message func) = message <> ": " <> showt func
showError (NumArgs expected found) = "Expected " <> showt expected <> " args: found values " <> unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " <> expected <> " found: " <> showt found
showError (Parser parseErr) = "Parse error at " <> T.pack (show parseErr)

instance Show LispError where show = T.unpack . showError