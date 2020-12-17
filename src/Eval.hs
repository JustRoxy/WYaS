{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Control.Monad.Error (catchError, return, throwError)
import Data.Text hiding (cons, foldl1, head, length, null)
import Datatypes
import Errors.Error

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = undefined

unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return (unpacked1 == unpacked2) `catchError` const (return False)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    _ -> eval conseq
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [x1, x2] = return . Bool $ x1 == x2
eqv badArgList = throwError $ NumArgs 2 badArgList

apply :: Text -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    $ lookup func primitives

primitives :: [(Text, [LispVal] -> ThrowsError LispVal)]
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
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv)
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

unpackStr :: LispVal -> ThrowsError Text
unpackStr (String s) = return s
unpackStr (Number s) = return . pack $ show s
unpackStr (Bool s) = return . pack $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- Excercise 2: remove next 6 lines
-- TODO: add decimal support
unpackNum (String n) =
  let parsed = reads (unpack n)
   in if null parsed
        then throwError . TypeMismatch "number" $ String n
        else return . fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
