{-# LANGUAGE ExistentialQuantification #-}

module Eval where

import Control.Monad
import Control.Monad.Error (catchError, return, throwError)
import Datatypes
import Debug.Trace
import Errors.Error

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = case (==) <$> unpacker arg1 <*> unpacker arg2 of
  Left _ -> return False
  Right v -> return v

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
-- Excercise 2: remove next 6 lines
-- TODO: add decimal support
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError . TypeMismatch "number" $ String n
        else return . fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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
eval (List [v]) = eval v
eval (List v) = mapM eval v >>= (return . List)
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    $ lookup func primitives

cond :: [LispVal] -> ThrowsError LispVal
cond ((List [Bool v, r]) : rest) =
  trace ("p = " ++ show v ++ ", r = " ++ show r) $
    if v then return r else cond rest
cond [] = throwError $ BadSpecialForm "exhausted pattern" (Atom "cond")
cond [List [Atom "else", r]] = return r
cond d = throwError $ NumArgs 2 d

elsE :: [LispVal] -> ThrowsError LispVal
elsE [v] = return $ List [Atom "else", v]
elsE v = throwError $ NumArgs 1 v

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
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal),
    ("cond", cond),
    ("else", elsE)
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

-- This is straight up fucked and i dont like it
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Atom a, Atom b] = return . Bool $ a == b
eqv [List [], List []] = return $ Bool True
eqv [List x, List y] = do
  v <- zipWithM (\f s -> equal [f, s]) x y
  return . Bool $ all (\(Bool x) -> x) v
eqv [DottedList a c, DottedList b d] = do
  listCompare <- eqv [List a, List b] >>= unpackBool
  dotCompare <- eqv [c, d] >>= unpackBool
  return . Bool $ listCompare && dotCompare
eqv [Number a, Number b] = return . Bool $ a == b
eqv [Decimal a, Decimal b] = return . Bool $ a == b
eqv [String a, String b] = return . Bool $ a == b
eqv [Bool a, Bool b] = return . Bool $ a == b
eqv [Char a, Char b] = return . Bool $ a == b
eqv [_, _] = return . Bool $ False
eqv badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList