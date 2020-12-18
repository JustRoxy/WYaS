{-# LANGUAGE ExistentialQuantification #-}

module Eval where

import Control.Monad
import Control.Monad.Error (catchError, liftIO, return, throwError)
import Data.Maybe
import Datatypes
import Debug.Trace (trace)
import Env
import Lib
import System.IO

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  case (==) <$> unpacker arg1 <*> unpacker arg2 of
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

elemV :: LispVal -> [LispVal] -> Bool
elemV _ [] = False
elemV value (x : xs) = case eqv [value, x] of
  Left _ -> elemV value xs
  Right (Bool False) -> elemV value xs
  Right (Bool True) -> True

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List (Atom "case" : pred : rest)) = do
  ptrn <- eval env pred
  liftThrows $ caseFunc ptrn rest
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . showVal

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else (liftIO . bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
apply f args = throwError $ NotFunction "Application to a non-function" (show f ++ " with " ++ show args)

caseFunc :: LispVal -> [LispVal] -> ThrowsError LispVal
caseFunc ptrn ((List [List v, result]) : xs) = if ptrn `elemV` v then return result else caseFunc ptrn xs
caseFunc ptrn [] = throwError . BadSpecialForm "non-exhaustive patterns" $ ptrn
caseFunc _ v = throwError . TypeMismatch "pattern" $ List v

cond :: [LispVal] -> ThrowsError LispVal
cond ((List [Bool v, r]) : rest)
  | v = return r
  | otherwise = cond rest
cond [] = throwError $ BadSpecialForm "non-exhaustive patterns" (Atom "cond")
cond [List [Atom "else", r]] = return r
cond d = throwError $ NumArgs 2 d

elseCond :: [LispVal] -> ThrowsError LispVal
elseCond [v] = return $ List [Atom "else", v]
elseCond v = throwError $ NumArgs 1 v

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
    ("else", elseCond)
  ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-file", closePort),
    ("close-output-file", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port . liftIO $ openFile filename mode
makePort _ v = throwError $ NumArgs 1 v

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc v = throwError $ NumArgs 1 v

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = String <$> liftIO (readFile filename)
readContents v = throwError $ NumArgs 1 v

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

boolBinop ::
  (LispVal -> ThrowsError a) ->
  (a -> a -> Bool) ->
  [LispVal] ->
  ThrowsError LispVal
boolBinop unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise = do
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
eqv [List x, List y] =
  Bool . all (\(Bool x) -> x) <$> zipWithM (\f s -> equal [f, s]) x y
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
