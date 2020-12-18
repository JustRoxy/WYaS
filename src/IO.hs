module IO where

import Env
import Errors.Error
import Eval
import Lib
import System.IO hiding (try)
import Text.Pretty.Simple (pPrint, pPrintString)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows . fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= pPrint

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>>") . evalAndPrint