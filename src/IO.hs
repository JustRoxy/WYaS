module IO where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Datatypes
import Env
import Errors.Error
import Eval
import Lib
import System.IO
import Text.Pretty.Simple (pPrint, pPrintString)

flushStr :: T.Text -> IO ()
flushStr str = T.putStr str >> hFlush stdout

readPrompt :: T.Text -> IO T.Text
readPrompt prompt = flushStr prompt >> T.getLine

evalString :: Env -> T.Text -> IO T.Text
evalString env expr = T.pack <$> (runIOThrows . fmap show $ liftThrows (readExpr expr) >>= eval env)

evalAndPrint :: Env -> T.Text -> IO ()
evalAndPrint env expr = evalString env expr >>= pPrint

runOne :: [T.Text] -> IO ()
runOne [] = T.hPutStrLn stderr "RUNONE NO ARGS"
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List . map String $ drop 1 args)]
  (runIOThrows . fmap show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>>") . evalAndPrint