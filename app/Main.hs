module Main where

import Errors.Error
import Eval
import Lib
import Parser
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  let val = readExpr (head args)
  eitherPrint val
  let evaled = val >>= eval
  eitherPrint evaled
  where
    eitherPrint = either pPrint pPrint