module Main where

import Errors.Error
import Lib
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print args
  let evaled = fmap show $ readExpr (head args) >>= eval
  print evaled