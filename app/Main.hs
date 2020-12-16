module Main where

import Errors.Error
import Lib
import Parser
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  either pPrint pPrint evaled