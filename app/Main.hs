module Main where

import Errors.Error
import Lib
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled