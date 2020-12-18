module Main where

import IO
import Lib
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args