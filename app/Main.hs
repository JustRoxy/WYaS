module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . eval . readExpr . (!! 0)