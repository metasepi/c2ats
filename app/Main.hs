module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
  let usage = error "Usage: c2ats C_CODE_PATH"
  args <- getArgs
  when (length args /= 1) usage
  let [fn] = args
  parseMyFile fn >>= printMyAST
