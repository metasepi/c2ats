module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
  let usage = error "Usage: c2ats C_FILEPATH"
  args <- getArgs
  when (length args /= 1) usage
  let [fn] = args
  gglobals <- parseMyFile fn
  mapM_ (\a -> do
            print . fst $ a
            printMyGlobal . snd $ a) gglobals
  return ()
