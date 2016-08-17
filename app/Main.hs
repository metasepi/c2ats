module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import Language.C2ATS

main :: IO ()
main = do
  let usage = error "Usage: c2ats C_FILEPATH"
  args <- getArgs
  when (length args /= 1) usage
  let [fn] = args
  (files, globals) <- parseMyFile fn
  let fglobal = sortFlatGlobal . flatGlobal $ globals
  print $ myPrettyGlobal fglobal
