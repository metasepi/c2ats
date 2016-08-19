module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import Language.C2ATS

main :: IO ()
main = do
  let usage = error "Usage: c2ats new C_FILEPATH"
  args <- getArgs
  when (length args /= 2) usage
  let cmd:[fn] = args
  when (cmd /= "new") usage
  (files, globals) <- parseMyFile fn
  let fglobal = sortFlatGlobal . flatGlobal $ globals
  -- xxx Pick up typedef using pointer
  -- xxx Split with files
  print $ atsPrettyGlobal fglobal
  -- xxx Create sats files
