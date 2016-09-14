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
  let global = injectForwardDecl . injectIncludes [
        "/usr/lib/gcc/x86_64-linux-gnu/",
        "/usr/include/x86_64-linux-gnu/"
        ] . sortFlatGlobal . flatGlobal $ globals
  print preDefineGlobal >> print (atsPrettyGlobal global)
