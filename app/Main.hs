module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import Language.C2ATS

getOpts :: [String] -> (String, [String])
getOpts (gcc:copts) = (gcc, copts)
getOpts []          = ("gcc", [])

main :: IO ()
main = do
  let usage = hPutStrLn stderr
                "Usage: c2ats gen C_HEADER_PATH [GCC_PATH [COMPILE_OPTS...]]"
                >> exitFailure
  args <- getArgs
  when (length args < 2) usage
  let cmd:fn:opts = args
  when (cmd /= "gen") usage
  let (gcc, copts) = getOpts opts

  (files, globals) <- parseMkGlobal gcc copts fn
  let global = injectForwardDecl . injectIncludes
               [ -- Includes
               ]
               [ -- Excludes
                 ".*"
               ] . sortFlatGlobal . flatGlobal $ globals
  preDefineGlobal fn >>= print >> print (atsPrettyGlobal global)
