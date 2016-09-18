module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.Console.GetOpt
import Language.C2ATS


usage :: IO ()
usage =
  hPutStrLn stderr "Usage: c2ats gen C_HEADER_PATH [GCC_PATH [COMPILE_OPTS...]]"
  >> exitFailure

getOpts :: [String] -> (String, [String])
getOpts (gcc:copts) = (gcc, copts)
getOpts []          = ("gcc", [])

subcmdGen :: [String] -> IO ()
subcmdGen args = do
  when (length args < 1) usage
  let fn:opts = args
      (gcc, copts) = getOpts opts
  (files, globals) <- parseMkGlobal gcc copts fn
  let global = injectForwardDecl . injectIncludes
               [ -- Includes
               ]
               [ -- Excludes
                 ".*"
               ] . sortFlatGlobal . flatGlobal $ globals
  preDefineGlobal fn >>= print >> print (atsPrettyGlobal global)

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) usage
  let subcmd:opts = args
  case subcmd of
   "gen" -> subcmdGen opts
   _     -> usage
