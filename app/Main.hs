module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import Data.Version (showVersion)
import Language.C2ATS
import Paths_c2ats (version)

usage :: IO ()
usage = hPutStrLn stderr msg >> exitFailure
  where
    msg = init $ unlines [
      "Usage: c2ats (gen | version)",
      "",
      "Available commands:",
      "  gen C_HEADER_PATH [GCC_PATH [COMPILE_OPTS...]]",
      "  version"
      ]

getOpts :: [String] -> (String, [String])
getOpts (gcc:copts) = (gcc, copts)
getOpts []          = ("gcc", [])

subcmdGen :: [String] -> IO ()
subcmdGen args = do
  when (length args < 1) usage
  let fn:opts = args
      (gcc, copts) = getOpts opts
  (files, globals) <- parseMkGlobal gcc ("-D_XOPEN_SOURCE":copts) fn
  let global = injectForwardDecl . injectAccessor . injectIncludes
               [ -- Includes
               ]
               [ -- Excludes
                 ".*"
               ] . sortFlatGlobal . flatGlobal $ globals
  preDefineGlobal fn >>= print >> print (atsPrettyGlobal global)

subcmdVer :: [String] -> IO ()
subcmdVer _ =
  putStrLn $ "c2ats version " ++ showVersion version ++ " with Copyright (c) 2016 Metasepi team"

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) usage
  let subcmd:opts = args
  case subcmd of
   "gen"     -> subcmdGen opts
   "version" -> subcmdVer opts
   _         -> usage
