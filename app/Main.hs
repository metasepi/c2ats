module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.Console.GetOpt
import Data.Maybe
import Data.List
import Data.Version (showVersion)
import Language.C2ATS
import Paths_c2ats (version)

data FlagGen = GenHelp | GenCpp String | GenOutput FilePath
  deriving (Show, Eq)

optionsGen :: [OptDescr FlagGen]
optionsGen =
  [ Option ['h'] ["help"]   (NoArg GenHelp)                  "show this help message"
  , Option ['c'] ["cpp"]    (ReqArg GenCpp "\"GCC CPPOPT\"") "GCC PATH and GCC options"
  , Option ['o'] ["output"] (ReqArg GenOutput "DIR")         "directory to output sats file"
  ]

compilerOptsGen :: [String] -> IO ([FlagGen], [String])
compilerOptsGen argv = do
  ret <- case getOpt Permute optionsGen argv of
    (o,n,[]  ) -> do
      if GenHelp `elem` o || null n then usageGen >> exitFailure else return (o,n)
    (_,_,errs) -> usageGen >> exitFailure
  return ret

getGccOpts :: [FlagGen] -> (String, [String])
getGccOpts opt = gccopt $ find go opt
  where
    go :: FlagGen -> Bool
    go (GenCpp _) = True
    go _          = False
    gccopt :: Maybe FlagGen -> (String, [String])
    gccopt Nothing             = ("gcc", ["-D_XOPEN_SOURCE"])
    gccopt (Just (GenCpp cpp)) = let c:o = words cpp in (c, "-D_XOPEN_SOURCE":o)

getOutOpt :: [FlagGen] -> Maybe FilePath
getOutOpt opt = strip $ find go opt
  where
    go :: FlagGen -> Bool
    go (GenOutput _) = True
    go _             = False
    strip :: Maybe FlagGen -> Maybe FilePath
    strip (Just (GenOutput f)) = Just f
    strip _                    = Nothing

usageGen :: IO ()
usageGen = hPutStrLn stderr (usageInfo header optionsGen)
  where header = "Usage: c2ats gen [OPTION...] C_HEADER_PATH"

sortedGlobal :: String -> [String] -> FilePath -> IO [FlatG]
sortedGlobal gcc copts fn = do
  (files, globals) <- parseMkGlobal gcc ("-D_XOPEN_SOURCE":copts) fn
  return $ injectForwardDecl . injectAccessor . injectIncludes
    [ -- Includes
    ]
    [ -- Excludes
      ".*"
    ] . sortFlatGlobal . flatGlobal $ globals

subcmdGen :: [String] -> IO ()
subcmdGen args = do
  (o,n) <- compilerOptsGen args
  let fn = head n
      (gcc, copts) = getGccOpts o
      out = getOutOpt o
  global <- sortedGlobal gcc copts fn
  if isNothing out
    then do
    preDefineGlobal fn >>= print >> print (atsPrettyGlobal global)
    else do
    let oDir    = fromJust out
        mGlobal = splitFlatGlobal global
    (mapHead, cTrees) <- headerTree gcc copts fn
    createSATS oDir mapHead cTrees

subcmdVer :: [String] -> IO ()
subcmdVer _ =
  putStrLn $ "c2ats version " ++ showVersion version ++ " with Copyright (c) 2016 Metasepi team"

usage :: IO ()
usage = hPutStrLn stderr msg >> exitFailure
  where
    msg = unlines [
      "Usage: c2ats (gen | version)",
      "",
      "Available commands:",
      "  gen",
      "  version"
      ]

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) usage
  let subcmd:opts = args
  case subcmd of
   "gen"     -> subcmdGen     opts
   "version" -> subcmdVer     opts
   _         -> usage
