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
                 "^/usr/include/glib-2.0/gio/",
                 "^/usr/include/glib-2.0/glib/",
                 "^/usr/include/glib-2.0/gobject/",
                 "^/usr/include/gtk-3.0/gdk/",
                 "^/usr/include/gtk-3.0/gtk/",
                 "^/usr/include/x86_64-linux-gnu/",
                 "^/usr/lib/gcc/x86_64-linux-gnu/",
                 "^/usr/lib/x86_64-linux-gnu/glib-2.0/"
               ] . sortFlatGlobal . flatGlobal $ globals
  preDefineGlobal fn >>= print >> print (atsPrettyGlobal global)
