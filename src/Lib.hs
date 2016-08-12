module Lib
    ( parseMyFile
    , printMyAST
    , printMyGlobal
    ) where

import System.IO
import Control.Arrow      hiding ((<+>))
import Data.List
import Language.C
import Language.C.Analysis
import Language.C.System.GCC
import Language.C.Data.Node

-- parseMyFile :: FilePath -> IO [(FilePath, CTranslUnit)]
parseMyFile :: FilePath -> IO GlobalDecls
parseMyFile input_file = do
  let compiler = newGCC "gcc"
      opts = []
  ast <- parseCFile compiler Nothing opts input_file >>= checkResult "[parsing]"
  (globals, warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast
  mapM (hPutStrLn stderr . show) warnings
  return globals
    where
      checkResult :: (Show a) => String -> (Either a b) -> IO b
      checkResult label = either (error . (label++) . show) return

printMyAST :: CTranslUnit -> IO ()
printMyAST = print . pretty

printMyGlobal :: GlobalDecls -> IO ()
printMyGlobal = print . pretty

groupAstBySourceFile :: CTranslUnit -> [(FilePath, CTranslUnit)]
groupAstBySourceFile (CTranslUnit decls _) =
  map (\decls -> (fileOfNode' (head decls), CTranslUnit decls (topNodePos decls))) .
  groupBy (\a b -> (fileOfNode' a) == fileOfNode' b) $ decls
    where
      fileOfNode' = maybe "<no-file>" id . fileOfNode
      topNodePos decls =
        let lastDecl = nodeInfo (last decls) in
        mkNodeInfoPosLen (posOf (head decls)) (getLastTokenPos lastDecl)
