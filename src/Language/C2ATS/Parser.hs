module Language.C2ATS.Parser
       ( parseMyFile
       , printMyAST
       , printMyGlobal
       ) where

import System.IO
import Control.Arrow      hiding ((<+>))
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Language.C
import Language.C.Analysis
import Language.C.System.GCC
import Language.C.Data.Node

parseMyFile :: FilePath -> IO [(FilePath, GlobalDecls)]
parseMyFile input_file = do
  let compiler = newGCC "gcc"
      opts = []
  ast <- parseCFile compiler Nothing opts input_file >>= checkResult "[parsing]"
  (globals, warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast
  mapM_ (hPutStrLn stderr . show) warnings
  return $ groupGlobalsBySourceFile globals
  where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return

printMyAST :: CTranslUnit -> IO ()
printMyAST = print . pretty

printMyGlobal :: GlobalDecls -> IO ()
printMyGlobal = print . pretty

groupGlobalsBySourceFile :: GlobalDecls -> [(FilePath, GlobalDecls)]
groupGlobalsBySourceFile gmap = map (\a -> (a, fltr a gmap)) fAll
  where
    d2f :: DeclEvent -> FilePath
    d2f = maybe "" id . fileOfNode
    fObjs, fTags, fTypeDefs, fAll :: [FilePath]
    fObjs     = map (d2f . DeclEvent)    $ Map.elems (gObjs gmap)
    fTags     = map (d2f . TagEvent)     $ Map.elems (gTags gmap)
    fTypeDefs = map (d2f . TypeDefEvent) $ Map.elems (gTypeDefs gmap)
    fAll      = delete "" . nub $ fObjs ++ fTags ++ fTypeDefs
    fltr :: FilePath -> GlobalDecls -> GlobalDecls
    fltr f = filterGlobalDecls ((== f) . d2f)
