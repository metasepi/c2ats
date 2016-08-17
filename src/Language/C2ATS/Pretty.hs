module Language.C2ATS.Pretty
       ( printGlobal
       , myPrettyGlobal
       , flatGlobal
       , sortFlatGlobal
       ) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ

import Language.C
import Language.C.Analysis

printGlobal :: GlobalDecls -> IO ()
printGlobal = print . pretty

myPrettyGlobal :: [(SUERef, FlatGlobalDecl)] -> Doc
myPrettyGlobal = vcat . map f
  where
    f :: (SUERef, FlatGlobalDecl) -> Doc
    f (s, d) = (text . show $ s) <+> text " ~> " <+> pretty d

data FlatGlobalDecl = FGObj  IdentDecl
                    | FGTag  TagDef
                    | FGType TypeDef

instance CNode FlatGlobalDecl where
  nodeInfo (FGObj  d) = nodeInfo d
  nodeInfo (FGTag  d) = nodeInfo d
  nodeInfo (FGType d) = nodeInfo d

instance Pretty FlatGlobalDecl where
  pretty (FGObj  d) = pretty d
  pretty (FGTag  d) = pretty d
  pretty (FGType d) = pretty d

flatGlobal :: GlobalDecls -> [(SUERef, FlatGlobalDecl)]
flatGlobal gmap = theTags ++ theObjs ++ theTypeDefs
  where
    theTags     = Map.assocs $ Map.map FGTag  $ gTags gmap
    theObjs     = Map.assocs $ Map.map FGObj  $ Map.mapKeys NamedRef $ gObjs gmap
    theTypeDefs = Map.assocs $ Map.map FGType $ Map.mapKeys NamedRef $ gTypeDefs gmap

sortFlatGlobal :: [(SUERef, FlatGlobalDecl)] -> [(SUERef, FlatGlobalDecl)]
sortFlatGlobal = sortBy order
  where
    order :: (SUERef, FlatGlobalDecl) -> (SUERef, FlatGlobalDecl) -> Ordering
    order (_, a) (_, b) = nodeInfo a `compare` nodeInfo b

class MyPretty p where
  myPretty     :: p -> Doc
  myPrettyPrec :: Int -> p -> Doc
  myPretty       = myPrettyPrec 0
  myPrettyPrec _ = myPretty

instance MyPretty GlobalDecls where
  myPretty _ = text ""
