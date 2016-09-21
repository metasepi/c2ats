module Language.C2ATS.FlatGlobalDecl
       ( FlatGlobalDecl (..)
       , FlatG (..)
       , noposSueref
       ) where

import Language.C
import Language.C.Analysis

data FlatGlobalDecl = FGObj  IdentDecl
                    | FGTag  TagDef
                    | FGType TypeDef
                    | FGRaw  String

instance CNode FlatGlobalDecl where
  nodeInfo (FGObj  d) = nodeInfo d
  nodeInfo (FGTag  d) = nodeInfo d
  nodeInfo (FGType d) = nodeInfo d
  nodeInfo (FGRaw  _) = undefNode

type FlatG = (SUERef, FlatGlobalDecl)

noposSueref :: String -> SUERef
noposSueref s = NamedRef $ mkIdent nopos s $ Name (-1)
