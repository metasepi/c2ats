module Language.C2ATS.FlatGlobalDecl
       ( FlatGlobalDecl (..)
       , FlatG (..)
       , MapFlatG (..)
       , noposSueref
       ) where

import Data.Map (Map)

import Language.C
import Language.C.Analysis

data FlatGlobalDecl = FGObj  IdentDecl
                    | FGTag  TagDef
                    | FGType TypeDef
                    | FGRaw  (String, NodeInfo)

instance CNode FlatGlobalDecl where
  nodeInfo (FGObj  d)      = nodeInfo d
  nodeInfo (FGTag  d)      = nodeInfo d
  nodeInfo (FGType d)      = nodeInfo d
  nodeInfo (FGRaw  (_, n)) = n

type FlatG = (SUERef, FlatGlobalDecl)
type MapFlatG = Map (Maybe FilePath) [FlatG]

noposSueref :: String -> SUERef
noposSueref s = NamedRef $ mkIdent nopos s $ Name (-1)
