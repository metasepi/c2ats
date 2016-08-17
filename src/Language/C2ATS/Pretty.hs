module Language.C2ATS.Pretty
       ( atsPrettyGlobal
       , flatGlobal
       , sortFlatGlobal
       ) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ
import Debug.Trace {- for warnings -}

import Language.C
import Language.C.Analysis
import Language.C.Analysis.Export
import Language.C.Data.Ident

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

atsPrettyGlobal :: [(SUERef, FlatGlobalDecl)] -> Doc
atsPrettyGlobal = vcat . map f
  where
    f :: (SUERef, FlatGlobalDecl) -> Doc
    f (_, d) = atsPretty Map.empty d

class AtsPretty p where
  atsPretty     :: Map Ident Type -> p -> Doc
  atsPrettyPrec :: Map Ident Type -> Int -> p -> Doc
  atsPretty     m   = atsPrettyPrec m 0
  atsPrettyPrec m _ = atsPretty m

instance AtsPretty FlatGlobalDecl where
  atsPretty m (FGObj  d) = pretty d -- xxx not yet
  atsPretty m (FGTag  d) = pretty d -- xxx not yet
  atsPretty m (FGType d) = atsPretty m d

instance AtsPretty TypeDef where
  atsPretty m (TypeDef ident ty _ _) =
    text "typedef" <+> atsPretty m ident <+> text "=" <+> atsPretty m ty

instance AtsPretty Ident where
  atsPretty m (Ident s _ _) = text "c2ats_" <> text s

instance AtsPretty Type where
  atsPretty m (DirectType t _ _)  = atsPretty m t
  atsPretty m (PtrType t _ _)     = text "cPtr0(" <> atsPretty m t <> text ")"
  atsPretty m (ArrayType t s _ _) = text "@[" <> atsPretty m t <> text "][" <> atsPretty m s <> text "]"
  atsPretty m (FunctionType f _)  = text "xxx not yet FunctionType xxx"
  atsPretty m (TypeDefType t _ _) = text "xxx not yet TypeDefType xxx"

instance AtsPretty TypeName where
  atsPretty m TyVoid                       = text "void"
  atsPretty m (TyComp (CompTypeRef s c _)) = atsPretty m c <> atsPretty m s
  atsPretty m (TyEnum e)                   = atsPretty m e
  atsPretty m (TyComplex _)  = trace "*** Complex type is not suppored" $ text "((* NOT SUPPORT *))"
  atsPretty m (TyBuiltin _)  = trace "*** Builtin type is not suppored" $ text "((* NOT SUPPORT *))"
  atsPretty m (TyIntegral t) = f t
    where
      f TyBool    = text "bool"
      f TyChar    = text "char"
      f TySChar   = text "schar"
      f TyUChar   = text "uchar"
      f TyShort   = trace "*** \"short\" not implemented in ATS" $ text "short"
      f TyUShort  = trace "*** \"ushort\" not implemented in ATS" $ text "ushort"
      f TyInt     = text "int"
      f TyUInt    = text "uint"
      f TyInt128  = trace "*** \"int128\" not implemented in ATS" $ text "int128"
      f TyUInt128 = trace "*** \"uint128\" not implemented in ATS" $ text "uint128"
      f TyLong    = text "lint"
      f TyULong   = text "ulint"
      f TyLLong   = text "llint"
      f TyULLong  = text "ullint"
  atsPretty m (TyFloating t) = f t
    where
      f TyFloat   = text "float"
      f TyDouble  = text "double"
      f TyLDouble = text "ldouble"

instance AtsPretty CompTyKind where
  atsPretty m StructTag = text "struct"
  atsPretty m UnionTag  = text "union"

instance AtsPretty SUERef where
  atsPretty m (AnonymousRef name) = text $ "_anon_" ++ show (nameId name)
  atsPretty m (NamedRef ident) = atsPretty m ident

instance AtsPretty EnumTypeRef where
  atsPretty m (EnumTypeRef _ _ ) = text "int" -- ATS does not have enum

instance AtsPretty ArraySize where
  atsPretty m (UnknownArraySize _) = text "0" -- ATS does not support UnknownArraySize
  atsPretty m (ArraySize _ e)      = pretty e
