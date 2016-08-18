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
sortFlatGlobal = sortBy order -- xxx swap anon {struct,union}
  where
    order :: (SUERef, FlatGlobalDecl) -> (SUERef, FlatGlobalDecl) -> Ordering
    order (_, a) (_, b) = nodeInfo a `compare` nodeInfo b

atsPrettyGlobal :: [(SUERef, FlatGlobalDecl)] -> Doc
atsPrettyGlobal m = vcat . map f $ m
  where
    f :: (SUERef, FlatGlobalDecl) -> Doc
    f (_, d) = atsPretty (Map.fromList m) d

class AtsPretty p where
  atsPretty     :: Map SUERef FlatGlobalDecl -> p -> Doc
  atsPrettyPrec :: Map SUERef FlatGlobalDecl -> Int -> p -> Doc
  atsPretty     m   = atsPrettyPrec m 0
  atsPrettyPrec m _ = atsPretty m

instance AtsPretty FlatGlobalDecl where
  atsPretty m (FGObj  d) = pretty d -- xxx not yet
  atsPretty m (FGTag  d) = atsPretty m d
  atsPretty m (FGType d) = atsPretty m d

instance AtsPretty TypeDef where
  atsPretty m (TypeDef ident ty _ _) =
    text "typedef" <+> atsPretty m ident <+> text "=" <+> atsPretty m ty

instance AtsPretty Ident where
  atsPretty m (Ident s _ _) = text "_c2ats_" <> text s

instance AtsPretty Type where
  atsPretty m (DirectType t _ _)  = atsPretty m t
  atsPretty m (PtrType (FunctionType t _) _ _) = atsPretty m t
  atsPretty m (PtrType (DirectType TyVoid _ _) _ _) = text "ptr"
  atsPretty m (PtrType t _ _)     = text "cPtr0(" <> atsPretty m t <> text ")"
  atsPretty m (ArrayType t s _ _) = text "@[" <> atsPretty m t <> text "][" <> atsPretty m s <> text "]"
  atsPretty m (FunctionType f _)  = atsPretty m f
  atsPretty m (TypeDefType t _ _) = atsPretty m t

instance AtsPretty TypeName where
  atsPretty m TyVoid                       = text "void"
  atsPretty m (TyComp (CompTypeRef s c _)) = atsPretty m c <> atsPretty m s
  atsPretty m (TyEnum e)                   = atsPretty m e
  atsPretty m (TyComplex _)  = trace "*** TyComplex type is not suppored" $ text "(* Not support TyComplex *)"
  atsPretty m (TyBuiltin _)  = trace "*** TyBuiltin type is not suppored" $ text "(* Not support TyBuiltin *)"
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
  atsPretty m (AnonymousRef name) = text "_c2ats_anon_" <> int (nameId name)
  atsPretty m (NamedRef ident) = atsPretty m ident

instance AtsPretty EnumTypeRef where
  atsPretty m (EnumTypeRef _ _ ) = text "int" -- ATS does not have enum

instance AtsPretty ArraySize where
  atsPretty m (UnknownArraySize _) = text "0" -- ATS does not support UnknownArraySize
  atsPretty m (ArraySize _ e)      = pretty e

instance AtsPretty TypeDefRef where
  atsPretty m (TypeDefRef ident _ _) = atsPretty m ident

instance AtsPretty FunType where
  atsPretty m (FunTypeIncomplete _) = trace "*** FunTypeIncomplete is not suppored" $ text "(* Not support FunTypeIncomplete *)"
  atsPretty m (FunType t ps _) = text "(" <> args <> text ")" <+> text "->" <+> atsPretty m t
    where
      x:xs = map (atsPretty m) ps
      args = hcat $ x : map (\a -> text "," <+> a) xs

instance AtsPretty ParamDecl where
  atsPretty m (ParamDecl (VarDecl _ _ ty) _)      = atsPretty m ty
  atsPretty m (AbstractParamDecl (VarDecl _ _ ty) _) = atsPretty m ty

instance AtsPretty TagDef where
  atsPretty m (CompDef compty) = atsPretty m compty
  atsPretty m (EnumDef enumty) = empty -- ATS does not have enum

instance AtsPretty CompType where
  atsPretty m (CompType sue_ref tag members attrs node) =
    tdef <> ext sue_ref <> text "\" of {" $+$ (nest 2 $ vcat (atsPretty' members)) $+$ text "}"
    where
      tdef = text "typedef" <+> atsPretty m tag <> atsPretty m sue_ref <+> text "= $extype_struct\""
      ext (NamedRef ident) = atsPretty m tag <+> pretty ident
      ext (AnonymousRef _) = atsPretty m tag <+> text "{" <+> hcat (map (cPretty m) members) <> text "}"
      atsPretty' :: [MemberDecl] -> [Doc]
      atsPretty' [] = [empty]
      atsPretty' [x] = [atsPretty m x]
      atsPretty' (x:xs) = atsPretty m x <> text "," : atsPretty' xs

instance AtsPretty MemberDecl where -- Ignore bit field
  atsPretty m (MemberDecl (VarDecl name declattrs ty) _ _) =
    pretty declattrs <+> pretty name <+> text "=" <+> atsPretty m ty
  atsPretty m (AnonBitField ty _ _) = empty -- Ignore AnonBitField

class CPretty p where
  cPretty     :: Map SUERef FlatGlobalDecl -> p -> Doc
  cPrettyPrec :: Map SUERef FlatGlobalDecl -> Int -> p -> Doc
  cPretty     m   = cPrettyPrec m 0
  cPrettyPrec m _ = cPretty m

subscriptArray :: Map SUERef FlatGlobalDecl -> Type -> Doc
subscriptArray m (ArrayType t s _ _) = (brackets $ atsPretty m s) <> subscriptArray m t
subscriptArray m t = empty

instance CPretty MemberDecl where
  cPretty m (MemberDecl (VarDecl name _ (PtrType (FunctionType (FunType ty para _) _) _ _)) _ _) =
    cPretty m ty <+> text "(*" <> pretty name <> text ")(" <> hcat (punctuate (text ", ") $ map (cPretty m) para) <> text ")" <> text "; "
  cPretty m (MemberDecl (VarDecl name declattrs ty) bitfield _) =
    pretty declattrs <+> cPretty m ty <+> pretty name <> subscriptArray m ty <+>
    (maybe empty (\bf -> text ":" <+> pretty bf) bitfield) <> text "; "
  cPretty m (AnonBitField ty bitfield_sz _) =
    cPretty m ty <+> text ":" <+> pretty bitfield_sz <> text "; "

instance CPretty Type where
  cPretty m (DirectType t _ _)  = cPretty m t
  cPretty m (PtrType t _ _)     = cPretty m t <+> text "*"
  cPretty m (ArrayType t _ _ _) = cPretty m t
  cPretty m (TypeDefType t _ _) = cPretty m t
  -- Do not print function type directly

instance CPretty TypeName where
  cPretty m TyVoid                       = text "void"
  cPretty m (TyComp (CompTypeRef s c _)) = cPretty m c <+> cPretty m s
  cPretty m (TyEnum e)                   = cPretty m e
  cPretty m (TyComplex _)  = trace "*** TyComplex type is not suppored" $ text "/* Not support TyComplex */"
  cPretty m (TyBuiltin _)  = trace "*** TyBuiltin type is not suppored" $ text "/* Not support TyBuiltin */"
  cPretty m (TyIntegral t) = f t
    where
      f TyBool    = text "bool"
      f TyChar    = text "char"
      f TySChar   = text "signed char"
      f TyUChar   = text "unsigned char"
      f TyShort   = text "short"
      f TyUShort  = text "unsigned short"
      f TyInt     = text "int"
      f TyUInt    = text "unsigned int"
      f TyInt128  = text "int128_t"
      f TyUInt128 = text "uint128_t"
      f TyLong    = text "long int"
      f TyULong   = text "unsigned long int"
      f TyLLong   = text "long long int"
      f TyULLong  = text "unsigned long long int"
  cPretty m (TyFloating t) = f t
    where
      f TyFloat   = text "float"
      f TyDouble  = text "double"
      f TyLDouble = text "long double"

instance CPretty CompTyKind where
  cPretty m StructTag = text "struct"
  cPretty m UnionTag  = text "union"

instance CPretty SUERef where
  cPretty m (AnonymousRef name) = text "$" <> int (nameId name)
  cPretty m (NamedRef ident) = cPretty m ident

instance CPretty EnumTypeRef where
  cPretty m (EnumTypeRef _ _ ) = text "int" -- ATS does not have enum

instance CPretty Ident where
  cPretty m (Ident s _ _) = text s

instance CPretty TypeDefRef where
  cPretty m (TypeDefRef ident _ _) = cPretty m ident

prettyCParamFun :: Map SUERef FlatGlobalDecl -> Ident -> Type -> [ParamDecl] -> Doc
prettyCParamFun m n ty para =
  cPretty m ty <+> cPretty m n <> parens (hcat $ punctuate (text ", ") $ map (cPretty m) para)
prettyCParam :: Map SUERef FlatGlobalDecl -> Ident -> Type -> Doc
prettyCParam m n ty = cPretty m ty <+> cPretty m n <> subscriptArray m ty
prettyCParamNoname :: Map SUERef FlatGlobalDecl -> Type -> Doc
prettyCParamNoname m ty = cPretty m ty <> subscriptArray m ty

instance CPretty ParamDecl where
  cPretty m (ParamDecl (VarDecl (VarName n _) _ (FunctionType (FunType ty para _) _)) _) =
    prettyCParamFun m n ty para
  cPretty m (AbstractParamDecl (VarDecl (VarName n _) _ (FunctionType (FunType ty para _) _)) _) =
    prettyCParamFun m n ty para
  cPretty m (ParamDecl (VarDecl (VarName n _) _ ty) _) =
    prettyCParam m n ty
  cPretty m (AbstractParamDecl (VarDecl (VarName n _) _ ty) _) =
    prettyCParam m n ty
  cPretty m (ParamDecl (VarDecl NoName _ ty) _) =
    prettyCParamNoname m ty
  cPretty m (AbstractParamDecl (VarDecl NoName _ ty) _) =
    prettyCParamNoname m ty
