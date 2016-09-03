{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Language.C2ATS.Pretty
       ( atsPrettyGlobal
       , flatGlobal
       , sortFlatGlobal
       , splitFlatGlobal
       , preDefineGlobal
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

splitFlatGlobal :: [(SUERef, FlatGlobalDecl)] -> [(Maybe FilePath, [(SUERef, FlatGlobalDecl)])]
splitFlatGlobal m = map (\a -> (a, filter (\b -> a == getFile b) m)) $ filePaths m
  where
    f :: String -> Maybe FilePath
    f "<no file>"  = Nothing
    f "<builtin>"  = Nothing
    f "<internal>" = Nothing
    f xs           = Just $ filter (\c -> c /= '"' && c /= ':' && c /= '(') . head . words $ xs
    getFile :: (SUERef, FlatGlobalDecl) -> Maybe FilePath
    getFile = f . show . posOfNode . nodeInfo . snd
    filePaths :: [(SUERef, FlatGlobalDecl)] -> [Maybe FilePath]
    filePaths = nub . map getFile

predef_c2ats_gnuc_va_list = text "predef_c2ats_gnuc_va_list"
predef_c2ats_any          = text "predef_c2ats_any"

preDefineGlobal :: Doc
preDefineGlobal =
  text "abst@ype" <+> predef_c2ats_gnuc_va_list $+$ -- can't use in ATS
  text "abst@ype" <+> predef_c2ats_any              -- can't use in ATS

atsPrettyGlobal :: [(SUERef, FlatGlobalDecl)] -> Doc
atsPrettyGlobal m = (vcat . map f $ m)
  where
    f :: (SUERef, FlatGlobalDecl) -> Doc
    f (_, d) = atsPretty (Map.fromList m) d

--------------------------------------------------------------------------------
type AtsPrettyMap = Map SUERef FlatGlobalDecl

class AtsPretty p where
  atsPretty     :: AtsPrettyMap -> p -> Doc
  atsPrettyPrec :: AtsPrettyMap -> Int -> p -> Doc
  atsPretty     m   = atsPrettyPrec m 0
  atsPrettyPrec m _ = atsPretty m

instance AtsPretty FlatGlobalDecl where
  atsPretty m (FGObj  d) = atsPretty m d
  atsPretty m (FGTag  d) = atsPretty m d
  atsPretty m (FGType d) = atsPretty m d

instance AtsPretty TypeDef where
  atsPretty m (TypeDef ident ty _ _) =
    text "typedef type" <> atsPretty m ident <+> text "=" <+> atsPretty m ty

instance AtsPretty Ident where
  atsPretty m (Ident s _ _) = text "_c2ats_" <> text s

instance AtsPretty Type where
  atsPretty m (DirectType t _ _)  = atsPretty m t
  atsPretty m (PtrType (FunctionType t _) _ _) = atsPretty m t
  atsPretty m (PtrType (DirectType TyVoid _ _) _ _) = text "ptr"
  atsPretty m (PtrType t _ _)     = text "cPtr0(" <> atsPretty m t <> text ")"
  atsPretty m (ArrayType t s _ _) = text "@[" <> atsPretty m t <> text "][" <> atsPretty m s <> text "]"
  atsPretty m (FunctionType f _)  = atsPretty m f
  atsPretty m (TypeDefType t _ _) = text "type" <> atsPretty m t

instance AtsPretty TypeName where
  atsPretty m TyVoid                       = text "void"
  atsPretty m (TyComp (CompTypeRef s c _)) = atsPretty m c <> atsPretty m s
  atsPretty m (TyEnum e)                   = atsPretty m e
  atsPretty m (TyComplex _) =
    let msg = text "Not support TyComplex"
    in trace ("*** " ++ show msg) $ text "(*" <+> msg <+> text "*)"
  atsPretty m (TyBuiltin TyVaList) = predef_c2ats_gnuc_va_list
  atsPretty m (TyBuiltin TyAny)    = predef_c2ats_any
  atsPretty m (TyIntegral t) = f t
    where
      f TyBool    = text "bool"
      f TyChar    = text "char"
      f TySChar   = text "schar"
      f TyUChar   = text "uchar"
      f TyShort   = text "sint"
      f TyUShort  = text "usint"
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
  atsPretty m (ArraySize _ e)      = atsPretty m e

instance AtsPretty TypeDefRef where
  atsPretty m (TypeDefRef ident _ _) = atsPretty m ident

instance AtsPretty FunType where
  atsPretty m (FunTypeIncomplete t) = text "() ->" <+> atsPretty m t
  atsPretty m (FunType t ps _) = addrs <> text "(" <> views <> args <> text ")" <+> text "->" <+> atsPretty m t
    where -- xxx Should follow pointer of pointer
      isViewPointer :: Type -> Bool
      isViewPointer (PtrType (FunctionType _ _) _ _)      = False
      isViewPointer (PtrType (DirectType TyVoid _ _) _ _) = False
      isViewPointer (PtrType _ _ _)                       = True
      isViewPointer _                                     = False
      unViewPointer :: Type -> Type
      unViewPointer (PtrType (FunctionType t _) _ _)      = undefined
      unViewPointer (PtrType (DirectType TyVoid _ _) _ _) = undefined
      unViewPointer (PtrType t _ _)                       = t
      unViewPointer _                                     = undefined
      argf :: AtsPrettyMap -> ParamDecl -> (Int, [Doc]) -> (Int, [Doc])
      argf m (ParamDecl (VarDecl _ _ ty) _) (n, ps) | isViewPointer ty =
        (n + 1, ps ++ [text "ptr l" <> int n])
      argf m p (n, ps) = (n, ps ++ [atsPretty m p])
      args = hcat $ punctuate (text ", ") $ snd (foldr (argf m) (1, []) ps)
      addrf :: AtsPrettyMap -> ParamDecl -> (Int, [Doc]) -> (Int, [Doc])
      addrf m (ParamDecl (VarDecl _ _ ty) _) (n, ps) | isViewPointer ty =
        (n + 1, ps ++ [text "l" <> int n])
      addrf m (AbstractParamDecl (VarDecl _ _ ty) _) (n, ps) | isViewPointer ty =
        (n + 1, ps ++ [text "l" <> int n])
      addrf m p l = l
      addrs = let a = snd (foldr (addrf m) (1, []) ps)
              in if null a then empty
                 else text "{" <> (hcat $ punctuate (text ",") a) <> text ":addr} "
      viewf :: AtsPrettyMap -> ParamDecl -> (Int, [Doc]) -> (Int, [Doc])
      viewf m (ParamDecl (VarDecl _ _ ty) _) (n, ps) | isViewPointer ty =
        (n + 1, ps ++ [atsPretty m (unViewPointer ty) <+> text "@ l" <> int n])
      viewf m (AbstractParamDecl (VarDecl _ _ ty) _) (n, ps) | isViewPointer ty =
        (n + 1, ps ++ [atsPretty m (unViewPointer ty) <+> text "@ l" <> int n])
      viewf m p l = l
      views = let v = snd (foldr (viewf m) (1, []) ps)
              in if null v then empty
                 else text "" <> (hcat $ punctuate (text ", ") v) <+> text "| "

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
      atsPretty' md = punctuate (text ",") $ delete empty . map (atsPretty m) $ md

instance AtsPretty MemberDecl where -- Ignore bit field
  atsPretty m (MemberDecl (VarDecl name declattrs ty) _ _) =
    pretty declattrs <+> pretty name <+> text "=" <+> atsPretty' m ty
    where
      atsPretty' :: AtsPrettyMap -> Type -> Doc
      atsPretty' m (PtrType (FunctionType t _) _ _) = atsPretty m t
      atsPretty' m (PtrType t _ _) = text "ptr (* cPtr0(" <> atsPretty m t <> text ") *)"
      atsPretty' m t = atsPretty m t
  atsPretty m (AnonBitField ty _ _) = empty -- Ignore AnonBitField

instance AtsPretty IdentDecl where
  atsPretty m (Declaration (Decl (VarDecl (VarName ident _) _ (FunctionType ty _)) _)) =
    text "fun fun" <> atsPretty m ident <> text ":" <+> atsPretty m ty <+> text "= \"mac#" <> pretty ident <> text "\""
  atsPretty m (Declaration (Decl (VarDecl (VarName ident _) _ ty) _)) =
    text "macdef extval" <> atsPretty m ident <+> text "= $extval(" <> atsPretty m ty <> text ", \"" <> pretty ident <> text "\")"
  atsPretty m (ObjectDef (ObjDef (VarDecl (VarName ident _) _ ty) _ _)) =
    text "macdef extval" <> atsPretty m ident <+> text "= $extval(" <> atsPretty m ty <> text ", \"" <> pretty ident <> text "\")"
  atsPretty m (FunctionDef (FunDef (VarDecl (VarName ident _) _ ty) _ _)) =
    text "fun fun" <> atsPretty m ident <> text ":" <+> atsPretty m ty <+> text "= \"mac#" <> pretty ident <> text "\""
  atsPretty m (EnumeratorDef (Enumerator i e _ _)) =
    text "#define enum" <> atsPretty m i <+> pretty e

maybeP :: (p -> Doc) -> Maybe p -> Doc
maybeP = maybe empty

identP :: Ident -> Doc
identP = text . identToString

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec prec prec2 t = if prec <= prec2 then t else parens t

instance AtsPretty CExpr where
  atsPrettyPrec m p (CComma exprs _) =
    parenPrec p (-1) $ hsep (punctuate comma (map (atsPrettyPrec m 2) exprs))
  atsPrettyPrec m p (CAssign op expr1 expr2 _) =
    parenPrec p 2 $ atsPrettyPrec m 3 expr1 <+> pretty op <+> atsPrettyPrec m 2 expr2
  atsPrettyPrec m p (CCond expr1 expr2 expr3 _) =
    parenPrec p 2 $ atsPrettyPrec m 4 expr1 <+> text "?" -- NB: assignment only has a higher precedence if cond is on the rhs
    <+> maybeP pretty expr2 <+> text ":" <+> atsPrettyPrec m 4 expr3
  atsPrettyPrec m p (CBinary op expr1 expr2 _) =
    let prec = binPrec op
    in  parenPrec p prec $ atsPrettyPrec m prec expr1
        <+> pretty op <+> atsPrettyPrec m (prec + 1) expr2
  atsPrettyPrec m p (CCast decl expr _) =
    parenPrec p 25 $ text "(" <> pretty decl <> text ")"
    <+> atsPrettyPrec m 25 expr
  atsPrettyPrec m p (CUnary CPostIncOp expr _) =
    parenPrec p 26 $ atsPrettyPrec m 26 expr <> text "++"
  atsPrettyPrec m p (CUnary CPostDecOp expr _) =
    parenPrec p 26 $ atsPrettyPrec m 26 expr <> text "--"
  atsPrettyPrec m p (CUnary op expr@(CUnary _ _ _) _) =
    --                             parens aren't necessary, but look nicer imho
    parenPrec p 25 $ pretty op <+> parens (atsPrettyPrec m 25 expr)
  atsPrettyPrec m p (CUnary op expr _) =
    parenPrec p 25 $ pretty op <> atsPrettyPrec m 25 expr
  atsPrettyPrec m p (CSizeofExpr expr _) =
    parenPrec p 25 $ text "sizeof" <> parens (pretty expr)
  atsPrettyPrec m p (CSizeofType decl _) =
    parenPrec p 25 $ text "sizeof" <> parens (atsPretty m decl)
  atsPrettyPrec m p (CAlignofExpr expr _) =
    parenPrec p 25 $ text "__alignof" <> parens (pretty expr)
  atsPrettyPrec m p (CAlignofType decl _) =
    parenPrec p 25 $ text "__alignof" <> parens (pretty decl)
  atsPrettyPrec m p (CComplexReal expr _) =
    parenPrec p 25 $ text "__real" <+> atsPrettyPrec m 25 expr
  atsPrettyPrec m p (CComplexImag expr _) =
    parenPrec p 25 $ text "__imag" <+> atsPrettyPrec m 25 expr
  atsPrettyPrec m p (CIndex expr1 expr2 _) =
    parenPrec p 26 $ atsPrettyPrec m 26 expr1
    <> text "[" <> pretty expr2 <> text "]"
  atsPrettyPrec m p (CCall expr args _) =
    parenPrec p 30 $ atsPrettyPrec m 30 expr <> text "("
    <> (sep . punctuate comma . map pretty) args <> text ")"
  atsPrettyPrec m p (CMember expr ident deref _) =
    parenPrec p 26 $ atsPrettyPrec m 26 expr
    <> text (if deref then "->" else ".") <> identP ident
  atsPrettyPrec m _p (CVar ident _) = identP ident
  atsPrettyPrec m _p (CConst constant) = pretty constant
  atsPrettyPrec m _p (CCompoundLit decl initl _) =
    parens (pretty decl) <+> (braces . hsep . punctuate comma) (map p initl) where
      p ([], initializer)           = pretty initializer
      p (mems, initializer) = hcat (punctuate (text ".") (map pretty mems)) <+> text "=" <+> pretty initializer
  atsPrettyPrec m _p (CStatExpr stat _) =
    text "(" <> pretty stat <> text ")"
  atsPrettyPrec m _p (CLabAddrExpr ident _) = text "&&" <> identP ident
  atsPrettyPrec m _p (CBuiltinExpr builtin) = pretty builtin

binPrec :: CBinaryOp -> Int
binPrec CMulOp = 20
binPrec CDivOp = 20
binPrec CRmdOp = 20
binPrec CAddOp = 19
binPrec CSubOp = 19
binPrec CShlOp = 18
binPrec CShrOp = 18
binPrec CLeOp  = 17
binPrec CGrOp  = 17
binPrec CLeqOp = 17
binPrec CGeqOp = 17
binPrec CEqOp  = 16
binPrec CNeqOp = 16
binPrec CAndOp = 15
binPrec CXorOp = 14
binPrec COrOp  = 13
binPrec CLndOp = 12
binPrec CLorOp = 11

instance AtsPretty CDecl where
 atsPretty m (CDecl [CTypeSpec t] _ _)= atsPretty m t
 atsPretty m t = pretty t

instance AtsPretty CTypeSpec where
  atsPretty m (CVoidType _)        = text "ptr"
  atsPretty m (CCharType _)        = text "char"
  atsPretty m (CShortType _)       = text "short"
  atsPretty m (CIntType _)         = text "int"
  atsPretty m (CFloatType _)       = text "float"
  atsPretty m (CDoubleType _)      = text "double"
  atsPretty m (CBoolType _)        = text "bool"
  atsPretty m (CEnumType enum _)   = text "int"
  atsPretty m (CTypeDef ident _)   = identP ident

--------------------------------------------------------------------------------
class CPretty p where
  cPretty     :: AtsPrettyMap -> p -> Doc
  cPrettyPrec :: AtsPrettyMap -> Int -> p -> Doc
  cPretty     m   = cPrettyPrec m 0
  cPrettyPrec m _ = cPretty m

subscriptArray :: AtsPrettyMap -> Type -> Doc
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
  cPretty m (PtrType t _ _)     = cPretty m t <> text "*"
  cPretty m (ArrayType t _ _ _) = cPretty m t
  cPretty m (TypeDefType t _ _) = cPretty m t
  -- Do not print function type directly

instance CPretty TypeName where
  cPretty m TyVoid                       = text "void"
  cPretty m (TyComp (CompTypeRef s c _)) = cPretty m c <+> cPretty m s
  cPretty m (TyEnum e)                   = cPretty m e
  cPretty m t@(TyComplex _) =
    let msg = text "Not support TyComplex"
    in trace ("*** " ++ show msg) $ text "/*" <+> msg <+> text "*/"
  cPretty m (TyBuiltin TyVaList) = text "__gnuc_va_list" -- gcc specific
  cPretty m (TyBuiltin TyAny) =
    let msg = text "Not support TyBuiltin TyAny"
    in trace ("*** " ++ show msg) $ text "2/*" <+> msg <+> text "*/"
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

prettyCParamFun :: AtsPrettyMap -> Ident -> Type -> [ParamDecl] -> Doc
prettyCParamFun m n ty para =
  cPretty m ty <+> cPretty m n <> parens (hcat $ punctuate (text ", ") $ map (cPretty m) para)
prettyCParam :: AtsPrettyMap -> Ident -> Type -> Doc
prettyCParam m n ty = cPretty m ty <+> cPretty m n <> subscriptArray m ty
prettyCParamNoname :: AtsPrettyMap -> Type -> Doc
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
