{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Language.C2ATS.Pretty
       ( atsPrettyGlobal
       , flatGlobal
       , injectForwardDecl
       , sortFlatGlobal
       , injectIncludes
       , preDefineGlobal
       , AtsPretty (..)
       , CPretty (..)
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
                    | FGRaw  String

instance CNode FlatGlobalDecl where
  nodeInfo (FGObj  d) = nodeInfo d
  nodeInfo (FGTag  d) = nodeInfo d
  nodeInfo (FGType d) = nodeInfo d
  nodeInfo (FGRaw  _) = undefNode

instance Pretty FlatGlobalDecl where
  pretty (FGObj  d) = pretty d
  pretty (FGTag  d) = pretty d
  pretty (FGType d) = pretty d
  pretty (FGRaw  s) = text s

type FlatG = (SUERef, FlatGlobalDecl)

flatGlobal :: GlobalDecls -> [FlatG]
flatGlobal gmap = theTags ++ theObjs ++ theTypeDefs
  where
    theTags     = Map.assocs $ Map.map FGTag  $ gTags gmap
    theObjs     = Map.assocs $ Map.map FGObj  $ Map.mapKeys NamedRef $ gObjs gmap
    theTypeDefs = Map.assocs $ Map.map FGType $ Map.mapKeys NamedRef $ gTypeDefs gmap

injectIncludes :: [String] -> [FlatG] -> [FlatG]
injectIncludes noincs m =
  concat . map (\a -> incl a $ filter (\b -> a == getFile b) m) $ filePaths m
  where
    f :: String -> Maybe FilePath
    f "<no file>"  = Nothing
    f "<builtin>"  = Nothing
    f "<internal>" = Nothing
    f xs           = Just $ filter (\c -> c /= '"' && c /= ':' && c /= '(') . head . words $ xs
    getFile :: FlatG -> Maybe FilePath
    getFile = f . show . posOfNode . nodeInfo . snd
    filePaths :: [FlatG] -> [Maybe FilePath]
    filePaths = nub . map getFile
    incl :: Maybe String -> [FlatG] -> [FlatG]
    incl (Nothing) fgs@((s,_):_) = (s, FGRaw $ "// No file"):fgs
    incl (Just n) fgs@((s,_):_) | not . or $ map (flip isPrefixOf n) noincs =
      (s, FGRaw $ init $ unlines [
          "// File: " ++ n,
          "%{#",
          "#include \"" ++ n ++ "\"",
          "%}"
          ]):fgs
    incl (Just n) fgs@((s,_):_)  = (s, FGRaw $ "// File: " ++ n):fgs

predef_c2ats_gnuc_va_list = "type_c2ats___gnuc_va_list"
predef_c2ats_any          = "type_c2ats___any"

preDefineGlobal :: Doc
preDefineGlobal =
  text "abst@ype" <+> text predef_c2ats_gnuc_va_list $+$ -- can't use in ATS
  text "abst@ype" <+> text predef_c2ats_any $+$          -- can't use in ATS
  text (init $ unlines [
           "viewdef ptr_v_1 (a:t@ype, l:addr) = a @ l",
           "dataview ptr_v_2 (a:t@ype+, l0: addr, l1: addr) =",
           "  | ptr_v_2_cons(a, l0, l1) of (ptr l1 @ l0, ptr_v_1 (a, l1))",
           "dataview ptr_v_3 (a:t@ype+, l0:addr, l1:addr, l2:addr) =",
           "  | ptr_v_3_cons(a, l0, l1, l2) of (ptr l1 @ l0, ptr_v_2 (a, l1, l2))"
           -- Need dataview ptr_v_4, and more?
           ])

type AtsPrettyMap = Map SUERef FlatGlobalDecl

--------------------------------------------------------------------------------
sortFlatGlobal :: [FlatG] -> [FlatG]
sortFlatGlobal = (\(a,_,_,b) -> a ++ b) . foldl go ([], Map.empty, Map.empty, []) . sortBy order
  where
    order :: FlatG -> FlatG -> Ordering
    order (_, a) (_, b) = nodeInfo a `compare` nodeInfo b
    go :: ([FlatG], Map Int (), Map Int (), [FlatG]) -> FlatG ->
          ([FlatG], Map Int (), Map Int (), [FlatG])
    go (out, knowns, deps, ks) fg@(s,_) =
      let knowns' = Map.insert (nodeSUERef s) () knowns
          deps'   = Map.difference (Map.union deps $ anons fg) knowns'
          out'    = out ++ if Map.null deps' then fg : ks else []
          ks'     = if Map.null deps' then [] else fg : ks
      in (out', knowns', deps', ks')
    anons :: FlatG -> Map Int ()
    anons (_, g) = anonRefs g

nodeSUERef :: SUERef -> Int
nodeSUERef (AnonymousRef n)                        = nameId n
nodeSUERef (NamedRef (Ident _ _ (NodeInfo _ _ n))) = nameId n
nodeSUERef _                                       = -1

class AnonRefs p where
  anonRefs :: p -> Map Int ()

instance AnonRefs FlatGlobalDecl where
  anonRefs (FGObj  d) = anonRefs d
  anonRefs (FGTag  d) = anonRefs d
  anonRefs (FGType d) = anonRefs d

instance AnonRefs IdentDecl where
  anonRefs (Declaration (Decl v _))     = anonRefs v
  anonRefs (ObjectDef (ObjDef v _ _))   = anonRefs v
  anonRefs (FunctionDef (FunDef v _ _)) = anonRefs v
  anonRefs (EnumeratorDef _)            = Map.empty -- ATS does not have enum

instance AnonRefs MemberDecl where
  anonRefs (MemberDecl v _ _)   = anonRefs v
  anonRefs (AnonBitField t _ _) = anonRefs t

instance AnonRefs VarDecl where
  anonRefs (VarDecl _ _ t) = anonRefs t

instance AnonRefs TagDef where
  anonRefs (EnumDef _)                    = Map.empty  -- ATS does not have enum
  anonRefs (CompDef (CompType _ _ m _ _)) = Map.unions $ map anonRefs m

instance AnonRefs TypeDef where
  anonRefs (TypeDef _ t _ _) = anonRefs t

instance AnonRefs Type where
  anonRefs (PtrType t _ _)                             = anonRefs t
  anonRefs (ArrayType t _ _ _)                         = anonRefs t
  anonRefs (FunctionType ft _)                         = anonRefs ft
  anonRefs (DirectType tn _ _)                         = anonRefs tn
  anonRefs _                                           = Map.empty

instance AnonRefs TypeName where
  anonRefs (TyComp (CompTypeRef s _ _)) = anonRefs s
  anonRefs _                            = Map.empty

instance AnonRefs FunType where
  anonRefs (FunTypeIncomplete t) = anonRefs t
  anonRefs (FunType t p _)       =
    Map.union (anonRefs t) (Map.unions $ map anonRefs p)

instance AnonRefs ParamDecl where
  anonRefs (ParamDecl v _)         = anonRefs v
  anonRefs (AbstractParamDecl v _) = anonRefs v

instance AnonRefs SUERef where
  anonRefs s@(AnonymousRef _) = Map.singleton (nodeSUERef s) ()
  anonRefs (NamedRef _)       = Map.empty

--------------------------------------------------------------------------------
type IndentsMap = (Maybe String, Map String ())

injectForwardDecl :: [FlatG] -> [FlatG]
injectForwardDecl = fst . foldl f ([], Map.empty)
  where
    f :: ([FlatG], Map String ()) -> FlatG -> ([FlatG], Map String ())
    f (fgs, is) fg@(s,g) =
      let (i, is') = idents g
          knownis  = maybe is (\a -> Map.insert a () is) i
          fds      = forwardDecls $ Map.keys $ Map.difference is' knownis
          is''     = Map.union is' knownis
      in (fgs ++ fds ++ [fg], is'')

forwardDecls :: [String] -> [FlatG]
forwardDecls = map f
  where
    f i = (NamedRef $ mkIdent nopos i (Name (-1)), FGRaw $ "abst@ype " ++ i ++ " // FIXME! Forward declaration.")

identsAppend :: IndentsMap -> IndentsMap -> IndentsMap
identsAppend (i_a@(Just _), is_a) (i_b, is_b) = (i_a, Map.union is_a is_b)
identsAppend (Nothing, is_a) (i_b, is_b)      = (i_b, Map.union is_a is_b)

class Idents p where
  idents :: p -> IndentsMap

instance Idents FlatGlobalDecl where
  idents (FGObj  d) = idents d
  idents (FGTag  d) = idents d
  idents (FGType d) = idents d
  idents (FGRaw  _) = (Nothing, Map.empty)

instance Idents IdentDecl where
  idents (Declaration (Decl v _))             = idents v
  idents (ObjectDef (ObjDef v _ _))           = idents v
  idents (FunctionDef (FunDef v _ _))         = idents v
  idents _                                    = (Nothing, Map.empty)

instance Idents MemberDecl where
  idents (MemberDecl v _ _)   = idents v
  idents (AnonBitField t _ _) = idents t

instance Idents VarDecl where
  idents (VarDecl _ _ t) = idents t

instance Idents TagDef where
  idents (CompDef (CompType (NamedRef i) k m _ _)) =
    foldl (\a b -> a `identsAppend` idents b) (Just $ show k ++ "_c2ats_" ++ identToString i, Map.empty) m
  idents (CompDef (CompType (AnonymousRef _) _ m _ _)) =
    foldl (\a b -> a `identsAppend` idents b) (Nothing, Map.empty) m
  idents _                              = (Nothing, Map.empty)

instance Idents TypeDef where
  idents (TypeDef i t _ _) =
    (Just $ "type_c2ats_" ++ identToString i, Map.empty) `identsAppend` idents t

instance Idents Type where
  idents (PtrType t _ _)                               = idents t
  idents (ArrayType t _ _ _)                           = idents t
  idents (FunctionType ft _)                           = idents ft
  idents (DirectType (TyComp (CompTypeRef (NamedRef i) k _)) _ _) =
    (Nothing, Map.singleton (show k ++ "_c2ats_" ++ identToString i) ())
  idents (TypeDefType (TypeDefRef i _ _) _ _)          =
    (Nothing, Map.singleton ("type_c2ats_" ++ identToString i) ())
  idents _                                             = (Nothing, Map.empty)

instance Idents FunType where
  idents (FunTypeIncomplete t) = idents t
  idents (FunType t pd _)      =
    foldl (\a b -> a `identsAppend` idents b) (idents t) pd

instance Idents ParamDecl where
  idents (ParamDecl v _)         = idents v
  idents (AbstractParamDecl v _) = idents v

--------------------------------------------------------------------------------
atsPrettyGlobal :: [FlatG] -> Doc
atsPrettyGlobal m = (vcat . map f $ m)
  where
    f :: FlatG -> Doc
    f (_, d) = atsPretty (Map.fromList m) d

class AtsPretty p where
  atsPretty     :: AtsPrettyMap -> p -> Doc
  atsPrettyPrec :: AtsPrettyMap -> Int -> p -> Doc
  atsPretty     m   = atsPrettyPrec m 0
  atsPrettyPrec m _ = atsPretty m

instance AtsPretty FlatGlobalDecl where
  atsPretty m (FGObj  d) = atsPretty m d
  atsPretty m (FGTag  d) = atsPretty m d
  atsPretty m (FGType d) = atsPretty m d
  atsPretty m (FGRaw s)  = text s

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
  atsPretty m (TyBuiltin TyVaList) = text predef_c2ats_gnuc_va_list
  atsPretty m (TyBuiltin TyAny)    = text predef_c2ats_any
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
  atsPretty m (FunType t ps _) =
    addrs <> text "(" <> views <> args <> text ")" <+> text "->" <+> raddrs <+> ret
    where
      paramDeclType :: ParamDecl -> Type
      paramDeclType (ParamDecl (VarDecl _ _ ty) _)         = ty
      paramDeclType (AbstractParamDecl (VarDecl _ _ ty) _) = ty
      isViewPointer :: Type -> Bool
      isViewPointer (PtrType (FunctionType _ _) _ _)      = False
      isViewPointer (PtrType (DirectType TyVoid _ _) _ _) = False
      isViewPointer (PtrType _ _ _)                       = True
      isViewPointer _                                     = False
      isViewPointer' :: ParamDecl -> Bool
      isViewPointer' = isViewPointer . paramDeclType
      atviewToList :: AtsPrettyMap -> Type -> (Int, Int) -> [Doc] -> [Doc]
      atviewToList m (PtrType (FunctionType t _) _ _) _ d      = (atsPretty m t) : d
      atviewToList m (PtrType (DirectType TyVoid _ _) _ _) _ d = text "ptr" : d
      atviewToList m (PtrType t _ _) (i,j) d                       =
        let minor = if j == 0 then empty else text "_" <> int j
        in atviewToList m t (i,j+1) (d ++ [text "l" <> int i <> minor])
      atviewToList m t _ d                                     = (atsPretty m t) : d
      atviewShow :: AtsPrettyMap -> Type -> Int -> Doc
      atviewShow m t n =
        let l = atviewToList m t (n, 0) []
        in text "ptr_v_" <> int (length l - 1) <> text "(" <>
           (hcat $ punctuate (text ", ") l) <> text ")"
      argf :: AtsPrettyMap -> (Int, [Doc]) -> ParamDecl -> (Int, [Doc])
      argf m (n, ps) pd | isViewPointer' pd =
        (n + 1, ps ++ [text "ptr l" <> int n])
      argf m (n, ps) p = (n, ps ++ [atsPretty m p])
      args = hcat $ punctuate (text ", ") $ snd (foldl (argf m) (1, []) ps)
      addrf :: AtsPrettyMap -> (Int, [Doc]) -> ParamDecl -> (Int, [Doc])
      addrf m (n, ps) pd | isViewPointer' pd =
        (n + 1, ps ++ (tail $ atviewToList m (paramDeclType pd) (n, 0) []))
      addrf m l p = l
      addrs = let a = snd (foldl (addrf m) (1, []) ps)
              in if null a then empty
                 else text "{" <> (hcat $ punctuate (text ",") a) <> text ":addr} "
      viewf :: AtsPrettyMap -> (Int, [Doc]) -> ParamDecl -> (Int, [Doc])
      viewf m (n, ps) pd | isViewPointer' pd =
        (n + 1, ps ++ [text "!" <> atviewShow m (paramDeclType pd) n])
      viewf m l p = l
      views = let v = snd (foldl (viewf m) (1, []) ps)
              in if null v then empty
                 else text "" <> (hcat $ punctuate (text ", ") v) <+> text "| "
      ri = fst (foldl (addrf m) (1, []) ps)
      rviews = if not (isViewPointer t) then empty
               else atviewShow m t ri <+> text "| "
      raddrs = if not (isViewPointer t) then empty
               else text "[" <> (hcat $ punctuate (text ",") (tail $ atviewToList m t (ri,0) [])) <> text ":addr]"
      ret = if not (isViewPointer t) then atsPretty m t
            else text "(" <> rviews <> text "ptr l" <> int ri <> text ")"

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
    atsPrettyPrec m 25 expr
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
    parenPrec p 25 $ text "sizeof" <> parens (atsPretty m expr)
  atsPrettyPrec m p (CSizeofType decl _) =
    parenPrec p 25 $ text "sizeof" <> parens (atsPretty m decl)
  atsPrettyPrec m p (CAlignofExpr expr _) =
    parenPrec p 25 $ text "__alignof" <> parens (atsPretty m expr)
  atsPrettyPrec m p (CAlignofType decl _) =
    parenPrec p 25 $ text "__alignof" <> parens (atsPretty m decl)
  atsPrettyPrec m p (CComplexReal expr _) =
    parenPrec p 25 $ text "__real" <+> atsPrettyPrec m 25 expr
  atsPrettyPrec m p (CComplexImag expr _) =
    parenPrec p 25 $ text "__imag" <+> atsPrettyPrec m 25 expr
  atsPrettyPrec m p (CIndex expr1 expr2 _) =
    parenPrec p 26 $ atsPrettyPrec m 26 expr1
    <> text "[" <> atsPretty m expr2 <> text "]"
  atsPrettyPrec m p (CCall expr args _) =
    parenPrec p 30 $ atsPrettyPrec m 30 expr <> text "("
    <> (sep . punctuate comma . map (atsPretty m)) args <> text ")"
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
 atsPretty m (CDecl t _ _) = atsPretty m t

instance AtsPretty [CDeclarationSpecifier NodeInfo] where
  atsPretty m [(CTypeSpec (CVoidType _))]        = text "ptr"
  atsPretty m [(CTypeSpec (CCharType _))]        = text "char"
  atsPretty m [(CTypeSpec (CShortType _))]       = text "short"
  atsPretty m [(CTypeSpec (CIntType _))]         = text "int"
  atsPretty m [(CTypeSpec (CFloatType _))]       = text "float"
  atsPretty m [(CTypeSpec (CDoubleType _))]      = text "double"
  atsPretty m [(CTypeSpec (CBoolType _))]        = text "bool"
  atsPretty m [(CTypeSpec (CEnumType enum _))]   = text "int"
  atsPretty m [(CTypeSpec (CSignedType _)),
               (CTypeSpec (CCharType _))]        = text "schar"
  atsPretty m [(CTypeSpec (CUnsigType _)),
               (CTypeSpec (CCharType _))]        = text "uchar"
  atsPretty m [(CTypeSpec (CSignedType _)),
               (CTypeSpec (CShortType _))]       = text "sint"
  atsPretty m [(CTypeSpec (CUnsigType _)),
               (CTypeSpec (CShortType _))]       = text "usint"
  atsPretty m [(CTypeSpec (CUnsigType _)),
               (CTypeSpec (CIntType _))]         = text "uint"
  atsPretty m [(CTypeSpec (CLongType _)),
               (CTypeSpec (CIntType _))]         = text "lint"
  atsPretty m [(CTypeSpec (CUnsigType _)),
               (CTypeSpec (CLongType _)),
               (CTypeSpec (CIntType _))]         = text "ulint"
  atsPretty m [(CTypeSpec (CLongType _)),
               (CTypeSpec (CLongType _)),
               (CTypeSpec (CIntType _))]         = text "llint"
  atsPretty m [(CTypeSpec (CUnsigType _)),
               (CTypeSpec (CLongType _)),
               (CTypeSpec (CLongType _)),
               (CTypeSpec (CIntType _))]         = text "ullint"
  atsPretty m [(CTypeSpec (CLongType _)),
               (CTypeSpec (CDoubleType _))]      = text "ldouble"
  atsPretty m [(CTypeSpec (CTypeDef ident _))]   = text "type" <> atsPretty m ident

--------------------------------------------------------------------------------
class CPretty p where
  cPretty     :: AtsPrettyMap -> p -> Doc
  cPrettyPrec :: AtsPrettyMap -> Int -> p -> Doc
  cPretty     m   = cPrettyPrec m 0
  cPrettyPrec m _ = cPretty m

subscriptArray :: AtsPrettyMap -> Type -> Doc
subscriptArray m (ArrayType t s _ _) = (brackets $ cPretty m s) <> subscriptArray m t
subscriptArray m t = empty

instance CPretty ArraySize where
  cPretty m (UnknownArraySize _) = empty
  cPretty m (ArraySize _ e)      = pretty e

instance CPretty FlatGlobalDecl where
  -- cPretty m (FGObj  d) = cPretty m d
  cPretty m (FGTag  d) = cPretty m d
  cPretty m (FGType d) = cPretty m d

instance CPretty TagDef where
  cPretty m (CompDef compty) = cPretty m compty
  cPretty m (EnumDef enumty) = text "int" -- ATS does not have enum

instance CPretty TypeDef where
  cPretty m (TypeDef ident ty _ _) =
    text "typedef" <+> cPretty m ty <+> cPretty m ident

instance CPretty CompType where
  cPretty m (CompType sue_ref tag members attrs node) =
    ext sue_ref
    where
      ext (NamedRef ident) = cPretty m tag <+> pretty ident
      ext (AnonymousRef _) = cPretty m tag <+> text "{" <+> hcat (map (cPretty m) members) <> text "}"

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
  cPretty m (TyComp (CompTypeRef s@(NamedRef _) c _))     = cPretty m c <+> cPretty m s
  cPretty m (TyComp (CompTypeRef s@(AnonymousRef _) c _)) = cPretty m s
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
  cPretty m (NamedRef ident) = cPretty m ident
  cPretty m s@(AnonymousRef _) =
    let Just g = Map.lookup s m in cPretty m g

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
