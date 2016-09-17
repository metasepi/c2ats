{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Language.C2ATS.Process
       ( flatGlobal
       , injectForwardDecl
       , sortFlatGlobal
       , injectIncludes
       , FlatGlobalDecl (..)
       , FlatG (..)
       ) where

import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Regex.Posix
import Debug.Trace {- for warnings -}

import Language.C
import Language.C.Analysis
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

type FlatG = (SUERef, FlatGlobalDecl)

flatGlobal :: GlobalDecls -> [FlatG]
flatGlobal gmap = theTags ++ theObjs ++ theTypeDefs
  where
    theTags     = Map.assocs $ Map.map FGTag  $ gTags gmap
    theObjs     = Map.assocs $ Map.map FGObj  $ Map.mapKeys NamedRef $ gObjs gmap
    theTypeDefs = Map.assocs $ Map.map FGType $ Map.mapKeys NamedRef $ gTypeDefs gmap

injectIncludes :: [String] -> [String] -> [FlatG] -> [FlatG]
injectIncludes includes excludes m =
  concat . map incl . map reverse . sortElems . foldl go ([], Map.empty) $ m
  where
    f :: String -> Maybe FilePath
    f ('<':_) = Nothing
    f xs      = Just . filter (\c -> c /= '"' && c /= ':' && c /= '(') . head . words $ xs
    getFile :: FlatG -> Maybe FilePath
    getFile = f . show . posOfNode . nodeInfo . snd
    sortElems :: ([Maybe FilePath], Map (Maybe FilePath) [FlatG]) -> [[FlatG]]
    sortElems (file:files,mp) =
      sortElems (files, Map.delete file mp) ++ [fromJust $ Map.lookup file mp]
    sortElems ([], _)         = []
    go :: ([Maybe FilePath], Map (Maybe FilePath) [FlatG]) -> FlatG
          -> ([Maybe FilePath], Map (Maybe FilePath) [FlatG])
    go (files,mp) fg =
      let file = getFile fg
          files' = if file `elem` files then files else file:files
      in (files', Map.insertWith (++) file [fg] mp)
    incl :: [FlatG] -> [FlatG]
    incl fgs@((s,fg):_) = case getFile (s,fg) of
      Nothing ->
        (s, FGRaw $ "// No file"):fgs
      (Just file) | (or $ map (=~ file) includes) || (not . or $ map ((=~) file) excludes) ->
        (s, FGRaw $ init $ unlines [
            "// File: " ++ file,
            "%{#",
            "#include \"" ++ file ++ "\"",
            "%}"
            ]):fgs
      (Just file) ->
        (s, FGRaw $ "// File: " ++ file):fgs

--------------------------------------------------------------------------------
sortFlatGlobal :: [FlatG] -> [FlatG]
sortFlatGlobal = (\(a,_,_,b) -> reverse a ++ b) . foldl go ([], Set.empty, Set.empty, []) . sortBy order
  where
    order :: FlatG -> FlatG -> Ordering
    order (_, a) (_, b) = nodeInfo a `compare` nodeInfo b
    go :: ([FlatG], Set Int, Set Int, [FlatG]) -> FlatG ->
          ([FlatG], Set Int, Set Int, [FlatG])
    go (out, knowns, deps, ks) fg@(s,_) =
      let knowns' = Set.insert (nodeSUERef s) knowns
          deps'   = Set.difference (Set.union deps $ anons fg) knowns'
          out'    = (if Set.null deps' then reverse ks ++ [fg] else []) ++ out
          ks'     = if Set.null deps' then [] else fg : ks
      in (out', knowns', deps', ks')
    anons :: FlatG -> Set Int
    anons (_, g) = anonRefs g

nodeSUERef :: SUERef -> Int
nodeSUERef (AnonymousRef n)                        = nameId n
nodeSUERef (NamedRef (Ident _ _ (NodeInfo _ _ n))) = nameId n
nodeSUERef _                                       = -1

class AnonRefs p where
  anonRefs :: p -> Set Int

instance AnonRefs FlatGlobalDecl where
  anonRefs (FGObj  d) = anonRefs d
  anonRefs (FGTag  d) = anonRefs d
  anonRefs (FGType d) = anonRefs d

instance AnonRefs IdentDecl where
  anonRefs (Declaration (Decl v _))     = anonRefs v
  anonRefs (ObjectDef (ObjDef v _ _))   = anonRefs v
  anonRefs (FunctionDef (FunDef v _ _)) = anonRefs v
  anonRefs (EnumeratorDef _)            = Set.empty -- ATS does not have enum

instance AnonRefs MemberDecl where
  anonRefs (MemberDecl v _ _)   = anonRefs v
  anonRefs (AnonBitField t _ _) = anonRefs t

instance AnonRefs VarDecl where
  anonRefs (VarDecl _ _ t) = anonRefs t

instance AnonRefs TagDef where
  anonRefs (EnumDef _)                    = Set.empty  -- ATS does not have enum
  anonRefs (CompDef (CompType _ _ m _ _)) = Set.unions $ map anonRefs m

instance AnonRefs TypeDef where
  anonRefs (TypeDef _ t _ _) = anonRefs t

instance AnonRefs Type where
  anonRefs (PtrType t _ _)     = anonRefs t
  anonRefs (ArrayType t _ _ _) = anonRefs t
  anonRefs (FunctionType ft _) = anonRefs ft
  anonRefs (DirectType tn _ _) = anonRefs tn
  anonRefs _                   = Set.empty

instance AnonRefs TypeName where
  anonRefs (TyComp (CompTypeRef s _ _)) = anonRefs s
  anonRefs _                            = Set.empty

instance AnonRefs FunType where
  anonRefs (FunTypeIncomplete t) = anonRefs t
  anonRefs (FunType t p _)       =
    Set.union (anonRefs t) (Set.unions $ map anonRefs p)

instance AnonRefs ParamDecl where
  anonRefs (ParamDecl v _)         = anonRefs v
  anonRefs (AbstractParamDecl v _) = anonRefs v

instance AnonRefs SUERef where
  anonRefs s@(AnonymousRef _) = Set.singleton $ nodeSUERef s
  anonRefs (NamedRef _)       = Set.empty

--------------------------------------------------------------------------------
type IndentsMap = (Maybe String, Set String)

injectForwardDecl :: [FlatG] -> [FlatG]
injectForwardDecl = reverse . fst . foldl f ([], Set.empty)
  where
    f :: ([FlatG], Set String) -> FlatG -> ([FlatG], Set String)
    f (fgs, is) fg@(s,g) =
      let (i, is') = idents g
          knownis  = maybe is (\a -> Set.insert a is) i
          fds      = forwardDecls $ Set.elems $ Set.difference is' knownis
          is''     = Set.union is' knownis
      in (fg : fds ++ fgs, is'')

forwardDecls :: [String] -> [FlatG]
forwardDecls = map f
  where
    f i = (NamedRef $ mkIdent nopos i (Name (-1)), FGRaw $ "abst@ype " ++ i ++ " // FIXME! Forward declaration.")

identsAppend :: IndentsMap -> IndentsMap -> IndentsMap
identsAppend (i_a@(Just _), is_a) (i_b, is_b) = (i_a, Set.union is_a is_b)
identsAppend (Nothing, is_a) (i_b, is_b)      = (i_b, Set.union is_a is_b)

class Idents p where
  idents :: p -> IndentsMap

instance Idents FlatGlobalDecl where
  idents (FGObj  d) = idents d
  idents (FGTag  d) = idents d
  idents (FGType d) = idents d
  idents (FGRaw  _) = (Nothing, Set.empty)

instance Idents IdentDecl where
  idents (Declaration (Decl v _))             = idents v
  idents (ObjectDef (ObjDef v _ _))           = idents v
  idents (FunctionDef (FunDef v _ _))         = idents v
  idents _                                    = (Nothing, Set.empty)

instance Idents MemberDecl where
  idents (MemberDecl v _ _)   = idents v
  idents (AnonBitField t _ _) = idents t

instance Idents VarDecl where
  idents (VarDecl _ _ t) = idents t

instance Idents TagDef where
  idents (CompDef (CompType (NamedRef i) k m _ _)) =
    foldl (\a b -> a `identsAppend` idents b) (Just $ show k ++ "_c2ats_" ++ identToString i, Set.empty) m
  idents (CompDef (CompType (AnonymousRef _) _ m _ _)) =
    foldl (\a b -> a `identsAppend` idents b) (Nothing, Set.empty) m
  idents _                              = (Nothing, Set.empty)

instance Idents TypeDef where
  idents (TypeDef i t _ _) =
    (Just $ "type_c2ats_" ++ identToString i, Set.empty) `identsAppend` idents t

instance Idents Type where
  idents (PtrType t _ _)                               = idents t
  idents (ArrayType t _ _ _)                           = idents t
  idents (FunctionType ft _)                           = idents ft
  idents (DirectType (TyComp (CompTypeRef (NamedRef i) k _)) _ _) =
    (Nothing, Set.singleton $ show k ++ "_c2ats_" ++ identToString i)
  idents (TypeDefType (TypeDefRef i _ _) _ _)          =
    (Nothing, Set.singleton $ "type_c2ats_" ++ identToString i)
  idents _                                             = (Nothing, Set.empty)

instance Idents FunType where
  idents (FunTypeIncomplete t) = idents t
  idents (FunType t pd _)      =
    foldl (\a b -> a `identsAppend` idents b) (idents t) pd

instance Idents ParamDecl where
  idents (ParamDecl v _)         = idents v
  idents (AbstractParamDecl v _) = idents v
