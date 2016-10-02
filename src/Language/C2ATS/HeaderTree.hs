{-# LANGUAGE OverloadedStrings #-}
module Language.C2ATS.HeaderTree
       ( headerTree
       , createSATS
       ) where

import Data.Maybe
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Internal as BI
import Control.Monad
import Control.Exception
import System.FilePath
import System.Directory
import System.Process
import System.Exit

import Language.C2ATS.FlatGlobalDecl
import Language.C2ATS.Pretty

data CHeader = CHeaderQuot FilePath
             | CHeaderLess FilePath
             deriving (Show)

type CHTree = Tree (CHeader, FilePath)
type MapCHeader = Map FilePath CHeader
type IncPath = ([FilePath], [FilePath])

-- Search order: https://gcc.gnu.org/onlinedocs/cpp/Include-Syntax.html
readFileHeader :: IncPath -> CHeader -> IO (Maybe FilePath, B.ByteString)
readFileHeader (_, incPath) (CHeaderLess file)         = readFileHeader' incPath file
readFileHeader (incPathQ, incPathL) (CHeaderQuot file) =
  readFileHeader' ("." : incPathQ ++ incPathL) file

readFileHeader' :: [FilePath] -> FilePath -> IO (Maybe FilePath, B.ByteString)
readFileHeader' (path:incPath) file = do
  handle handler $ do
    let file' = path </> file
    buf <- B.readFile file'
    rPath <- realPath file'
    return (Just rPath, buf)
  where
    handler :: IOException -> IO (Maybe FilePath, B.ByteString)
    handler _ = readFileHeader' incPath file
readFileHeader' [] file = return (Nothing, "")

includeHeaders :: MapCHeader -> IncPath -> B.ByteString -> IO (MapCHeader, [CHTree])
includeHeaders mapHead hPath buffer = do
  (mapHead', cTree) <- foldM (go hPath) (mapHead, []) $ map toCHeader $ incs buffer
  return (mapHead', reverse cTree)
  where
    incs :: B.ByteString -> [B.ByteString]
    incs = filter (BC.isPrefixOf "#include") . map (B.filter (/= BI.c2w ' ')) . BC.lines
    toCHeader :: B.ByteString -> CHeader
    toCHeader inc =
      if isJust $ BC.find (== '"') inc
      then CHeaderQuot $ BC.unpack ((BC.split '"' inc) !! 1)
      else if isJust $ BC.find (== '<') inc
           then CHeaderLess $ BC.unpack $ (BC.split '>' ((BC.split '<' inc) !! 1)) !! 0
           else error $ BC.unpack inc
    go :: IncPath -> (MapCHeader, [CHTree]) -> CHeader -> IO (MapCHeader, [CHTree])
    go hPath (mapHead, cTrees) cHead = do
      (rFile, buf) <- readFileHeader hPath cHead
      if isNothing rFile then return (mapHead, cTrees)
        else if Map.member (fromJust rFile) mapHead then
        return (mapHead, Node {rootLabel = (cHead, fromJust rFile),
                               subForest = []}:cTrees)
        else do
        (mapHead', cTree) <- toTree hPath mapHead (fromJust rFile) cHead buf
        return (mapHead', cTree:cTrees)

toTree :: IncPath -> MapCHeader -> FilePath -> CHeader -> B.ByteString -> IO (MapCHeader, CHTree)
toTree hPath mapHead rFile file buf = do
  let mapHead' = Map.insert rFile file mapHead
  (mapHead'', incs) <- includeHeaders mapHead' hPath buf
  return (mapHead'', Node {rootLabel = (file, rFile), subForest = incs})

headerTree :: String -> [String] -> FilePath -> IO (MapCHeader, CHTree)
headerTree gcc copts file = do
  (ExitSuccess,_,spec) <- readProcessWithExitCode gcc (["-E", "-Q", "-v"] ++ file:copts) ""
  let file' = CHeaderQuot file
      hPath = searchPath spec
  (rFile, buf) <- readFileHeader hPath file'
  if isNothing rFile
    then return (Map.empty, Node {rootLabel = (CHeaderQuot "", ""), subForest = []})
    else toTree hPath Map.empty (fromJust rFile) file' buf

foldMTree :: Monad m => (b -> Tree a -> m b) -> b -> Tree a -> m b
foldMTree f b t@(Node {rootLabel = r, subForest = []}) = f b t
foldMTree f b t@(Node {rootLabel = r, subForest = s})  =
  f b t >>= (\a -> foldMTree' f a s)

foldMTree' :: Monad m => (b -> Tree a -> m b) -> b -> [Tree a] -> m b
foldMTree' f b []     = return b
foldMTree' f b (t:ts) = foldMTree f b t >>= (\a -> foldMTree' f a ts)

createSATS :: FilePath -> MapCHeader -> CHTree -> MapFlatG -> IO ()
createSATS oDir mapHead cTrees sGlobal =
  prelude sGlobal
  >>= (\sg -> foldMTree go sg cTrees)
  >>= (\sg -> if Map.null sg then return ()
              else error ("*** HeaderTree remains " ++ (show $ Map.keys sg)))
  where
    prelude :: MapFlatG -> IO MapFlatG
    prelude sg = do
      let pre  = fromJust $ Map.lookup Nothing sg
          sats = oDir </> "c2ats_prelude.sats"
      isNotExist <- fmap not $ doesFileExist $ sats
      when isNotExist $ do
        createDirectoryIfMissing True $ takeDirectory sats
        writeFile sats $ show . atsPrettyGlobal $ pre
      return $ Map.delete Nothing sg
    go :: MapFlatG -> CHTree -> IO MapFlatG
    go sg (Node {rootLabel = (CHeaderQuot file, rPath), subForest = sub}) =
      go' sg rPath sub (unlines [
                           "%{#",
                           "#include \"" ++ file ++ "\"",
                           "%}"
                           ])
    go sg (Node {rootLabel = (CHeaderLess file, rPath), subForest = sub}) =
      go' sg rPath sub (unlines [
                           "%{#",
                           "#include <" ++ file ++ ">",
                           "%}"
                           ])
    go' :: MapFlatG -> FilePath -> [CHTree] -> String -> IO MapFlatG
    go' sg rPath sub inc = do
      let sats   = oDir </> tail rPath -<.> ".sats"
      isNotExist <- fmap not $ doesFileExist $ sats
      when isNotExist $ do
        createDirectoryIfMissing True $ takeDirectory sats
        BL.writeFile sats $ BLC.pack $ "// File: " ++ rPath -<.> ".sats\n"
        when (not . null $ sub) $
          BL.appendFile sats $ foldr stainc "" sub
        BL.appendFile sats $ BLC.pack inc
        let insg = Map.lookup (Just rPath) sg
        when (isJust insg) $
          BL.appendFile sats $ BLC.pack $ show . atsPrettyGlobal $ fromJust insg
      return $ Map.delete (Just rPath) sg
    stainc :: CHTree -> BL.ByteString -> BL.ByteString
    stainc (Node {rootLabel = (_, rPath), subForest = _}) =
      BLC.append $ BLC.pack $ "#include \"" ++ oDir </> tail rPath -<.> ".sats" ++ "\"\n"

searchPath :: String -> IncPath
searchPath spec =
  let incQuot = "#include \"...\" search starts here:"
      incLess = "#include <...> search starts here:"
      endInc  = "End of search list."
      headerQ, headerL :: [FilePath]
      headerQ = takeWhile (/= incLess) $ tail $ dropWhile (/= incQuot) $ lines spec
      headerL = takeWhile (/= endInc) $ tail $ dropWhile (/= incLess) $ lines spec
  in (map (filter (/= ' ')) $ headerQ, map (filter (/= ' ')) headerL)
