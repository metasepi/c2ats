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

realPath :: FilePath -> IO FilePath
realPath file = do
  (ExitSuccess,rfile,_) <- readProcessWithExitCode "realpath" [file] ""
  return $ init rfile

-- Search order: https://gcc.gnu.org/onlinedocs/cpp/Include-Syntax.html
readFileHeader :: IncPath -> CHeader -> IO (FilePath, B.ByteString)
readFileHeader (_, incPath) (CHeaderLess file)         = readFileHeader' incPath file
readFileHeader (incPathQ, incPathL) (CHeaderQuot file) =
  readFileHeader' ("." : incPathQ ++ incPathL) file

readFileHeader' :: [FilePath] -> FilePath -> IO (FilePath, B.ByteString)
readFileHeader' (path:incPath) file = do
  handle handler $ do
    let file' = path </> file
    buf <- B.readFile file'
    rPath <- realPath file'
    return (rPath, buf)
  where
    handler :: IOException -> IO (FilePath, B.ByteString)
    handler _ = readFileHeader' incPath file
readFileHeader' [] file = throw $ PatternMatchFail file

includeHeaders :: MapCHeader -> IncPath -> B.ByteString -> IO (MapCHeader, [CHTree])
includeHeaders mapHead hPath buf =
  handle (handler mapHead) $ do
    (mapHead', cTree) <- foldM (go hPath) (mapHead, []) $ map toCHeader $ incs buf
    return (mapHead', reverse cTree)
  where
    handler :: MapCHeader -> SomeException -> IO (MapCHeader, [CHTree])
    handler mapHead _ = return (mapHead, [])
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
      if Map.member rFile mapHead then do
        return (mapHead, Node {rootLabel = (cHead, rFile), subForest = []}:cTrees)
        else do
        (mapHead', cTree) <- toTree hPath mapHead rFile cHead buf
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
  toTree hPath Map.empty rFile file' buf

foldMTree :: Monad m => (b -> Tree a -> m b) -> b -> Tree a -> m b
foldMTree f b t@(Node {rootLabel = r, subForest = []}) = f b t
foldMTree f b t@(Node {rootLabel = r, subForest = s})  =
  f b t >>= (\a -> foldMTree' f a s)

foldMTree' :: Monad m => (b -> Tree a -> m b) -> b -> [Tree a] -> m b
foldMTree' f b []     = return b
foldMTree' f b (t:ts) = foldMTree f b t >>= (\a -> foldMTree' f a ts)

createSATS :: FilePath -> MapCHeader -> CHTree -> Map (Maybe FilePath) [FlatG] -> IO ()
createSATS oDir mapHead cTrees sGlobal =
  foldMTree go sGlobal cTrees >>= (\a -> print $ Map.keys a)
  where
    go :: Map (Maybe FilePath) [FlatG] -> CHTree -> IO (Map (Maybe FilePath) [FlatG])
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
    go' :: Map (Maybe FilePath) [FlatG] -> FilePath -> [CHTree] -> String
           -> IO (Map (Maybe FilePath) [FlatG])
    go' sg rPath sub inc = do
      let sats   = oDir </> tail rPath -<.> ".sats"
      isNotExist <- fmap not $ doesFileExist $ sats
      when isNotExist $ do
        createDirectoryIfMissing True $ takeDirectory sats
        writeFile sats $ "// File: " ++ rPath -<.> ".sats\n"
        when (not . null $ sub) $
          appendFile sats $ foldr stainc "" sub
        appendFile sats inc
        let insg = Map.lookup (Just rPath) sg
        when (isJust insg) $
          appendFile sats $ show . atsPrettyGlobal $ fromJust insg
      return $ Map.delete (Just rPath) sg
    stainc :: CHTree -> String -> String
    stainc (Node {rootLabel = (_, rPath), subForest = _}) =
      (++) $ "#include \"" ++ "{$C2ATS}" </> tail rPath -<.> ".sats" ++ "\"\n"

searchPath :: String -> IncPath
searchPath spec =
  let incQuot = "#include \"...\" search starts here:"
      incLess = "#include <...> search starts here:"
      endInc  = "End of search list."
      headerQ, headerL :: [FilePath]
      headerQ = takeWhile (/= incLess) $ tail $ dropWhile (/= incQuot) $ lines spec
      headerL = takeWhile (/= endInc) $ tail $ dropWhile (/= incLess) $ lines spec
  in (map (filter (/= ' ')) $ headerQ, map (filter (/= ' ')) headerL)
