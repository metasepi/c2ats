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
import Control.Monad
import Control.Exception
import System.FilePath
import System.Directory
import System.Process
import System.Exit

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
    incs = filter (BC.isPrefixOf "#include") . BC.lines
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
        return (mapHead, cTrees)
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

traverseTree :: Monad m => (Tree a -> m ()) -> Tree a -> m ()
traverseTree f t@(Node {rootLabel = r, subForest = []}) = f t
traverseTree f t@(Node {rootLabel = r, subForest = s})  =
  f t >> mapM_ (traverseTree f) s

createSATS :: FilePath -> (MapCHeader, CHTree) -> IO ()
createSATS oDir (mapHead, cTrees) = traverseTree go cTrees
  where
    go :: CHTree -> IO ()
    go (Node {rootLabel = (CHeaderQuot file, rPath), subForest = sub}) =
      go' file rPath sub (\f -> unlines [
                             "%{#",
                             "#include \"" ++ f ++ "\"",
                             "%}"
                             ])
    go (Node {rootLabel = (CHeaderLess file, rPath), subForest = sub}) =
      go' file rPath sub (\f -> unlines [
                             "%{#",
                             "#include <" ++ f ++ ">",
                             "%}"
                             ])
    go' :: FilePath -> FilePath -> [CHTree] -> (String -> String) -> IO ()
    go' file rPath sub doInc = do
      let file' = oDir </> file
      isNotExist <- fmap not $ doesFileExist $ file' -<.> ".sats"
      when isNotExist $ do
        createDirectoryIfMissing True $ takeDirectory $ file'
        writeFile (file' -<.> ".sats") $ "// File: " ++ file -<.> ".sats\n"
        when (not . null $ sub) $
          appendFile (file' -<.> ".sats") $ foldr staload "" sub
        appendFile (file' -<.> ".sats") $ doInc file
    staload :: CHTree -> String -> String
    staload (Node {rootLabel = (CHeaderQuot file, _), subForest = _}) =
      (++) $ "staload \"" ++ "{$C2ATS}" </> file -<.> ".sats" ++ "\"\n"
    staload (Node {rootLabel = (CHeaderLess file, _), subForest = _}) =
      (++) $ "staload \"" ++ "{$C2ATS}" </> file -<.> ".sats" ++ "\"\n"

searchPath :: String -> IncPath
searchPath spec =
  let incQuot = "#include \"...\" search starts here:"
      incLess = "#include <...> search starts here:"
      endInc  = "End of search list."
      headerQ, headerL :: [FilePath]
      headerQ = takeWhile (/= incLess) $ tail $ dropWhile (/= incQuot) $ lines spec
      headerL = takeWhile (/= endInc) $ tail $ dropWhile (/= incLess) $ lines spec
  in (map (filter (/= ' ')) $ headerQ, map (filter (/= ' ')) headerL)
