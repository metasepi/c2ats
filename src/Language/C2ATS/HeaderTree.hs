{-# LANGUAGE OverloadedStrings #-}
module Language.C2ATS.HeaderTree
       ( headerTree
       ) where

import Data.Maybe
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Exception
import System.FilePath
import System.Process
import System.Exit

data CHeader = CHeaderQuot FilePath
             | CHeaderLess FilePath
             | CHeaderNone
             deriving (Show)

type CHeaders = Tree (CHeader, FilePath)

realPath :: FilePath -> IO FilePath
realPath file = do
  (ExitSuccess,rfile,_) <- readProcessWithExitCode "realpath" [file] ""
  return $ init rfile

-- Search order: https://gcc.gnu.org/onlinedocs/cpp/Include-Syntax.html
readFileHeader :: ([FilePath], [FilePath]) -> CHeader -> IO (FilePath, B.ByteString)
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

includeHeaders :: ([FilePath], [FilePath]) -> B.ByteString -> IO [CHeaders]
includeHeaders hPath buf =
  handle handler $ do
    mapM toTree $ map toCHeader $ incs buf
  where
    handler :: SomeException -> IO [CHeaders]
    handler _ = return []
    incs :: B.ByteString -> [B.ByteString]
    incs = filter (BC.isPrefixOf "#include") . BC.lines
    toCHeader :: B.ByteString -> CHeader
    toCHeader inc =
      if isJust $ BC.find (== '"') inc
      then CHeaderQuot $ BC.unpack ((BC.split '"' inc) !! 1)
      else if isJust $ BC.find (== '<') inc
           then CHeaderLess $ BC.unpack $ (BC.split '>' ((BC.split '<' inc) !! 1)) !! 0
           else CHeaderNone
    toTree :: CHeader -> IO CHeaders
    toTree CHeaderNone = return $ Node {rootLabel = (CHeaderNone, ""), subForest = []}
    toTree file        = do
      (rFile, buf) <- readFileHeader hPath file
      incs <- includeHeaders hPath buf
      return Node {rootLabel = (file, rFile), subForest = incs}

headerTree :: String -> [String] -> FilePath -> IO CHeaders
headerTree gcc copts file = do
  (ExitSuccess,_,spec) <- readProcessWithExitCode gcc (["-E", "-Q", "-v"] ++ file:copts) ""
  let file' = CHeaderQuot file
      hPath = searchPath spec
  (rFile, buf) <- readFileHeader hPath file'
  s <- includeHeaders hPath buf
  return $ Node {rootLabel = (file', rFile), subForest = s}

searchPath :: String -> ([FilePath], [FilePath])
searchPath spec =
  let incQuot = "#include \"...\" search starts here:"
      incLess = "#include <...> search starts here:"
      endInc  = "End of search list."
      headerQ, headerL :: [FilePath]
      headerQ = takeWhile (/= incLess) $ tail $ dropWhile (/= incQuot) $ lines spec
      headerL = takeWhile (/= endInc) $ tail $ dropWhile (/= incLess) $ lines spec
  in (map (filter (/= ' ')) $ headerQ, map (filter (/= ' ')) headerL)
