{-# LANGUAGE OverloadedStrings #-}
module Language.C2ATS.HeaderTree
       ( headerTree
       ) where

import Data.Maybe
import Data.Tree
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

readFileHeader :: ([FilePath], [FilePath]) -> CHeader -> IO B.ByteString
readFileHeader (incPath, _) (CHeaderQuot file) = readFileHeader' incPath file
readFileHeader (_, incPath) (CHeaderLess file) = readFileHeader' incPath file

readFileHeader' :: [FilePath] -> FilePath -> IO B.ByteString
readFileHeader' (path:incPath) file =
  handle handler $ B.readFile $ path </> file
  where
    handler :: IOException -> IO B.ByteString
    handler _ = readFileHeader' incPath file
readFileHeader' [] file             = B.readFile file

includeHeaders :: ([FilePath], [FilePath]) -> CHeader -> IO [Tree CHeader]
includeHeaders (headerQ, headerL) file =
  handle handler $ do
    readFileHeader (headerQ, headerL) file >>= (\buf -> mapM toTree $ map toCHeader $ incs buf)
  where
    handler :: IOException -> IO [Tree CHeader]
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
    toTree :: CHeader -> IO (Tree CHeader)
    toTree CHeaderNone = return $ Node {rootLabel = CHeaderNone, subForest = []}
    toTree file        =
      includeHeaders (headerQ, headerL) file
      >>= (\l -> return Node {rootLabel = file, subForest = l})

searchPath :: String -> ([FilePath], [FilePath])
searchPath spec =
  let incQuot = "#include \"...\" search starts here:"
      incLess = "#include <...> search starts here:"
      endInc  = "End of search list."
      headerQ, headerL :: [FilePath]
      headerQ = takeWhile (/= incLess) $ tail $ dropWhile (/= incQuot) $ lines spec
      headerL = takeWhile (/= endInc) $ tail $ dropWhile (/= incLess) $ lines spec
  in (map (filter (/= ' ')) $ headerQ, map (filter (/= ' ')) headerL)

headerTree :: String -> [String] -> FilePath -> IO (Tree CHeader)
headerTree gcc copts file = do
  (ExitSuccess,_,spec) <- readProcessWithExitCode gcc (["-E", "-Q", "-v"] ++ file:copts) ""
  let file' = CHeaderQuot file
  s <- includeHeaders (searchPath spec) file'
  return $ Node {rootLabel = file', subForest = s}
