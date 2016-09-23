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
import System.Process
import System.Exit

data CHeader = CHeaderLess FilePath
             | CHeaderQuot FilePath
             | CHeaderNone
             deriving (Show)

includeHeaders :: FilePath -> IO [Tree CHeader]
includeHeaders file =
  handle handler $ do
    B.readFile file >>= (\buf -> mapM toTree $ map toCHeader $ incs buf)
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
    toTree (CHeaderLess file) =
      includeHeaders file
      >>= (\l -> return Node {rootLabel = CHeaderLess file, subForest = l})
    toTree (CHeaderQuot file) =
      includeHeaders file
      >>= (\l -> return Node {rootLabel = CHeaderQuot file, subForest = l})
    toTree CHeaderNone        = return $ Node {rootLabel = CHeaderNone, subForest = []}

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
  print $ searchPath spec
  s <- includeHeaders file
  return $ Node {rootLabel = CHeaderQuot file, subForest = s}
