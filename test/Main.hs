{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

import Language.C2ATS

main :: IO ()
main = $(defaultMainGenerator)

case_1 = 1 @=? 1
