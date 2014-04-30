module Main where

import Data.Array
import Data.Maybe

import Debug.Trace

import qualified Text.Parsing.StringParser as P

import qualified Test.QuickCheck as QC

main = do

  let test1 = P.string "test" :: P.Parser String
  
  print $ P.runParser test1 "testing"
  print $ P.runParser test1 "foo" 
