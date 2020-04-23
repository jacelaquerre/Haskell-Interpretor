-- Final Project
-- CS225
-- Jace Laquerre & Chris McCabe

module FP where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Testing
import qualified Lang.L1 as L1

-- This function is just to test that the file is building properly
cubed :: Int -> Int
cubed x =
  if x == 0
    then 0
    else x * x * x

main :: IO ()
main = runTests []
