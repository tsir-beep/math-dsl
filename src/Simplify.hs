module Simplify where

import Parser
import qualified Data.Map as Map

-- Occurence dictionary
type VarOccurences = Map.Map Char Int

-- Constant vector encoding
-- Holds the constant throughout occurence counting as the first argument
type MulVec = (Int, VarOccurences)

-- Merge tuples carrying constant
merge :: MulVec -> MulVec -> MulVec
merge (c1, vo1) (c2, vo2) = (c1 * c2, Map.unionWith (+) vo1 vo2)

-- Count the occurrences that appear in a product term
countVars :: Expr -> MulVec
countVars (Var x) = (1, Map.singleton x 1)
countVars (Const n) = (n, Map.empty)
countVars (Pow (Var x) n) = (1, Map.singleton x n)
countVars (Mul lexpr rexpr) = merge (countVars lexpr) (countVars rexpr)
countVars _ = (1, Map.empty)