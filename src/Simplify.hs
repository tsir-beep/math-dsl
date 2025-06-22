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

-- Stitches occurrence map to make a folded product
stitchVO :: MulVec -> Expr
stitchVO (c, vo) = Mul (Const c) (go $ Map.toList vo)
  where
    go [] = Const 1
    go [(x,n)]
      | n == 1 = Var x
      | otherwise = Pow (Var x) n
    go ((x,n):xs)
      | n == 1 = Mul (Var x) (go xs)
      | otherwise = Mul (Pow (Var x) n) (go xs)

-- Simplify product terms in a Expr by folding them
simplifyProducts :: Expr -> Expr
simplifyProducts pTerm@(Mul _ _) = stitchVO $ countVars pTerm
simplifyProducts (Add lexpr rexpr) 
  = Add (simplifyProducts lexpr) (simplifyProducts rexpr)
simplifyProducts (Frac lexpr rexpr) 
  = Frac (simplifyProducts lexpr) (simplifyProducts rexpr)
simplifyProducts (Pow expr n) = Pow (simplifyProducts expr) n
simplifyProducts var@(Var _) = var
simplifyProducts c@(Const _) = c