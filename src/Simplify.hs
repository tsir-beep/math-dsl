module Simplify where

import Parser
import qualified Data.Map as Map

-- Occurence dictionary
type ExprOccurences = Map.Map Expr Int

-- Constant vector encoding
-- Holds the constant throughout occurence counting as the first argument
type MulVec = (Int, ExprOccurences)

-- Merge tuples carrying constant
merge :: MulVec -> MulVec -> MulVec
merge (c1, vo1) (c2, vo2) = (c1 * c2, Map.unionWith (+) vo1 vo2)

-- Count the occurrences that appear in a product term
countVars :: Expr -> MulVec
countVars e@(Var _) = (1, Map.singleton e 1)
countVars (Const n) = (n, Map.empty)
countVars (Pow e n) = (1, Map.singleton e n)
countVars (Mul lexpr rexpr) = merge (countVars lexpr) (countVars rexpr)
countVars e@(Add _ _) = (1, Map.singleton e 1)
countVars _ = (1, Map.empty)

-- Stitches occurrence map to make a folded product
stitchVO :: MulVec -> Expr
stitchVO (c, vo)
  | c == 1 = go $ Map.toList vo
  | otherwise = Mul (Const c) (go $ Map.toList vo)
    where
      go [] = Const 1
      go [(x,n)]
        | n == 1 = x
        | otherwise = Pow x n
      go ((x,n):xs)
        | n == 1 = Mul x (go xs)
        | otherwise = Mul (Pow x n) (go xs)

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