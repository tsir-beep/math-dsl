module Simplify where

import Parser
import qualified Data.Map as Map

-- Occurence dictionary
type ExprOccurences = Map.Map Expr Int

-- Constant vector encoding over multiplication
-- Holds the constant throughout occurence counting as the first argument
type MulVec = (Int, ExprOccurences)

-- Merge tuples carrying constant
mulMerge :: MulVec -> MulVec -> MulVec
mulMerge (c1, vo1) (c2, vo2) = (c1 * c2, Map.unionWith (+) vo1 vo2)

-- Count the occurrences that appear in a product term
pCountExpr :: Expr -> MulVec
pCountExpr e@(Var _) = (1, Map.singleton e 1)
pCountExpr (Const n) = (n, Map.empty)
pCountExpr (Pow e n) = (1, Map.singleton e n)
pCountExpr (Mul rexpr (Frac nexpr dexpr)) = pCountExpr (Frac (Mul rexpr nexpr) dexpr)
pCountExpr (Mul lexpr rexpr) = mulMerge (pCountExpr lexpr) (pCountExpr rexpr)
pCountExpr e@(Add _ _) = (1, Map.singleton (simplifyProducts e) 1)
pCountExpr e@(Frac _ _) = (1, Map.singleton (simplifyProducts e) 1)

-- Stitches occurrence map to make a folded product
pStitchEO :: MulVec -> Expr
pStitchEO (c, eo)
  | c == 1 = go $ Map.toList eo
  | otherwise = Mul (Const c) (go $ Map.toList eo)
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
simplifyProducts pTerm@(Mul _ _) = pStitchEO $ pCountExpr pTerm
simplifyProducts (Add lexpr rexpr) 
  = Add (simplifyProducts lexpr) (simplifyProducts rexpr)
simplifyProducts (Frac lexpr rexpr) 
  = Frac (simplifyProducts lexpr) (simplifyProducts rexpr)
simplifyProducts (Pow expr n) = Pow (simplifyProducts expr) n
simplifyProducts var@(Var _) = var
simplifyProducts c@(Const _) = c

-- Constant vector encoding over addition
-- Holds the constant throughout occurence counting as the first argument
type AddVec = (Int, ExprOccurences)

-- Merge tuples carrying constant
addMerge :: MulVec -> MulVec -> MulVec
addMerge (c1, vo1) (c2, vo2) = (c1 + c2, Map.unionWith (+) vo1 vo2)

-- Count occurrences over addition
aCountExpr :: Expr -> MulVec
aCountExpr (Add lexpr rexpr) = addMerge (aCountExpr lexpr) (aCountExpr rexpr)
aCountExpr (Const n) = (n, Map.empty)
aCountExpr (Mul (Const n) e) = (0, Map.singleton e n)
aCountExpr e = (0, Map.singleton e 1)

-- Add like-terms
addLikeTerms :: MulVec -> Expr
addLikeTerms (c, eo) 
  | c == 0 = go $ Map.toList eo -- 0 is identity element over addition
  | otherwise = Add (go $ Map.toList eo) (Const c)
  where
    go [] = Const 0
    go [(x,n)]
      | n == 1 = x
      | otherwise = Mul (Const n) x
    go ((x,n):xs)
      | n == 1 = Add x (go xs)
      | otherwise = Add (Mul (Const n) x) (go xs)

-- Simplify over addition
simplifyOverAddition :: Expr -> Expr
simplifyOverAddition = addLikeTerms . aCountExpr

-- Simplify arithmetic expression
simplify :: Expr -> Expr
simplify = simplifyOverAddition . simplifyProducts