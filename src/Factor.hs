module Factor where

import Parser
import Simplify
import qualified Data.Map as Map

-- Stitch together MulVecs to find the greatest common factor
commonFactorMerge :: MulVec -> MulVec -> MulVec
commonFactorMerge (c1, eo1) (c2, eo2) = (gcd c1 c2, Map.intersectionWith (min) eo1 eo2)

-- Find greatest common factor over addition
gcf :: Expr -> Expr
gcf expr = pStitchEO $ go expr
  where
    go e@(Mul _ _) = pCountExpr e
    go (Add lexpr rexpr) = commonFactorMerge (go lexpr) (go rexpr)
    go (Frac nexpr _) = go nexpr
    go e = pCountExpr e