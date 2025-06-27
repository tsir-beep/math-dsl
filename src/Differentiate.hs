module Differentiate (diffExpr) where

import Parser

-- Differentiate an expression with respect to a variable
diffExpr :: Expr -> Char -> Expr
diffExpr (Const _) _ = (Const 0)
diffExpr (Var x) v
  | x == v = (Const 1)
  | otherwise = (Const 0) 
diffExpr (Add lexpr rexpr) v = Add (diffExpr lexpr v) (diffExpr rexpr v)
diffExpr (Mul lexpr rexpr) v = Add (Mul (rexpr) (diffExpr lexpr v)) (Mul (lexpr) (diffExpr rexpr v)) -- Product rule
diffExpr (Pow expr n) v = Mul (Const n) (Mul (diffExpr expr v) (Pow (expr) (n - 1))) -- Chain rule
diffExpr (Frac nexpr dexpr) v -- Quotient rule
  = Frac 
    (Add 
      (Mul (dexpr) (diffExpr nexpr v)) 
      (Mul (Const (-1)) 
        (Mul (nexpr) (diffExpr dexpr v)))) 
    (Pow (dexpr) 2) 