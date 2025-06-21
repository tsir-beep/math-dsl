module Printer where

import Parser

-- This is pretty much a CFG
exprToString :: Expr -> String
exprToString expr = case expr of
  Add lexpr rexpr -> "(" ++ (exprToString lexpr) ++ " + " ++ (exprToString rexpr) ++ ")"
  Mul lexpr rexpr -> "(" ++ (exprToString lexpr) ++ (exprToString rexpr) ++ ")"
  Frac lexpr rexpr -> "(" ++ (exprToString lexpr) ++ "/" ++ (exprToString rexpr) ++ ")"
  Pow lexpr n -> "(" ++ (exprToString lexpr) ++ "^" ++ show n ++ ")"
  Var c -> [c]
  Const n -> show n