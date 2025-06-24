module Printer where

import Parser

-- Precedence table for paranthesising
pTable :: Expr -> Int
pTable (Add _ _) = 0
pTable (Frac _ _) = 0
pTable (Mul _ _) = 1
pTable (Pow _ _) = 2
pTable (Const _) = 3
pTable (Var _) = 3

-- This is pretty much a CFG
exprToString :: Expr -> String
-- exprToString expr = case expr of
--   Add lexpr rexpr -> "(" ++ (exprToString lexpr) ++ " + " ++ (exprToString rexpr) ++ ")"
--   Mul lexpr rexpr -> "(" ++ (exprToString lexpr) ++ (exprToString rexpr) ++ ")"
--   Frac lexpr rexpr -> "(" ++ (exprToString lexpr) ++ "/" ++ (exprToString rexpr) ++ ")"
--   Pow lexpr n -> "(" ++ (exprToString lexpr) ++ "^" ++ show n ++ ")"
--   Var c -> [c]
--   Const n -> show n

exprToString = go 0
  where
    go _ (Const n) = show n
    go _ (Var c) = [c]

    go p e@(Add lexpr rexpr) = 
      paranthesise (p > 1) $ (go (pTable e) lexpr) ++ " + " ++ (go (pTable e) rexpr)
    go p e@(Mul lexpr rexpr) =
      paranthesise (p > 2) $ (go (pTable e) lexpr) ++ (go (pTable e) rexpr)
    go _ e@(Frac lexpr rexpr) =
       paranthesise True $ (go (pTable e) lexpr) ++ " / " ++ (go (pTable e) rexpr)
    go p e@(Pow lexpr n) =
       paranthesise (p > 3) $ (go (pTable e) lexpr) ++ "^" ++ show n
    
    paranthesise True s = "(" ++ s ++ ")"
    paranthesise False s = s