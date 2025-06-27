module Printer (exprToString) where

import Parser

-- Precedence table for paranthesising
pTable :: Expr -> Int
pTable (Add _ _) = 0
pTable (Frac _ _) = 1
pTable (Mul _ _) = 2
pTable (Pow _ _) = 3
pTable (Const _) = 4
pTable (Var _) = 4

-- This is pretty much a CFG
exprToString :: Expr -> String
exprToString = go 0
  where
    go _ (Const n) = show n
    go _ (Var c) = [c]

    go p e@(Add lexpr rexpr) = 
      paranthesise (p > 0) $ (go (pTable e) lexpr) ++ " + " ++ (go (pTable e) rexpr)
    go p e@(Mul lexpr rexpr) =
      paranthesise (p > 2) $ (go (pTable e) lexpr) ++ (go (pTable e) rexpr)
    go p e@(Frac lexpr rexpr) =
       paranthesise (p > 0) $ (go (pTable e) lexpr) ++ "/" ++ (go (pTable e) rexpr)
    go p e@(Pow lexpr n) =
       paranthesise (p > 3) $ (go (pTable e) lexpr) ++ "^" ++ show n
    
    paranthesise True s = "(" ++ s ++ ")"
    paranthesise False s = s