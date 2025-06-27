module Eval (evalExpr, parseEval) where

import Parser
import qualified Data.Map as Map
import Data.List.Split
import Data.Char

-- Parse evaluation command
parseEval :: String -> Map.Map Char Int
parseEval varMap = Map.fromList $ go $ splitOn "," $ filter (not . isSpace) varMap
  where
    go ls = [(x, read n) | (x:'=':n) <- ls]

-- Evaluate an expression by subbing variables
evalExpr :: Expr -> Map.Map Char Int -> Double
evalExpr expr varMap = fromIntegral (round ((go expr) * 100) :: Integer)/ 100.0
  where
    go :: Expr -> Double
    go (Mul lexpr rexpr) = (go lexpr) * (go rexpr)
    go (Add lexpr rexpr) = (go lexpr) + (go rexpr)
    go (Frac nexpr dexpr) = (go nexpr) / (go dexpr)
    go (Pow e n) = (go e)^n
    go (Var c) = case (Map.lookup c varMap) of
      (Just i) -> fromIntegral i
      Nothing  -> error "Given variable not in expression"
    go (Const n) = fromIntegral n