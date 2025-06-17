module Main (main) where

import Data.List
import Data.List.Split
import Data.Char

-- AST to represent mathematical expressions
data Expr = 
  Const Int 
  | Var Char 
  | Pow Expr Int
  | Add [Expr]
  | Mul [Expr]
  | Frac Expr Expr
      deriving Show

-- Parse given user input into command and expression
parseInput :: String -> (String, String)
parseInput userInput = case (words userInput) of
  (cmd:exprWords) -> (cmd, concat exprWords)
  _               -> ("", "")
  
-- Generate Expr from String
genExpr :: String -> Expr
genExpr exprString
  |'/' `elem` exprString =   -- Check for '/' to generate Frac
    let 
      [numerator, denominator] = splitOn "/" exprString

      -- Recursively generate numerator and denominator
      numExpr = genExpr numerator
      denExpr = genExpr denominator
    in Frac numExpr denExpr
  | '+' `elem` exprString =
    let
      operands = splitOn "+" exprString

    in Add (map genExpr operands) -- Recurse on all addition operands  

  | otherwise =
    -- Generate product expression
    parseProduct exprString

-- Parse a product expression into Expr
parseProduct :: String -> Expr
parseProduct exprString = Mul (go exprString)
  where
    go "" = []
    go s@(c:_)
      | isDigit c = -- Read constant
        let (constant, rest) = span isDigit s
        in Const (read constant) : go rest
      | otherwise = -- Read variable
        let (varName, afterVar) = (c, tail s)
        in case afterVar of
          ('^':restExpr) -> -- Read exponent
            let (exponent, rest) = span isDigit restExpr
            in Pow (Var varName) (read exponent) : go rest
          _ -> Var varName : go afterVar

main :: IO ()
main = do
  userInput <- getLine
  let (cmd, exprString) = parseInput userInput
  putStrLn (show $ genExpr exprString)