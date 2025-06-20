module Parser where

import Data.List.Split
import Data.Char
import Control.Monad.State

-- AST to represent mathematical expressions
data Expr = 
  Const Int 
  | Var Char 
  | Pow Expr Int
  | Add Expr Expr
  | Mul Expr Expr
  | Frac Expr Expr
      deriving Show

-- Tokens
data Token = 
  TNum Int
  | TVar Char
  | TPow
  | TPlus
  | TProd
  | TSlash
  | TLParen
  | TRParen
  | TEOF
    deriving Show

-- Binding power lookup
bindPower :: Token -> Int
bindPower TPow = 40
bindPower TPlus = 10
bindPower TProd = 20
bindPower TSlash = 30
bindPower _ = 0

-- Lexer (tokenize a string)
lexer :: String -> [Token]
lexer [] = [TEOF]
lexer s@(c:cs)
  | isSpace c = lexer cs
  | isDigit c = let (constant, rest) = span isDigit s
                in TNum (read constant) : lexer rest
  | isAlpha c = let (varName, afterVar) = (c, tail s)
                in inner varName afterVar 
  | c == '+' = TPlus : lexer cs
  | c == '*' = TProd : lexer cs
  | c == '/' = TSlash : lexer cs
  | c == '^' = TPow : lexer cs
  | c == '(' = TLParen : lexer cs
  | c == ')' = TRParen : lexer cs
  | otherwise = [TEOF]
    where 
      inner varName afterVar
        | isAlpha $ head afterVar = [TVar varName, TProd] ++ lexer afterVar
        | otherwise = TVar varName : lexer afterVar
        
-- Token stream monad to keep track of remaining tokens as we build Expr
type TokenStream a = State [Token] a

-- Scan next token without consuming
peek :: TokenStream Token
peek = gets head

-- Return and eat next token
advance :: TokenStream Token
advance = do
  tokens <- get
  case tokens of
    [] -> return TEOF -- Not sure how to handle this yet
    (t:ts) -> do
      put ts
      return t

-- -- Generate Expr from String
-- genExpr :: String -> Expr
-- genExpr exprString
--   |'/' `elem` exprString =   -- Check for '/' to generate Frac
--     let 
--       [numerator, denominator] = splitOn "/" exprString

--       -- Recursively generate numerator and denominator
--       numExpr = genExpr numerator
--       denExpr = genExpr denominator
--     in Frac numExpr denExpr
    
--   | '+' `elem` exprString = -- Check for '+' to generate Add
--     let operands = splitOn "+" exprString
--     in Add (map genExpr operands) -- Recurse on all addition operands  

--   | otherwise =
--     -- Generate product expression
--     parseProduct exprString

-- -- Parse a product expression into Expr
-- parseProduct :: String -> Expr
-- parseProduct exprString = Mul (go exprString)
--   where
--     go "" = []
--     go s@(c:_)
--       | isDigit c = -- Read constant
--         let (constant, rest) = span isDigit s
--         in Const (read constant) : go rest
--       | otherwise = -- Read variable
--         let (varName, afterVar) = (c, tail s)
--         in case afterVar of
--           ('^':restExpr) -> -- Read exponent
--             let (exponent, rest) = span isDigit restExpr
--             in Pow (Var varName) (read exponent) : go rest
--           _ -> Var varName : go afterVar