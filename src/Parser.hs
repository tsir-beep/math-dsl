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
  | otherwise = error "lexer: unaccepted character"
    where 
      inner varName afterVar = case afterVar of
        (x:_) | isAlpha x -> [TVar varName, TProd] ++ lexer afterVar
        _                  -> TVar varName : lexer afterVar
        
-- Token stream monad to keep track of remaining tokens as we build Expr
type Parser a = State [Token] a

-- Scan next token without consuming
peek :: Parser Token
peek = gets head

-- Return and eat next token
advance :: Parser Token
advance = do
  tokens <- get
  case tokens of
    [] -> error "advance: empty token stream" -- Not sure how to handle this yet
    (t:ts) -> do
      put ts
      return t

-- Null denotation (denote on constants and parantheses)
-- I.e., start of an Expr
nud :: Token -> Parser Expr
nud (TNum n) = return $ Const n
nud (TVar c) = return $ Var c
nud tok = error ("nud: invalid token: " ++ show tok) 

-- Left denotation (denote on the left expression formed)
led :: Expr -> Token -> Parser Expr
led leftExpr tok = case tok of
  TPlus -> do
    rightExpr <- parseExpr (bindPower tok)
    return $ Add leftExpr rightExpr
  TProd -> do
    rightExpr <- parseExpr (bindPower tok)
    return $ Mul leftExpr rightExpr
  TSlash -> do
    rightExpr <- parseExpr (bindPower tok)
    return $ Frac leftExpr rightExpr
  TPow -> do
    rightExpr <- parseExpr (bindPower tok)
    case rightExpr of
      Const n -> return $ Pow leftExpr n
      _       -> error "exponent must be numerical"
  otherTok -> error ("led: invalid token: " ++ show otherTok)

-- Parse the expression through the token stream
parseExpr :: Int -> Parser Expr
parseExpr minBP = do
  tok <- advance
  leftExpr <- nud tok
  loop leftExpr
    where
      loop leftExpr = do
        tok <- peek
        if bindPower tok <= minBP
          then return leftExpr
        else do
          _ <- advance
          leftExpr' <- led leftExpr tok
          loop leftExpr'

-- Generate Expr from tokenized string
genExpr :: String -> Expr
genExpr s = evalState (parseExpr 0) $ lexer s

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