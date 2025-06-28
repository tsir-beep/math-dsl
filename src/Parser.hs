module Parser (Expr(..), genExpr) where

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
      deriving (Show, Eq, Ord)

-- Tokens
data Token = 
  TNum Int
  | TVar Char
  | TPow
  | TPlus
  | TMinus
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
bindPower TMinus = 10
bindPower TProd = 30
bindPower TSlash = 20
bindPower _ = 0

-- Lexer (tokenize a string)
lexer :: String -> [Token]
lexer [] = [TEOF]
lexer s@(c:cs)
  | isSpace c = lexer cs
  | isDigit c = let (constant, rest) = span isDigit s
                -- in TNum (read constant) : lexer rest
                in innerNum constant rest
  | isAlpha c = let (varName, afterVar) = (c, tail s)
                in innerVar varName afterVar 
  | c == '+' = TPlus : lexer cs
  | c == '-' = TMinus : lexer cs
  | c == '*' = TProd : lexer cs
  | c == '/' = TSlash : lexer cs
  | c == '^' = TPow : lexer cs
  | c == '(' = TLParen : lexer cs
  | c == ')' = let (_, afterParen) = (c, tail s)
               in innerRParen afterParen
  | otherwise = error "lexer: unaccepted character"
    where 
      innerNum constant rest = case rest of
        (x:_) | isAlpha x || x == '(' -> [TNum (read constant), TProd] ++ lexer rest
        _                 -> TNum (read constant) : lexer rest
      innerVar varName afterVar = case afterVar of
        (x:_) | isAlpha x || x == '(' -> [TVar varName, TProd] ++ lexer afterVar
        _                 -> TVar varName : lexer afterVar
      innerRParen afterParen = case afterParen of
        (x:_) | x == '(' -> [TRParen, TProd] ++ lexer afterParen
        _                 -> TRParen : lexer afterParen
        
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
nud TMinus = do
  expr <- parseExpr (bindPower TMinus + 40) -- Need higher binding power for nud
  return (Mul (Const (-1)) expr)
nud TLParen = do
  innerExpr <- parseExpr 0
  tok <- advance
  case tok of
    TRParen -> return innerExpr
    _       -> error ("nud: expected closing paranthesis, got: " ++ show tok)
nud tok = error ("nud: invalid token: " ++ show tok) 

-- Left denotation (denote on the left expression formed)
led :: Expr -> Token -> Parser Expr
led leftExpr tok = case tok of
  TPlus -> do
    rightExpr <- parseExpr (bindPower tok)
    return $ Add leftExpr rightExpr
  TMinus -> do
    rightExpr <- parseExpr (bindPower tok)
    return $ Add leftExpr (Mul (Const (-1)) (rightExpr))
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