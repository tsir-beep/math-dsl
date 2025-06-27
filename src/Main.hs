module Main (main) where

import Parser
import Printer
import Simplify
import Factor

-- Parse given user input into command and expression
parseInput :: String -> (String, String)
parseInput userInput = case (words userInput) of
  (cmd:exprWords) -> (cmd, concat exprWords)
  _               -> error "Incorrect input to DSL"

main :: IO ()
main = do
  userInput <- getLine
  let (cmd, exprString) = parseInput userInput
  let expr = genExpr exprString
  
  if cmd == "SIMPLIFY"
    then do
      putStrLn (show $ expr)
      putStrLn (exprToString expr)
      putStrLn (show $ simplify expr)
      putStrLn (show $ simplifyFractions $ simplify expr)
      putStrLn (exprToString $ simplifyFractions $ simplify expr)
  else if cmd == "FACTOR"
    then do
      putStrLn (show $ simplifyFractions $ simplify expr)
      putStrLn (exprToString $ gcf $ simplifyFractions $ simplify expr)
  else do
    putStrLn ("Invalid command")