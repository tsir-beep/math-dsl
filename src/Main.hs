module Main (main) where

import Parser
import Printer
import Simplify
import Factor
import Eval

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
      putStrLn (exprToString $ simplify expr)

  else if cmd == "FACTOR"
    then do
      putStrLn (exprToString $ gcf $ simplify expr)

  else if (take 4 cmd) == "EVAL"
    then do
      let varMap = init $ drop 5 cmd
      putStrLn (show $ evalExpr expr $ parseEval varMap)

  else if cmd == "DEBUG"
    then do
      putStrLn ("Inputted expression: ")
      putStrLn (show $ expr)
      putStrLn (exprToString expr)
      putStrLn ""
      putStrLn ("Outputted expression: ")
      putStrLn (show $ simplify expr)
      putStrLn (exprToString $ simplify expr)

  else do
    putStrLn ("Invalid command")