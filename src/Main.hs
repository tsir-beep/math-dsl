module Main (main) where

import Parser
import Printer
import Simplify
import Factor
import Eval
import Differentiate
import Data.List.Split

-- Parse given user input into command and expression
parseInput :: String -> IO (String, String)
parseInput userInput = do
  if (take 4 userInput) == "EVAL"
    then do
      case (splitOn ") " userInput) of
        [cmd, exprWords] -> return $ (cmd ++ ")", exprWords)
        _                -> error "Incorrect input to DSL"
    else do
     case (words userInput) of
      (cmd:exprWords) -> return $ (cmd, concat exprWords)
      _               -> error "Incorrect input to DSL"

main :: IO ()
main = do
  putStrLn "Math-DSL --- Type SIMPLIFY, FACTOR, EVAL, or DIFF followed by your expression. Ctrl+C to quit."
  
  userInput <- getLine
  (cmd, exprString) <- parseInput userInput
  let expr = genExpr exprString
  
  putStrLn "Result:"

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

  else if (take 4 cmd) == "DIFF"
    then do
      let wrt = head $ drop 5 cmd
      putStrLn (exprToString $ simplify $ diffExpr (simplify expr) wrt)

  else if cmd == "DEBUG"
    then do
      putStrLn ("Inputted expression: ")
      putStrLn (show $ expr)
      putStrLn (exprToString expr)
      putStrLn ""
      putStrLn ("Outputted expression: ")
      putStrLn (show $ simplify expr)
      putStrLn (exprToString $ simplify expr)
      putStrLn ""
      putStrLn ("Differentiated expression: ")
      putStrLn (show $ simplify $ diffExpr expr 'x')
      putStrLn (exprToString $ simplify $ diffExpr expr 'x')

  else do
    putStrLn ("Invalid command")