module Main (main) where

import Parser

-- Parse given user input into command and expression
parseInput :: String -> (String, String)
parseInput userInput = case (words userInput) of
  (cmd:exprWords) -> (cmd, concat exprWords)
  _               -> ("", "")

main :: IO ()
main = do
  userInput <- getLine
  let (cmd, exprString) = parseInput userInput
  putStrLn (show $ genExpr exprString)