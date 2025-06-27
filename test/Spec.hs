module Main (main) where

import Test.Hspec
import Parser
import Printer
import Simplify
import Factor
import Eval

simplifyThenPrint :: String -> String
simplifyThenPrint = exprToString . simplify . genExpr

gcfThenPrint :: String -> String
gcfThenPrint = exprToString . gcf . simplify . genExpr

parseThenEval :: String -> String -> Double
parseThenEval evalMap exprString = evalExpr (genExpr exprString) (parseEval evalMap) 

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "Parse a single variable" $ do
      genExpr "x" `shouldBe` (Var 'x')

    it "Parse a constant of multiple digits" $ do
      genExpr "123" `shouldBe` Const 123
      genExpr "1000x" `shouldBe` Mul (Const 1000) (Var 'x')

    it "Parses simple expressions" $ do
      genExpr "1 + xy + x^2y^2z" `shouldBe` Add
                                              (Add 
                                                (Const 1) 
                                                (Mul 
                                                  (Var 'x') (Var 'y'))) 
                                              (Mul 
                                                (Mul 
                                                  (Pow (Var 'x') 2) 
                                                  (Pow (Var 'y') 2)) 
                                                (Var 'z'))
      genExpr "x + (x+y)/(2y)" `shouldBe` Add 
                                            (Var 'x')
                                            (Frac
                                              (Add
                                                (Var 'x')
                                                (Var 'y'))
                                              (Mul
                                                (Const 2)
                                                (Var 'y')))

    it "Parses subexpressions" $ do
      genExpr "(x+1)^2 + 3(x+1)(x+2) + x" `shouldBe` Add 
                                                      (Add 
                                                        (Pow (Add (Var 'x') (Const 1)) 2) 
                                                        (Mul 
                                                          (Mul 
                                                            (Const 3) 
                                                            (Add 
                                                              (Var 'x') 
                                                              (Const 1))) 
                                                          (Add 
                                                            (Var 'x') 
                                                            (Const 2)))) 
                                                      (Var 'x')

  describe "Simplify" $ do
    it "Simplifies basic product terms" $ do
      simplifyThenPrint "xxx" `shouldBe` "x^3"
      simplifyThenPrint "3xx^2y^2x^3zy" `shouldBe` "3x^6y^3z"

    it "Simplifies product terms containing subexpressions" $ do
      simplifyThenPrint "20(x+1)(x+2)^5(x+1)^2(x+1)(x+2)" `shouldBe` "20(x + 1)^4(x + 2)^6"

    it "Simplifies an addition expression" $ do
      simplifyThenPrint "x + x + 2 + 6" `shouldBe` "2x + 8"
      simplifyThenPrint "20x + 50y + 80x + 40y" `shouldBe` "100x + 90y"
      simplifyThenPrint "(x+1)^2 + 3(x+1)^2 + x + 1 + x + 1" `shouldBe` "2x + 4(x + 1)^2 + 2" 

    it "Simplifies fractions" $ do
      simplifyThenPrint "(5x^2y)/(10xy^5)" `shouldBe` "x/2y^4"
      simplifyThenPrint "(x+1)^2 + 3(x+1)^2 + (2x^4z)/(10x) + x + 1 + x + 1 + (3x^5y)/(3xy)" `shouldBe` "2x + x^4 + 4(x + 1)^2 + x^3z/5 + 2" 
      simplifyThenPrint "x + (xy)/y + 2" `shouldBe`"2x + 2" 

    it "Simplifies fractions containing subexpressions" $ do
      simplifyThenPrint "(2(x+1)^2)/(4(x+2)(x+1))" `shouldBe` "(x + 1)/2(x + 2)"

    -- it "Simplifies implicit multiplication over fractions" $ do
    --   genExpr "((2xy/x^3)(4xy/8yz))" `shouldBe` Mul
    --                                               (Frac 
    --                                                 (Mul
    --                                                   (Mul
    --                                                     (Const 2)
    --                                                     (Var 'x'))
    --                                                   (Var 'y'))
    --                                                 (Pow (Var 'x') 3))
    --                                               (Frac
    --                                                 (Mul
    --                                                   (Mul
    --                                                     (Const 4)
    --                                                     (Var 'x'))
    --                                                   (Var 'y'))
    --                                                 (Mul
    --                                                   (Mul
    --                                                     (Const 8)
    --                                                     (Var 'y'))
    --                                                   (Var 'z')))
    --   (exprToString $ genExpr "((2xy/x^3)(4xy/8yz))") `shouldBe` "((2xy/x^3)(4xy/8yz))"
    --   simplifyThenPrint "((2xy/x^3)(4xy/8yz))" `shouldBe` "(2y/x^2)(x/2z)"

    it "Simplifies identity elements in expressions" $ do
      simplifyThenPrint "x + 0" `shouldBe` "x"
      simplifyThenPrint "1x" `shouldBe` "x"
      simplifyThenPrint "x/1" `shouldBe` "x"
      simplifyThenPrint "(x+1)^1 + x^1" `shouldBe` "2x + 1"

  describe "Factor" $ do
    it "Gives the factor of expressions" $ do
      gcfThenPrint "x^3y + x^2xz" `shouldBe` "x^3"
      gcfThenPrint "(x+1)^2 + 2(x+1)(x+2)" `shouldBe`"x + 1"
      gcfThenPrint "2(x+1)^3 + 4x(x+1) + (2(x+1)^3)/(3y(x+1)(x+2))" `shouldBe`"2(x + 1)"  
    
    it "Gives a factor containing multiple subexpressions" $ do
      gcfThenPrint "2(x+1)(x+2)(x+2)(x+1) + 10(x+1)^2(x+2)(x+1)(x+2)(x+3)" `shouldBe` "2(x + 1)^2(x + 2)^2" 

  describe "Eval" $ do
    it "Evaluates expressions" $ do
      parseThenEval "x=5, y=2" "x+2y+x+10" `shouldBe` 24.0
      parseThenEval "x=2,y=0"  "xy" `shouldBe` 0.0
      parseThenEval "x=2, y=4" "x^4 + (x+y+2)^2" `shouldBe` 80.0
      parseThenEval "x=4, y=8" "x/y" `shouldBe` 0.50