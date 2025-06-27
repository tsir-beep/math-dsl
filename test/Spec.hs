module Main (main) where

import Test.Hspec
import Parser

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
