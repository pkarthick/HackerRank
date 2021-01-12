module SimplifySpec where

import Test.Hspec
import Simplify

main :: IO ()
main = hspec $
        describe "Simplify" $ do
            it "Test 0" $ do
                let expressions = 
                        [
                            "10x + 2x - (3x + 6)/3",
                            "18*(2x+2) - 5 ",
                            "((9x + 81)/3 + 27)/3  - 2x",
                            "18x + (12x + 10)*(2x+4)/2 - 5x",
                            "(2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1)  - 2x",
                            "(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - 2x"
                        ]

                let expected = 
                        [
                            "11x - 2",
                            "36x + 31",
                            "-x + 18",
                            "12x^2 + 47x + 20",
                            "2x^3 + 23x^2 + 61x + 45",
                            "2x^5 + 5x^4 + 18x^2 + 61x + 45" ]

                actual <- mapM simplifyExpression expressions
                actual `shouldBe` expected


            it "Test 1" $ do
                let expressions = 
                        [   "- (4x + 8)/4 + 10x + 2x", 
                            "- 5 + 9*(4x+4)", 
                            "-3x + ((9x + 81)/3 + 27)/3", 
                            "- 5x + 18x + (24x + 20)*(2x+4)/4", 
                            "(2x+5) * (x*(3x+27) + 27)/3 - x", 
                            "(2x+5) * (x*(3x^3 + 27) + 27)/3 - x" ]

                let expected = 
                        [
                            "11x - 2",
                            "36x + 31",
                            "-2x + 18",
                            "12x^2 + 47x + 20",
                            "2x^3 + 23x^2 + 62x + 45",
                            "2x^5 + 5x^4 + 18x^2 + 62x + 45" ]

                actual <- mapM simplifyExpression expressions
                actual `shouldBe` expected

            it "Test 2" $ do
                let expressions = 
                        [   
                            "x + 2x - (3x + 6)/3",
                            "(1+7+10)*(2x+2) - 5",
                            "((9x^5 + 81)/3 + 27)/3  - 2x",
                            "18x^2 + (2x + 10)*(2x+4)/2 - 5x",
                            "(2x+5) * (x^2*(9x + 81)/3 + 27)/(1+1+1)  - 2x",
                            "(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - x^3",
                            "(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1) - x^3 + 2x - (3x + 6)/3",
                            "((9x^5 + 81)/3 + 27)/3  - 2x + 18x^2 + (2x + 10)*(2x+4)/2 - 5x",
                            "18x^2 + (2x + 10)*(2x+4)/2 - 5x + 20x^2 + 30x(2x+1)",
                            "15x(2x+1) + (2x+5) * (x^2*(9x + 81)/3 + 27)/3  - 2x" ]

                let expected = 
                        [
                            "2x - 2",
                            "36x + 31",
                            "x^5 - 2x + 18",
                            "20x^2 + 9x + 20",
                            "2x^4 + 23x^3 + 45x^2 + 16x + 45",
                            "2x^5 + 5x^4 - x^3 + 18x^2 + 63x + 45",
                            "2x^5 + 5x^4 - x^3 + 18x^2 + 64x + 43",
                            "x^5 + 20x^2 + 7x + 38",
                            "100x^2 + 39x + 20",
                            "2x^4 + 23x^3 + 75x^2 + 31x + 45" ]

                actual <- mapM simplifyExpression expressions
                actual `shouldBe` expected

            it "Test 3" $ do
                let expressions = 
                        [
                            "10x/(1+8/(1+2/(1+1)))",
                            "24x^2/(2+8/(1+2/(1+1)))"
                        ]   
                let expected = 
                        [
                            "2x",
                            "4x^2"
                        ]
                actual <- mapM simplifyExpression expressions
                actual `shouldBe` expected
