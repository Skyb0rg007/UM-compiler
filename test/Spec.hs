{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Parser                                   ( instruction
                                                          , instructions
                                                          )
import           WriteAST                                 ( astToWord )
import           AST                                      ( UM(..)
                                                          , UMInstruction(..)
                                                          , Reg(..)
                                                          )
import           Test.Hspec
import           Data.Attoparsec.Text                     ( parseOnly )
import           Data.Either                              ( isLeft )

main :: IO ()
main = hspec $ do
    describe "Parser.instruction" $ do
        it "correctly parses $r[1] register syntax"
            $          parseOnly instruction "$r[1] = 10"
            `shouldBe` Right (LoadVal 1 10)
        it "correctly parses R1 register syntax"
            $          parseOnly instruction "R1 = R2+R3"
            `shouldBe` Right (Add 1 2 3)
    describe "WriteAST.astToWord" $ do
        it "correctly converts a halt instruction to bytecode"
            $          astToWord Halt
            `shouldBe` 0x70000000
        it "correctly converts a load value instruction to bytecode"
            $          astToWord (LoadVal 0 0xbeef)
            `shouldBe` 0xd000beef
    describe "Parser.instructions" $ do
        it "correctly parses multi-command statements" $ do
            let command = "R0 = R1 + R2; load_prog($m[R1][R2])"
            parseOnly instructions command
                `shouldBe` Right (UM [Add 0 1 2, LoadProg 1 2])
        it "fails to parse empty statements" $ do
            let command = ""
            parseOnly instructions command `shouldSatisfy` isLeft
