{-# LANGUAGE OverloadedStrings #-}
module Parser (instruction, instructions) where

import           AST                                      ( UM(..)
                                                          , UMInstruction(..)
                                                          , Reg(..)
                                                          )

import           Prelude                           hiding ( takeWhile )
import           Control.Applicative                      ( (<|>) )
import           Data.Attoparsec.Text                     ( satisfy
                                                          , char
                                                          , takeWhile
                                                          , skipSpace
                                                          , digit
                                                          , Parser
                                                          , decimal
                                                          , string
                                                          , space
                                                          , asciiCI
                                                          )
import           Data.Attoparsec.Combinator               ( sepBy1 )
import           Data.Char                                ( isDigit
                                                          , isAlpha
                                                          , digitToInt
                                                          )
import           Data.Functor                             ( ($>) )
import           Data.Text                                ( Text
                                                          , cons
                                                          )

-- identifier :: Parser Text
-- identifier = do
    -- h <- satisfy isAlpha
    -- t <- takeWhile isIdentifier
    -- return (h `cons` t)
    -- where
        -- isIdentifier :: Char -> Bool
        -- isIdentifier a = isDigit a || isAlpha a || a == '_'

skipSpace1 :: Parser ()
skipSpace1 = space *> skipSpace

register :: Parser Reg
register = do
    let dollarVersion = do
            string "$r["
            n <- digitToInt <$> digit
            char ']'
            return n
        capitalVersion = do
            string "R"
            read . pure <$> digit
    n <- dollarVersion <|> capitalVersion :: Parser Int
    if n >= 8 then fail "registers only valid when 0-7" else return (Reg n)

memoryLoc :: Parser (Reg, Reg)
memoryLoc = do
    (string "$m[" <|> string "M[") *> skipSpace
    reg1 <- register
    skipSpace *> string "][" *> skipSpace
    reg2 <- register
    skipSpace *> string "]" *> skipSpace
    return (reg1, reg2)


instruction :: Parser UMInstruction
instruction =
    parseCondMove
        <|> parseSegLoad
        <|> parseSegStore
        <|> parseOp "+"   Add
        <|> parseOp "/"   Div
        <|> parseOp "*"   Mult
        <|> parseOp "~&~" Nand
        <|> parseHalt
        <|> parseMapSeg
        <|> parseUnmapSeg
        <|> parseOutput
        <|> parseInput
        <|> parseLoadProgram
        <|> parseLoadVal
    where
        parseCondMove :: Parser UMInstruction
        parseCondMove = do
            skipSpace *> string "if" *> skipSpace *> string "(" *> skipSpace
            reg1 <- register
            skipSpace
                <* "!="
                <* skipSpace
                <* string "0"
                <* skipSpace
                <* string ")"
                <* skipSpace
            reg2 <- register
            skipSpace *> string "=" *> skipSpace
            reg3 <- register
            skipSpace
            return $ CondMove reg2 reg3 reg1
        parseSegLoad :: Parser UMInstruction
        parseSegLoad = do
            skipSpace
            reg1 <- register
            skipSpace *> string "=" *> skipSpace
            (reg2, reg3) <- memoryLoc
            skipSpace
            return $ SegLoad reg1 reg2 reg3
        parseSegStore :: Parser UMInstruction
        parseSegStore = do
            skipSpace
            (reg1, reg2) <- memoryLoc
            skipSpace *> string "=" *> skipSpace
            reg3 <- register
            skipSpace
            return $ SegStore reg1 reg2 reg3
        parseOp
            :: Text
            -> (Reg -> Reg -> Reg -> UMInstruction)
            -> Parser UMInstruction
        parseOp op constructor = do
            skipSpace
            reg1 <- register
            skipSpace *> string "=" *> skipSpace
            reg2 <- register
            skipSpace *> string op *> skipSpace
            reg3 <- register
            skipSpace
            return $ constructor reg1 reg2 reg3
        parseHalt :: Parser UMInstruction
        parseHalt = skipSpace *> asciiCI "halt" *> skipSpace $> Halt
        parseMapSeg :: Parser UMInstruction
        parseMapSeg = do
            skipSpace
            reg1 <- register
            skipSpace *> string "=" *> skipSpace *> string "alloc" *> skipSpace1
            reg2 <- register
            skipSpace
            return $ MapSeg reg1 reg2
        parseUnmapSeg :: Parser UMInstruction
        parseUnmapSeg = do
            skipSpace *> string "free" *> skipSpace1
            reg1 <- register
            skipSpace
            return $ UnMapSeg reg1
        parseOutput :: Parser UMInstruction
        parseOutput = do
            skipSpace *> string "print" *> skipSpace1
            reg1 <- register
            skipSpace
            return $ Output reg1
        parseInput :: Parser UMInstruction
        parseInput = do
            skipSpace
            reg1 <- register
            skipSpace
                *> string "="
                *> skipSpace
                *> string "input()"
                *> skipSpace
            return $ Input reg1
        parseLoadProgram :: Parser UMInstruction
        parseLoadProgram = do
            skipSpace *> string "load_prog(" *> skipSpace
            (reg1, reg2) <- memoryLoc
            skipSpace *> string ")" *> skipSpace
            return $ LoadProg reg1 reg2
        parseLoadVal :: Parser UMInstruction
        parseLoadVal = do
            skipSpace
            reg1 <- register
            skipSpace *> string "=" *> skipSpace
            val <- decimal
            skipSpace
            return $ LoadVal reg1 val

instructions :: Parser UM
instructions = UM <$> sepBy1 instruction (char ';')

