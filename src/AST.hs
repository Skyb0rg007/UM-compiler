{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AST
    ( UM(..)
    , UMInstruction(..)
    , Reg(..)
    )
where

import           Data.List                                ( intercalate )

newtype Reg = Reg Int
    deriving (Eq, Num)

instance Show Reg where
    show (Reg n) = "$r[" ++ show n ++ "]"

data UMInstruction =
        CondMove Reg Reg Reg
      | SegLoad Reg Reg Reg
      | SegStore Reg Reg Reg
      | Add Reg Reg Reg
      | Mult Reg Reg Reg
      | Div Reg Reg Reg
      | Nand Reg Reg Reg
      | Halt
      | MapSeg Reg Reg
      | UnMapSeg Reg
      | Output Reg
      | Input Reg
      | LoadProg Reg Reg
      | LoadVal Reg Int
      deriving (Eq)

instance Show UMInstruction where
    show (CondMove a b c) =
        "if (" ++ show c ++ " != 0) { " ++ show a ++ " == " ++ show b ++ " }"
    show (SegLoad a b c) =
        show a ++ " = $m[" ++ show b ++ "][" ++ show c ++ "]"
    show (SegStore a b c) =
        "$m[" ++ show a ++ "][" ++ show b ++ "] = " ++ show c
    show (Add a b c)  = showOp "+" a b c
    show (Mult a b c) = showOp "*" a b c
    show (Div a b c)  = showOp "/" a b c
    show (Nand a b c) = showOp "~&~" a b c
    show Halt = "HALT"
    show (MapSeg b c) = show b ++ " = alloc(" ++ show c ++ ")"
    show (UnMapSeg c) = "free(" ++ show c ++ ")"
    show (Output c) = "print(" ++ show c ++ ")"
    show (Input c) = show c ++ " = input()"
    show (LoadProg b c) = "load_prog($m[" ++ show b ++ "][" ++ show c ++ "])"
    show (LoadVal a v) = show a ++ " = " ++ show v

showOp :: String -> Reg -> Reg -> Reg -> String
showOp op a b c = show a ++ " = " ++ show b ++ " " ++ op ++ " " ++ show c

newtype UM = UM [UMInstruction]
    deriving (Eq)

instance Show UM where
    show (UM instructions) = intercalate "\n" $ show <$> instructions

