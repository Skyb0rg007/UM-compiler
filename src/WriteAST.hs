module WriteAST (astToFile, astToWord) where

import           AST                                      ( UM(..)
                                                          , UMInstruction(..)
                                                          , Reg(..)
                                                          )

import           Data.Binary.Put                          ( runPut
                                                          , putWord32be
                                                          )
import           Data.Bits                                ( (.&.)
                                                          , (.|.)
                                                          , shiftL
                                                          )
import qualified Data.ByteString.Lazy          as LBS
                                                          ( writeFile )
import           Data.Word                                ( Word32 )


infixl 8 .<<.
(.<<.) :: Word32 -> Int -> Word32
(.<<.) = shiftL

threeRegister :: Word32 -> Reg -> Reg -> Reg -> Word32
threeRegister op (Reg a) (Reg b) (Reg c) =
    let a' = (toEnum a :: Word32) .&. 0x7
        b' = (toEnum b :: Word32) .&. 0x7
        c' = (toEnum c :: Word32) .&. 0x7
    in  op .<<. 28 .|. a' .<<. 6 .|. b' .<<. 3 .|. c'

twoRegister :: Word32 -> Reg -> Int -> Word32
twoRegister op (Reg a) val =
    let op'  = op .<<. 28
        a'   = ((toEnum a .&. 0x7) .<<. 25)
        val' = (toEnum val :: Word32) .&. (2 ^ 25 - 1)
    in  op .|. a' .|. val'

astToWord :: UMInstruction -> Word32
astToWord (CondMove a b c) = threeRegister 0x0 a b c
astToWord (SegLoad  a b c) = threeRegister 0x1 a b c
astToWord (SegStore a b c) = threeRegister 0x2 a b c
astToWord (Add      a b c) = threeRegister 0x3 a b c
astToWord (Mult     a b c) = threeRegister 0x4 a b c
astToWord (Div      a b c) = threeRegister 0x5 a b c
astToWord (Nand     a b c) = threeRegister 0x6 a b c
astToWord Halt             = threeRegister 0x7 0 0 0
astToWord (MapSeg b c    ) = threeRegister 0x8 0 b c
astToWord (UnMapSeg c    ) = threeRegister 0x9 0 0 c
astToWord (Output   c    ) = threeRegister 0xa 0 0 c
astToWord (Input    c    ) = threeRegister 0xb 0 0 c
astToWord (LoadProg b c  ) = threeRegister 0xc 0 b c
astToWord (LoadVal  a val) = twoRegister 0xd a val

astToFile :: UM -> FilePath -> IO ()
astToFile (UM ast) filename = do
    let bs = runPut $ mconcat $ putWord32be . astToWord <$> ast
    LBS.writeFile filename bs
