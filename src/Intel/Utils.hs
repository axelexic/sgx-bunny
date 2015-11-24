module Intel.Utils (toHexRep, toBinaryRep) where

import Text.Printf
import Data.Bits
import Data.Binary
import qualified Data.ByteString.Lazy as L

toHexRep :: L.ByteString -> String
toHexRep bs = concatMap toHexStr (L.unpack bs)
  where
    toHexStr :: Word8 -> String
    toHexStr = printf "%.2x"


toBinaryRep :: L.ByteString -> String
toBinaryRep bs = concatMap toBinStr (L.unpack bs)
  where
    toBinStr :: Word8 -> String
    toBinStr x = [bitVal x (7-i) | i <- [0..7]]

    bitVal :: Word8 -> Int -> Char
    bitVal x l = let val  = (x `unsafeShiftR` l) .&. 0x1
                 in if val == 1
                    then '1'
                    else '0'
