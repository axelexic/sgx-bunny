{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module CryptoLatte.RSA
       (
         RSAPrivateKey(..)
       , RSAPublicKey(..)
       , newRSAPrivateKey
       , os2i
       , i2os
       )where

import CryptoLatte.Random
import CryptoLatte.CryptoExceptions
import Data.Bits ( unsafeShiftL, unsafeShiftR, (.|.) , (.&.) )
import Data.Word                     (Word32, Word8)
import GHC.Integer.GMP.Internals     (gcdExtInteger, sizeInBaseInteger,
                                      recipModInteger, powModInteger)
import Control.Exception             (SomeException, throwIO, assert)
import Data.Foldable                 (foldr')
import qualified Data.ByteString.Lazy as L
import GHC.Exts

data RSAPublicKey = RSAPublicKey {
  rsaKeySize    :: !Word32
  , rsaModulus  :: !Integer
  , rsaExponent :: !Integer
  } deriving (Show, Eq)

data RSAPrivateKey = RSAPrivateKey {
  rsaExpInv     :: !Integer
  , rsaPrimeP   :: !Integer
  , rsaPrimeQ   :: !Integer
  , rsaDp       :: !Integer
  , rsaDq       :: !Integer
  , rsaQInv     :: !Integer
  , rsaPubComp  :: !RSAPublicKey
  } deriving (Show, Eq)


rsaEncryptNoPadding :: RSAPublicKey                       -- Public Key
                    -> L.ByteString                       -- Input Byte String
                    -> Either CryptoExceptions L.ByteString   -- encrypted blob
rsaEncryptNoPadding pk inp =
  let tbeData = os2i inp
      e       = rsaExponent pk
      n       = rsaModulus pk
  in if fromIntegral (numBits tbeData) >  rsaKeySize  pk
     then Left InvalidSize
     else Right . i2os $ powModInteger  tbeData e n


newRSAPrivateKey ::  SystemRandom     -- Source of randomness
                 ->  Word32           -- Modulus bit size. Must be >= 1024
                 ->  Integer          -- PublicExponent
                 ->  IO RSAPrivateKey -- Output
newRSAPrivateKey rnd count expo
  | count < 1024 || expo < 3           = throwIO InvalidSize
  | numBits expo >= fromIntegral count = throwIO KeyGenParametersInvalid
  | otherwise                         = newRSAPrivateKey' 20
  where
    newRSAPrivateKey' :: Int -- Repeat count
                      -> IO RSAPrivateKey
    newRSAPrivateKey' n = do
      let p_bit_count = count `div` 2
          q_bit_count = count - p_bit_count
      (p1,dp) <- genRSAPrimeWithCRT rnd expo p_bit_count n
      (q1,dq) <- genRSAPrimeWithCRT rnd expo q_bit_count n
      let (q,p) = minMax p1 q1
      return  RSAPrivateKey {
        rsaExpInv      = recipModInteger expo ((p - 1)*(q - 1))
        , rsaPrimeP   = p
        , rsaPrimeQ   = q
        , rsaPubComp  = RSAPublicKey count (p*q) expo
        , rsaDp       = dp `mod` (p - 1)
        , rsaDq       = dq `mod` (q - 1)
        , rsaQInv     = q `qInv` p
        }

genRSAPrimeWithCRT :: SystemRandom  -- Randomness source
                   -> Integer       -- Public Exponent
                   -> Word32        -- Bit Count
                   -> Int           -- Repeat count
                   -> IO (Integer, Integer) -- Prime P and dP
genRSAPrimeWithCRT rnd expo bits retry
  | retry < 0 = throwIO TooManyRetries
  | otherwise = do
      p <- genRandomPrime rnd (fromIntegral bits) 20
      let (g,s,_) = extGcd expo (p-1)
      if g == 1
        then return (p, s)
        else genRSAPrimeWithCRT rnd expo bits (retry - 1)

qInv :: Integer       -- q (< p)
     -> Integer       -- p
     -> Integer       -- 1/q mod p
qInv q p = let (_, _, t) = extGcd p q
           in if t > 0 then t else p + t

minMax :: Integer
       -> Integer
       -> (Integer, Integer)
minMax x y = if x < y
             then (x,y)
             else (y,x)

numBits :: Integer
        -> Int
numBits val = I# (word2Int# (sizeInBaseInteger val 2# ))


numBytes :: Integer
        -> Int
numBytes val = I# (word2Int# (sizeInBaseInteger val 256# ))


extGcd :: Integer     -- a
       -> Integer     -- b
       -> (Integer, Integer, Integer ) -- (g, s, t) where
                                      -- s * a + t * b = g
extGcd a b =
  let (n,x)      = minMax a b
      isFlipped  = a /= x
      (# g, s #) = gcdExtInteger x n
      (t,r)      = (g - s * x) `divMod` n
  in if isFlipped
     then assert (r == 0 && g == a * t + b * s ) (g, t, s)
     else assert (r == 0 && g == a * s + b * t ) (g, s, t)


os2i :: L.ByteString
      -> Integer
os2i ba = foldr' (\x y -> (y `unsafeShiftL` 8) .|. (fromIntegral x)) 0
                  (L.unpack ba)

i2os :: Integer
      -> L.ByteString
i2os i = L.reverse .  L.pack  $ i2os' i [] (numBytes i)
  where
    i2os' :: Integer -> [Word8] -> Int -> [Word8]
    i2os' v x c
      | c == 0 = x
      | c > 0 = i2os' ( v `unsafeShiftR` 8)
                      ( fromIntegral (v .&. 0xFF ) : x)
                      (c-1)
      | otherwise = error "Invalid count"
