{-# LANGUAGE MagicHash #-}

module CryptoLatte.Random (
  SystemRandom
  , BitCount
  , ByteCount
  , newSystemRandom
  , genRandomBytes
  , genRandomInteger
  , genRandomPrime
  )  where

import qualified Data.ByteString.Lazy as L
import System.IO          (Handle, IOMode(..), openBinaryFile)
import Control.Exception  (SomeException, catch, throwIO)
import Data.Foldable      (foldr')
import Data.Bits          (unsafeShiftL, unsafeShiftR, (.|.) )
import Control.Monad
import CryptoLatte.CryptoExceptions
import GHC.Exts
import GHC.Integer.GMP.Internals

type BitCount  = Int
type ByteCount = Int


devRandomSource :: FilePath
devRandomSource = "/dev/urandom"

data SystemRandom = SystemRandom {
  getRandomState :: IO (Either SomeException Handle)
  }


newSystemRandom :: SystemRandom
newSystemRandom = SystemRandom {
  getRandomState =
      catch (liftM Right (openBinaryFile devRandomSource ReadMode))
            (return . Left)
  }


genRandomBytes :: SystemRandom -> ByteCount -> IO L.ByteString
genRandomBytes rnd count =
  if count < 0
  then throwIO InvalidSize
  else do
    v <- getRandomState rnd
    case v of
      Left  y -> throwIO y
      Right h -> L.hGet h count


genRandomInteger :: SystemRandom   -- System Random as Source
                 -> BitCount      -- Number of bits
                 -> IO Integer     -- Resulting Integer
genRandomInteger rnd bitcount =
  let byteCount      = (bitcount + 7) `div` 8
      zero_out_bits  = (byteCount * 8) - bitcount
      zero_out_arr x  = ((head x `unsafeShiftL` zero_out_bits) `unsafeShiftR` zero_out_bits) : tail x
      bs2int    = foldr' (\x -> \y -> (y `unsafeShiftL` 8) .|. (fromIntegral x) ) 0x0
  in if bitcount <= 0
     then throwIO InvalidSize
     else do
       bytes <- genRandomBytes rnd byteCount
       return . bs2int . zero_out_arr . L.unpack $ bytes


genRandomPrime :: SystemRandom     -- SystemRandom as Source
               -> BitCount         -- Number of bits in the prime
               -> Int              -- Retry count >= 0
               -> IO Integer       -- The resulting value
genRandomPrime rnd bits count =
  if count < 0
  then throwIO PrimeNumberNotFound
  else do
    p <- fmap nextPrimeInteger (genRandomInteger rnd bits)
    if I# (word2Int# (sizeInBaseInteger p 2#)) > bits
      then genRandomPrime rnd bits (count - 1)
      else return p
