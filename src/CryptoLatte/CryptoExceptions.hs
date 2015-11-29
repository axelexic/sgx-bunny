{-# LANGUAGE DeriveDataTypeable #-}

module CryptoLatte.CryptoExceptions
       (
         CryptoExceptions(..)
       )
       where

import Control.Exception
import Data.Typeable

data CryptoExceptions = RandomSourceUnavailable
                      | InvalidSize
                      | PrimeNumberNotFound
                      | TooManyRetries
                      | KeyGenParametersInvalid
                      deriving (Show, Typeable)

instance Exception CryptoExceptions
