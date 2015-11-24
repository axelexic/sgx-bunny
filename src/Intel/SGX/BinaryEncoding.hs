{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Intel.SGX.BinaryEncoding where

import Intel.SGX.Types
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.Binary.Builder as BD
import qualified Data.ByteString.Lazy as L

setBitIfTrue :: Bool
             -> Int
             -> Word8
setBitIfTrue t i = if t
                   then 0x1 `shiftL` i
                   else 0x0

instance Binary MiscSelect where
  put x  = if miscExInfo x
           then putWord32le 0x80
           else putWord32le 0x0

  get  = do
    msBit <- getWord8
    skip 3
    return $ MiscSelect (msBit .&. 0x80 == 0x80) 0x00


putPadBytes :: Int   -- Pad count
            -> Word8 -- Pad value per byte
            -> Put
putPadBytes c = putBuilder . BD.fromLazyByteString . L.take c' . L.repeat
                where c' = fromIntegral c


instance Binary Attributes where
  get = do
    w        <- getWord8
    skip 7
    xf       <- get
    return Attributes {
      attrInit                = testBit w 7
      , attrDebug             = testBit w 6
      , attrMode64Bit         = testBit w 5
      , attrReserved_bit3     = False
      , attrProvisionKey      = testBit w 3
      , attrEinitTokenKey     = testBit w 2
      , attrReserved_bit6_63  = L.take 7 $ L.repeat 0x0
      , attrXFRM              = xf
      }


  put attr = do
    put topByte
    putPadBytes 7 0x0
    put (attrXFRM attr)
      where
        topByte :: Word8
        topByte = let
          isInit   = setBitIfTrue (attrInit  attr)     7
          isDebug  = setBitIfTrue (attrDebug attr)     6
          isMode64 = setBitIfTrue (attrMode64Bit attr) 5
          isReserved_bit3 = 0                       -- 4
          isProvK  = setBitIfTrue (attrProvisionKey attr) 3
          isEinitK = setBitIfTrue (attrEinitTokenKey attr) 2
          in isInit .|. isDebug .|. isMode64 .|. isReserved_bit3 .|.
             isProvK .|. isEinitK


instance Binary XFRM where
  get = do
    w <- getWord64be
    case w `shiftR` 62 of
      0x3  -> return $ XFRM True  w  (w `shiftL` 2 > 0)
      _    -> return $ XFRM False 0x0 False

  put x = case xfrmEnabled x of
    False -> putPadBytes 8 0x0
    True  -> case xfrmHasXSave x of
      True  -> (putWord64be . xfrmXCR0) x
      False -> do
        put (0xc0 :: Word8)
        putPadBytes 7 0x0

#ifdef __TEST__
-- Test MiscSecet
testMiscSelect :: Bool
testMiscSelect =
  let true_misc  = MiscSelect True 0
      false_misc = MiscSelect False 11
      true_bin   = L.pack [0x80,0x0,0x0,0x0]
      false_bin  = L.pack [0x0,0x0,0x0,0x0]
      enc_true   = encode true_misc == true_bin
      enc_false  = encode false_misc == false_bin
      dec_true   = decode true_bin  == MiscSelect True  0
      dec_false  = decode false_bin == MiscSelect False 0
  in
    enc_true && enc_false && dec_true && dec_false

-- Test XFRM
testXFRM :: Bool
testXFRM =
  let xfrm_not_set      = XFRM False 0x0 False
      xfrm_set_no_xsave = XFRM True  0xC000000000000000 False
      xfrm_set_xsave    = XFRM True  0xC000000000000701 True
      xfrm_bin_not_set  = L.take 8 $ L.repeat 0x0
      xfrm_bin_no_xsave = L.pack [0xC0, 0, 0, 0, 0, 0, 0, 0]
      xfrm_bin_xsave    = L.pack [0xC0, 0, 0, 0, 0, 0, 0x07, 0x01]
      put_truth         = encode xfrm_not_set      == xfrm_bin_not_set   &&
                          encode xfrm_set_no_xsave == xfrm_bin_no_xsave  &&
                          encode xfrm_set_xsave    == xfrm_bin_xsave
      get_truth         = decode xfrm_bin_not_set  == xfrm_not_set       &&
                          decode xfrm_bin_no_xsave == xfrm_set_no_xsave  &&
                          decode xfrm_bin_xsave    == xfrm_set_xsave
  in put_truth && get_truth

#endif
