module Intel.SGX.Types where

import qualified Data.ByteString as B
import Data.Word (Word64, Word32, Word16)

data SECS =
  SECS{
    secsSize                    :: Word64        -- Size of enclave in bytes; must be power of 2
    , secsBaseAddr              :: Word64        -- Enclave Base Linear Address must be naturally aligned to size
    , secsSSAFrameSize          :: Word32        -- Size of one SSA frame in pages (including XSAVE, pad, GPR, and conditionally MISC).
    , secsMiscSelect            :: MiscSelect    -- See MiscSecelct data type
    , secsReserved_byte24_47    :: B.ByteString  -- Reserved bytes. Set to zero
    , secsAttr                  :: Attributes    -- See Attributes data type
    , secsMrEnclave             :: B.ByteString  -- 256-bit SHA256 hash
    , secsReserved_byte96_127   :: B.ByteString  -- Reserved bytes. Set to zero
    , secsMrSigner              :: B.ByteString  -- 256-bit SHA256 hash of signer public key *after* enclave sig was verified
    , secsReserved_byte160_255  :: B.ByteString  -- Reserved bytes. Set to zero
    , secsISVProdId             :: Word16        -- Product ID of enclave
    , secsISVSVN                :: Word16        -- Security version number (SVN) of the enclave
    , secsReserved_byte260_4095 :: B.ByteString  -- 3836 bytes of wasted space
    }

data MiscSelect = MiscSelect{
  miscExInfo             :: Bool   -- Report information about page fault and general protection exception that occurred inside an enclave
  , miscReserved_bit1_32 :: Word32 -- Wasted space
  }

data Attributes = Attributes {
  attrInit                :: Bool -- if the enclave has been initialized by EINIT
  , attrDebug             :: Bool -- If 1, the enclave permit debugger to read and write enclave data
  , attrMode64Bit         :: Bool -- Enclave runs in 64-bit mode
  , attrReserved_bit3     :: Bool -- Must be zero
  , attrProvisionKey      :: Bool -- Provisioning Key is available from EGETKEY
  , attrEinitTokenKey     :: Bool -- EINIT token key is available from EGETKEY
  , attrReserved_bit6_63  :: B.ByteString -- Reserved. Set to zero.
  , attrXFRM              :: XFRM -- See XFRM data type
  }

data XFRM = XFRM

data TCS = TCS {
  tcsReserved_byte0_7  :: Word64
  , tcsFlags           :: TCSFlags
  , tcsOSSA            :: Word64
  , tcsCSSA            :: Word32
  , tcsNSSA            :: Word32
  }

data TCSFlags = TCSFlags {
  tcsFlagsDebugOptIn          :: Bool
  , tcsFlagsReserved_bit1_63 :: Word64
  }
