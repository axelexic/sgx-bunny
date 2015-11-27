module Intel.SGX.Types where

import Text.Printf
import qualified Data.ByteString.Lazy as L
import Data.Word (Word64, Word32, Word16, Word8)

data SECS = SECS {
  secsSize                    :: Word64        -- Size of enclave in bytes; must be power of 2
  , secsBaseAddr              :: Word64        -- Enclave Base Linear Address must be naturally aligned to size
  , secsSSAFrameSize          :: Word32        -- Size of one SSA frame in pages (including XSAVE, pad, GPR, and conditionally MISC).
  , secsMiscSelect            :: MiscSelect    -- See MiscSecelct data type
  , secsReserved_byte24_47    :: L.ByteString  -- Reserved bytes. Set to zero
  , secsAttr                  :: Attributes    -- See Attributes data type
  , secsMrEnclave             :: L.ByteString  -- 256-bit SHA256 hash
  , secsReserved_byte96_127   :: L.ByteString  -- Reserved bytes. Set to zero
  , secsMrSigner              :: L.ByteString  -- 256-bit SHA256 hash of signer public key *after* enclave sig was verified
  , secsReserved_byte160_255  :: L.ByteString  -- Reserved bytes. Set to zero
  , secsISVProdId             :: Word16        -- Product ID of enclave
  , secsISVSVN                :: Word16        -- Security version number (SVN) of the enclave
  , secsReserved_byte260_4095 :: L.ByteString  -- 3836 bytes of wasted space
  }

data MiscSelect = MiscSelect {
  miscExInfo             :: Bool   -- Report information about page fault and general protection exception that occurred inside an enclave
  , miscReserved_bit1_32 :: Word32 -- Wasted space
  } deriving (Eq)

data Attributes = Attributes {
  attrInit                :: Bool -- if the enclave has been initialized by EINIT
  , attrDebug             :: Bool -- If 1, the enclave permit debugger to read and write enclave data
  , attrMode64Bit         :: Bool -- Enclave runs in 64-bit mode
  , attrReserved_bit3     :: Bool -- Must be zero
  , attrProvisionKey      :: Bool -- Provisioning Key is available from EGETKEY
  , attrEinitTokenKey     :: Bool -- EINIT token key is available from EGETKEY
  , attrReserved_bit6_63  :: L.ByteString -- Reserved. Set to zero.
  , attrXFRM              :: XFRM -- See XFRM data type
  }deriving(Eq, Show)

data XFRM = XFRM {
  xfrmEnabled  :: Bool
  , xfrmXCR0  :: Word64    -- Valid value of XCR0
  , xfrmHasXSave :: Bool -- Does the CPU has XSAVE instruction
  } deriving(Eq, Show)

data PageInfo = PageInfo {
  pgEnclaveLinAddr  :: Word64        -- Enclave linear address.
  , pgSourceAddr    :: Word64        -- Effective address of the page where contents are located.
  , pgSecInfo       :: Word64        -- Effective address of the SECINFO or PCMD
  , pgSecs          :: Word64        -- Effective address of EPC slot that currently contains the SECS
  }

data SecInfo = SecInfo {
  siFlags       :: SecInfoFlags
  }

data SecInfoFlags = SecInfoFlags {
  sifIsRead               :: Bool
  , sifIsWrite            :: Bool
  , sifIsExecute          :: Bool
  , sifIsPending          :: Bool
  , sifIsModified         :: Bool
  , sifHasPermRestriction :: Bool
  , sifReserved_bit6_7    :: Word8
  , sifPageType           :: PageType
  , sifReserved_bit16_64  :: Word64
  }

data PageType = PT_SECS
              | PT_TCS
              | PT_REG
              | PT_VA
              | PT_TRIM
              deriving (Show, Eq)

instance Enum PageType where
  toEnum 0 = PT_SECS
  toEnum 1 = PT_TCS
  toEnum 2 = PT_REG
  toEnum 3 = PT_VA
  toEnum 4 = PT_TRIM
  toEnum _ = undefined

  fromEnum PT_SECS = 0
  fromEnum PT_TCS  = 1
  fromEnum PT_REG  = 2
  fromEnum PT_VA   = 3
  fromEnum PT_TRIM = 4

data PCMD = PCMD {
  pcmdSecInfo               :: SecInfo
  , pcmdEnclaveId           :: Word64
  , pcmdReserved_byte72_111 :: L.ByteString
  , pcmdMac                 :: L.ByteString   -- 16-Bytes
  }

data SigStruct = SigStruct{
  ssHeader1                :: SigStructHeader      -- 16 bytes. Signed
  , ssVendor               :: SigStructVendor      -- 4  bytes. Signed
  , ssBuildDate            :: SigStructDate        -- 4  bytes. Signed
  , ssHeader2              :: SigStructHeader      -- 16 bytes. Signed
  , ssSwDefined            :: Word32               -- 4  bytes. Signed
  , ssReserved_byte44_127  :: L.ByteString         -- 84 bytes of zero. Signed
  , ssModulus              :: Integer              -- 384 bytes. Not signed
  , ssExponent             :: Word32               -- 4  bytes. Not signed
  , ssSignature            :: Integer              -- 384 bytes. Not signed
  , ssMiscSelect           :: MiscSelect           -- 4  bytes. Signed
  , ssMiscMask             :: MiscSelect           -- 4  bytes. Signed
  , ssReserved_byte908_927 :: L.ByteString         -- 20 bytes of zero. Signed
  , ssAttributes           :: Attributes           -- 16 bytes. Signed
  , ssAttributesMask       :: Attributes           -- 16 bytes. Signed
  , ssEnclaveHash          :: L.ByteString         -- 32 bytes of SHA256 of enclave. Signed
  , ssReserved_byte992_1023 :: L.ByteString        -- 32 bytes of zero. Signed
  , ssIsvProdId           :: Word16                -- 2  bytes. Signed
  , ssIsvSvn              :: Word16                -- 2  bytes. Signed
  , ssReserved_byte1028_1039 :: L.ByteString       -- 12 bytes of zero. Not signed
  , ssQ1                     :: Integer            -- 384 bytes of Q1
  , ssQ2                     :: Integer            -- 284 bytes of Q2
  }


data EInitToken = EInitToken {
  eitDebug                   :: Bool               -- 4 Bytes. MACed
  , eitReserved_byte4_47     :: L.ByteString       -- 44 Bytes of Zero. MACed
  , eitAttributes            :: Attributes         -- 16 Bytes. MACed
  , eitMrEnclave             :: L.ByteString       -- 32 Bytes. MACed
  , eitReserved_byte96_127   :: L.ByteString       -- 32 Bytes of Zero. MACed
  , eitMtSigner              :: L.ByteString       -- 32 Bytes. MACed
  , eitReserved_byte160_191  :: L.ByteString       -- 32 Bytes. MACed.
  , eitCpuSvnLe              :: CPUSVN             -- 16 Bytes. Not MACed
  , eitIsvProdIdLe           :: Word16             -- 2  Bytes. Not MACed
  , eitIsvSvnLe              :: Word16             -- 2  Bytes. Not MACed
  , eitReserved_byte212_235  :: L.ByteString       -- Reserved. Not MACed
  , eitMaskedMiscSelectLe    :: Word32             -- Returned by the Launch Enclave
  , eitMaskedAttributes      :: Attributes         -- Returned by the Launch Enclave
  , eitKeyId                 :: L.ByteString       -- 32-bytes of KeyID protection. Not MACed
  , eitMAC                   :: L.ByteString       -- 16 bytes of final MAC.
  }


data SigStructHeader = SSHeader{
  ssHeaderValue :: Integer
  }

ssHeaderVal1 :: SigStructHeader
ssHeaderVal1 = SSHeader 0x06000000E10000000000010000000000

ssHeaderVal2 :: SigStructHeader
ssHeaderVal2 = SSHeader 0x01010000600000006000000001000000


data SigStructVendor = SSVendorIntel
                     | SSVendorOther
                     deriving (Show, Eq)

instance Enum SigStructVendor where
  toEnum 0x8086 = SSVendorIntel
  toEnum 0x0    = SSVendorOther
  toEnum _      = undefined

  fromEnum SSVendorIntel = 0x8086
  fromEnum SSVendorOther = 0x0


data SigStructDate = SSDate {
  ssYear      :: Year
  ,  ssMonth  :: Month
  ,  ssDay    :: Day
  } deriving (Show, Eq)


data Year = Year {
  y1 :: Word8
  , y2 :: Word8
  , y3 :: Word8
  , y4 :: Word8
  } deriving (Eq)

instance Show Year where
  show (Year a1 a2 a3 a4) =
    printf "%.1x%.1x%.1x%.1x" a1 a2 a3 a4

data Month = Jan | Feb | Mar | Apr
           | May | Jun | Jul | Aug
           | Sep | Oct | Nov | Dec
           deriving( Show, Eq)

data Day = Day Word8 deriving (Eq)

instance Enum Day where
  succ (Day x) = Day $ if x == 31
                       then 0
                       else succ x
  pred (Day x) = Day $ if x == 0
                       then 31
                       else pred x
  toEnum  = Day . fromIntegral
  fromEnum (Day x) = fromIntegral x

instance Show Day where
  show (Day d) = show d

data TCS = TCS {
  tcsReserved_byte0_7        :: Word64
  , tcsFlags                 :: TCSFlags
  , tcsOSSA                  :: Word64
  , tcsCSSA                  :: Word32
  , tcsNSSA                  :: Word32
  , tcsOentry                :: Word64
  , tcsAep                   :: Word64
  , tcsOFSBasSgx             :: Word64
  , tcsOGSBasSgx             :: Word64
  , tcsFSLimit               :: Word32
  , tcsGSLimit               :: Word32
  , tcsReserved_byte72_4095  :: L.ByteString
  }

data TCSFlags = TCSFlags {
  tcsFlagsDebugOptIn          :: Bool
  , tcsFlagsReserved_bit1_63 :: Word64
  }

data SSAFrame = SSAFrame {
  ssaXsave    :: L.ByteString
  , ssaPad    :: L.ByteString
  , ssaMisc   :: L.ByteString
  , ssaGprSgx :: L.ByteString
  }

data GPRSGX = GPRSGX {
  gprRAX :: Word64
  , gprRCX    :: Word64
  , gprRDX    :: Word64
  , gprRBX    :: Word64
  , gprRSP    :: Word64
  , gprRBP    :: Word64
  , gprRSI    :: Word64
  , gprRDI    :: Word64
  , gprR8     :: Word64
  , gprR9     :: Word64
  , gprR10    :: Word64
  , gprR11    :: Word64
  , gprR12    :: Word64
  , gprR13    :: Word64
  , gprR14    :: Word64
  , gprR15    :: Word64
  , gprRFLAGS :: Word64
  , gprRIP    :: Word64
  , gprURSP   :: Word64
  , gprURBP   :: Word64
  , gprExitInfo :: ExitInfo
  , gprReserved_byte164_167 :: Word32
  , gprFsBase :: Word64
  , gprGsBase :: Word64
  }

data ExitInfo = ExitInfo {
  eiVector              :: ExcptVector
  , eiType              :: ExitInfoType
  , eiReserved_bit11_30 :: Word32
  , eiValid             :: Bool
  }

data ExitInfoType = ExitTypeHwExcept |
                    ExitTypeSwExcept

instance Enum ExitInfoType where
  fromEnum ExitTypeHwExcept = 0x3
  fromEnum ExitTypeSwExcept = 0x6

  toEnum 0x3 = ExitTypeHwExcept
  toEnum 0x6 = ExitTypeSwExcept
  toEnum _   = undefined

data ExcptVector = DividerExcpt
                 | DebugExcpt
                 | BreakpointExcpt
                 | BoundRangeExceedExcpt
                 | InvalidOpCodeExcpt
                 | GeneralProtectionExcpt
                 | PageFaultExcpt
                 | FPUErrorExcept
                 | AlignmentCheckExcept
                 | SIMDException
                 deriving (Show, Eq)


instance Enum ExcptVector where
  fromEnum DividerExcpt           = 0
  fromEnum DebugExcpt             = 1
  fromEnum BreakpointExcpt        = 3
  fromEnum BoundRangeExceedExcpt  = 5
  fromEnum InvalidOpCodeExcpt     = 6
  fromEnum GeneralProtectionExcpt = 13
  fromEnum PageFaultExcpt         = 14
  fromEnum FPUErrorExcept         = 16
  fromEnum AlignmentCheckExcept   = 17
  fromEnum SIMDException          = 19

  toEnum 0 = DividerExcpt
  toEnum 1 = DebugExcpt
  toEnum 3 = BreakpointExcpt
  toEnum 5 = BoundRangeExceedExcpt
  toEnum 6 = InvalidOpCodeExcpt
  toEnum 13 = GeneralProtectionExcpt
  toEnum 14 = PageFaultExcpt
  toEnum 16 = FPUErrorExcept
  toEnum 17 = AlignmentCheckExcept
  toEnum 19 = SIMDException
  toEnum _  = undefined


data Report = Report {
  repCpuSvn                 :: CPUSVN         -- 16 Bytes
  , repMiscSelect           :: MiscSelect     -- 4 Bytes
  , repReserved_byte20_47   :: L.ByteString   -- reseved 28 bytes of Zero
  , repAttributes           :: Attributes     -- 16 bytes
  , repMrEnclave            :: L.ByteString   -- 32 bytes
  , repReserved_byte96_127  :: L.ByteString   -- 32 bytes of zero
  , repMrSigner             :: L.ByteString   -- 32 bytes of signger
  , repReserved_byte160_255 :: L.ByteString   -- 32 bytes of zero
  , repIsvProdId            :: Word16         -- 2 bytes
  , repIsvSvn               :: Word16         -- 2 bytes
  , repReserved_byte260_319 :: L.ByteString   -- 60 bytes of zero
  , repReportData           :: L.ByteString   -- 64 bytes of report data
  , repKeyId                :: L.ByteString   -- 32-bytes of key id
  , repMAC                  :: L.ByteString   -- 16-bytes of Mac
  }


data CPUSVN = CPUSVN {
  cpuSvnValue :: L.ByteString
  }


data TargetInfo = TargetInfo {
  tiTargetMrEnclave       :: L.ByteString        -- 32 bytes of MrEnclave
  , tiAttributes          :: Attributes          -- 16 bytes of attributes
  , tiReserved_byte48_51  :: Word32              -- 4 bytes reserved
  , tiMiscSelect          :: MiscSelect          -- 4 bytes of MiscSelect
  , tiReserved_byte56_511 :: L.ByteString        -- Reserved to zero
  }


data KeyRequest = KeyRequest {
  krKeyName               :: KeyName              -- 2 bytes
  , krKeyPolicy           :: KeyPolicy            -- 2 bytes
  , krIsvSvn              :: Word16               -- 2 bytes
  , krReserved_byte6_7    :: Word16               -- 2 bytes
  , krCpuSvn              :: CPUSVN               -- 16 bytes
  , krAttributeMask       :: Attributes           -- 16 bytes
  , krKeyId               :: L.ByteString         -- 32 bytes
  , krMiscMask            :: MiscSelect           -- 4 bytes
  , krReserved_byte76_511 :: L.ByteString         -- 436 bytes of zero
  }


data KeyName = EINIT_TOKEN_KEY
             | PROVISION_KEY
             | PROVISION_SEAL_KEY
             | REPORT_KEY
             | SEAL_KEY
             deriving (Show, Eq)


instance Enum KeyName where
  toEnum 0 = EINIT_TOKEN_KEY
  toEnum 1 = PROVISION_KEY
  toEnum 2 = PROVISION_SEAL_KEY
  toEnum 3 = REPORT_KEY
  toEnum 4 = SEAL_KEY
  toEnum _ = undefined

  fromEnum EINIT_TOKEN_KEY     = 0
  fromEnum PROVISION_KEY       = 1
  fromEnum PROVISION_SEAL_KEY  = 2
  fromEnum REPORT_KEY          = 3
  fromEnum SEAL_KEY            = 4


data KeyPolicy = KeyPolicy {
  kpIsMrEnclave :: Bool
  , kpIsMrSigner :: Bool
  , kpReserved_bit2_15 :: Word16
  }
