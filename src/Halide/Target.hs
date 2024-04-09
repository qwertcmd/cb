{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Halide.Target where

import Control.Lens
import Data.Bits.Lens
import Data.Word
import Data.List
import System.IO.Unsafe

import Foreign
import Foreign.C

-- | The operating system used by the target. Determines which system
--    calls to generate.
data TargetOS
  = OSUnknown
  | Linux
  | Windows
  | MacOS
  | Android
  | IOS
  | QuRT
  | NoOS
  deriving Enum

instance Show TargetOS where
  show = \case
    OSUnknown -> "os_unknown"
    Linux     -> "linux"
    Windows   -> "windows"
    MacOS     -> "osx"
    Android   -> "android"
    IOS       -> "ios"
    QuRT      -> "qurt"
    NoOS      -> "noos"

-- | The architecture used by the target. Determines the instruction set
--   to use.
data TargetArch
  = ArchUnknown
  | X86
  | Arm
  | Mips
  | Hexagon
  | PowerPC
  deriving Enum

instance Show TargetArch where
  show = \case
    ArchUnknown -> "arch_unknown"
    X86         -> "x86"
    Arm         -> "arm"
    Mips        -> "mips"
    PowerPC     -> "powerpc"
    Hexagon     -> "hexagon"

data Target = Target
  { targetOS       :: !TargetOS
    -- ^ the operating system used by the target
  , targetArch     :: !TargetArch
    -- ^ the architecture used by the target
  , targetBits     :: !Int
    -- ^ the number of bits the target uses -- 32 or 64; 0 for unknown
  , targetFeatures :: !Word64
    -- ^ bit mask of target features
    --
    -- (may turn into Integer in the future if more featuers are added)
  }

instance Show Target where
  show t@Target {..} =
    merge $ [show targetArch, show targetBits, show targetOS] ++ sort features
    where
      features = map show $ setFeatures t
      merge    = intercalate "-"

-- features ------------------------------------------------------------

feats :: Lens' Target Word64
feats f t = f (targetFeatures t) <&> \as -> t { targetFeatures = as }

-- | Lens onto whether the target feature is enabled.
targetFeature :: TargetFeature -> Lens' Target Bool
targetFeature (TargetFeature a) = feats . bitAt (fromIntegral a)

-- | Set a 'TargetFeature' for the 'Target.
setFeature :: TargetFeature -> Target -> Target
setFeature tf = targetFeature tf .~ True

addFeatures :: [TargetFeature] -> Target -> Target
addFeatures fs t = foldr setFeature t fs

-- | List of features that have been set
setFeatures :: Target -> [TargetFeature]
setFeatures =
  toListOf (feats . bits . filtered id . asIndex . to (TargetFeature . fromIntegral))

-- | Unset a 'TargetFeature' for the 'Target.
unsetFeature :: TargetFeature -> Target -> Target
unsetFeature tf = targetFeature tf .~ False

-- | The type used to describe target features.
newtype TargetFeature = TargetFeature Word32
  deriving (Eq, Ord)

instance Show TargetFeature where
  show (TargetFeature i) = case i of
    0  -> "jit"
    1  -> "debug"
    2  -> "no_asserts"
    3  -> "no_bounds_query"
    4  -> "sse41"
    5  -> "avx"
    6  -> "avx2"
    7  -> "fma"
    8  -> "fma4"
    9  -> "f16c"
    10 -> "armv7s"
    11 -> "no_neon"
    12 -> "vsx"
    13 -> "power_arch_2_07"
    14 -> "cuda"
    15 -> "cuda_capability_30"
    16 -> "cuda_capability_32"
    17 -> "cuda_capability_35"
    18 -> "cuda_capability_50"
    19 -> "opencl"
    20 -> "cl_doubles"
    21 -> "opengl"
    22 -> "openglcompute"
    -- 23 -> "unused"
    24 -> "user_context"
    25 -> "matlab"
    26 -> "profile"
    27 -> "no_runtime"
    28 -> "metal"
    29 -> "mingw"
    30 -> "c_plus_plus_name_mangling"
    31 -> "large_buffers"
    32 -> "hvx_64"
    33 -> "hvx_128"
    34 -> "hvx_v62"
    35 -> "hvx_shared_object"
    36 -> "fuzz_float_stores"
    37 -> "soft_float_abi"
    38 -> "msan"
    39 -> "avx512"
    40 -> "avx512_knl"
    41 -> "avx512_skylake"
    42 -> "avx512_cannonlake"
    43 -> "trace_loads"
    44 -> "trace_stores"
    45 -> "trace_realizations"
    _  -> "unknown_feature"


-- | Generate code that will run immediately inside the calling process.
targetJit :: TargetFeature
targetJit = TargetFeature 0

-- | Turn on debug info and output for runtime code.
targetDebug :: TargetFeature
targetDebug = TargetFeature 1

-- | Disable all runtime checks for slightly tighter code.
targetNoAsserts :: TargetFeature
targetNoAsserts = TargetFeature 2

-- | Disable the bounds querying functionality.
targetNoBoundsQuery :: TargetFeature
targetNoBoundsQuery = TargetFeature 3


-- | Use SSE 4.1 and earlier instructions. Only relevant on x86.
targetSse41 :: TargetFeature
targetSse41 = TargetFeature 4

-- | Use AVX 1 instructions. Only relevant on x86.
targetAvx :: TargetFeature
targetAvx = TargetFeature 5

-- | Use AVX 2 instructions. Only relevant on x86.
targetAvx2 :: TargetFeature
targetAvx2 = TargetFeature 6

-- | Enable x86 FMA instruction
targetFma :: TargetFeature
targetFma = TargetFeature 7

-- | Enable x86 (AMD) FMA4 instruction set
targetFma4 :: TargetFeature
targetFma4 = TargetFeature 8

-- | Enable x86 16-bit float support
targetF16c :: TargetFeature
targetF16c = TargetFeature 9


-- | Generate code for ARMv7s. Only relevant for 32-bit ARM.
targetArmv7s :: TargetFeature
targetArmv7s = TargetFeature 10

-- | Avoid using NEON instructions. Only relevant for 32-bit ARM.
targetNoNeon :: TargetFeature
targetNoNeon = TargetFeature 11


-- | Use VSX instructions. Only relevant on POWERPC.
targetVsx :: TargetFeature
targetVsx = TargetFeature 12

-- | Use POWER ISA 2.07 new instructions. Only relevant on POWERPC.
targetPowerArch_2_07 :: TargetFeature
targetPowerArch_2_07 = TargetFeature 13


-- | Enable the CUDA runtime. Defaults to compute capability 2.0 (Fermi)
targetCuda :: TargetFeature
targetCuda = TargetFeature 14

-- | Enable CUDA compute capability 3.0 (Kepler)
targetCudaCapability30 :: TargetFeature
targetCudaCapability30 = TargetFeature 15

-- | Enable CUDA compute capability 3.2 (Tegra K1)
targetCudaCapability32 :: TargetFeature
targetCudaCapability32 = TargetFeature 16

-- | Enable CUDA compute capability 3.5 (Kepler)
targetCudaCapability35 :: TargetFeature
targetCudaCapability35 = TargetFeature 17

-- | Enable CUDA compute capability 5.0 (Maxwell)
targetCudaCapability50 :: TargetFeature
targetCudaCapability50 = TargetFeature 18


-- | Enable the OpenCL runtime.
targetOpencl :: TargetFeature
targetOpencl = TargetFeature 19

-- | Enable double support on OpenCL targets
targetClDoubles :: TargetFeature
targetClDoubles = TargetFeature 20


-- | Enable the OpenGL runtime.
targetOpengl :: TargetFeature
targetOpengl = TargetFeature 21

-- | Enable OpenGL Compute runtime.
targetOpenglcompute :: TargetFeature
targetOpenglcompute = TargetFeature 22


-- | Unused. (Formerly: Enable the RenderScript runtime.)
-- targetUnused_23 :: TargetFeature
-- targetUnused_23 = TargetFeature 23


-- | Generated code takes a userContext pointer as first argument
targetUserContext :: TargetFeature
targetUserContext = TargetFeature 24


-- | Generate a mexFunction compatible with Matlab mex libraries. See
--   tools/mexHalide.m.
targetMatlab :: TargetFeature
targetMatlab = TargetFeature 25


-- | Launch a sampling profiler alongside the Halide pipeline that
--   monitors and reports the runtime used by each Func
targetProfile :: TargetFeature
targetProfile = TargetFeature 26

-- | Do not include a copy of the Halide runtime in any generated object
--   file or assembly
targetNoRuntime :: TargetFeature
targetNoRuntime = TargetFeature 27


-- | Enable the (Apple) Metal runtime.
targetMetal :: TargetFeature
targetMetal = TargetFeature 28

-- | For Windows compile to MinGW toolset rather then Visual Studio
targetMingw :: TargetFeature
targetMingw = TargetFeature 29


-- | Generate C++ mangled names for result function et al
targetCPlusPlusMangling :: TargetFeature
targetCPlusPlusMangling = TargetFeature 30


-- | Enable 64-bit buffer indexing to support buffers > 2GB.
targetLargeBuffers :: TargetFeature
targetLargeBuffers = TargetFeature 31


-- | Enable HVX 64 byte mode.
targetHvx64 :: TargetFeature
targetHvx64 = TargetFeature 32

-- | Enable HVX 128 byte mode.
targetHvx128 :: TargetFeature
targetHvx128 = TargetFeature 33

-- | Enable Hexagon v62 architecture.
targetHvxV62 :: TargetFeature
targetHvxV62 = TargetFeature 34

-- | On every floating point store set the last bit of the mantissa to
--   zero. Pipelines for which the output is very different with this
--   feature enabled may also produce very different output on different
--   processors.
targetFuzzFloatStores :: TargetFeature
targetFuzzFloatStores = TargetFeature 35

-- | Enable soft float ABI. This only enables the soft float ABI calling
--   convention which does not necessarily use soft floats.
targetSoftFloatAbi :: TargetFeature
targetSoftFloatAbi = TargetFeature 36

-- | Enable hooks for MSAN support.
targetMsan :: TargetFeature
targetMsan = TargetFeature 37

-- | Enable the base AVX512 subset supported by all AVX512
--   architectures. The specific feature sets are AVX-512F and
--   AVX512-CD. See https://en.wikipedia.org/wiki/AVX-512 for a
--   description of each AVX subset.
targetAvx512 :: TargetFeature
targetAvx512 = TargetFeature 38

-- | Enable the AVX512 features supported by Knight's Landing chips such
--   as the Xeon Phi x200. This includes the base AVX512 set and also
--   AVX512-CD and AVX512-ER.
targetAvx512Knl :: TargetFeature
targetAvx512Knl = TargetFeature 39

-- | Enable the AVX512 features supported by Skylake Xeon server
--   processors. This adds AVX512-VL AVX512-BW and AVX512-DQ to the base
--   set. The main difference from the base AVX512 set is better support
--   for small integer ops. Note that this does not include the Knight's
--   Landing features. Note also that these features are not available
--   on Skylake desktop and mobile processors.
targetAvx512Skylake :: TargetFeature
targetAvx512Skylake = TargetFeature 40

-- | Enable the AVX512 features expected to be supported by future
--   Cannonlake processors. This includes all of the Skylake features
--   plus AVX512-IFMA and AVX512-VBMI.
targetAvx512Cannonlake :: TargetFeature
targetAvx512Cannonlake = TargetFeature 41

-- | Deprecated
targetHvxUseSharedObject :: TargetFeature
targetHvxUseSharedObject = TargetFeature 42

-- | Trace all loads done by the pipeline. Equivalent to calling
--   Func::traceLoads on every non-inlined Func.
targetTraceLoads :: TargetFeature
targetTraceLoads = TargetFeature 43

-- | Trace all stores done by the pipeline. Equivalent to calling
--   Func::traceStores on every non-inlined Func.
targetTraceStores :: TargetFeature
targetTraceStores = TargetFeature 44

-- | Trace all realizations done by the pipeline. Equivalent to calling
--   Func::traceRealizations on every non-inlined Func.
targetTraceRealizations :: TargetFeature
targetTraceRealizations = TargetFeature 45

numFeatures :: Int
numFeatures = 46

-- FFI -----------------------------------------------------------------

foreign import ccall "new_target"
  new_target :: Word32 -> Word32 -> CInt -> Word64 -> IO (Ptr Target)

newTarget :: Target -> IO (Ptr Target)
newTarget Target {..} = do
  let o = fromIntegral $ fromEnum targetOS
      a = fromIntegral $ fromEnum targetArch
      b = fromIntegral targetBits
  new_target o a b targetFeatures

foreign import ccall "target_from_string"
  target_from_string :: CString -> IO (Ptr Target)

foreign import ccall "target_string"
  target_string :: Ptr Target -> CString -> IO ()

targetString :: Ptr Target -> IO String
targetString target = do
  allocaBytes 1024 $ \cstr -> do
    target_string target cstr
    peekCString cstr

foreign import ccall "from_target"
  from_target :: Ptr Target -> Ptr Word32 -> Ptr Word32
              -> Ptr CInt -> Ptr Word64 -> IO ()

foreign import ccall "host_target"
  host_target :: IO (Ptr Target)

hostTarget :: Target
hostTarget = unsafePerformIO $ do
  tPtr <- host_target
  t    <- fromTarget tPtr
  delete_target tPtr
  return t
{-# NOINLINE hostTarget #-}

fromTarget :: Ptr Target -> IO Target
fromTarget tPtr =
  alloca $ \oPtr ->
  alloca $ \aPtr ->
  alloca $ \bPtr ->
  alloca $ \fPtr -> do
    from_target tPtr oPtr aPtr bPtr fPtr
    o <- fromIntegral <$> peek oPtr
    a <- fromIntegral <$> peek aPtr
    b <- fromIntegral <$> peek bPtr
    f <- peek fPtr
    return $ Target (toEnum o) (toEnum a) b f

foreign import ccall "delete_target"
  delete_target :: Ptr Target -> IO ()

withTarget :: Target -> (Ptr Target -> IO a) -> IO a
withTarget t f = do
  tPtr <- newTarget t
  a <- f tPtr
  delete_target tPtr
  return a

