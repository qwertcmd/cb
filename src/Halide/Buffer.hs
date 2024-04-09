{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Halide.Buffer where

import           Control.Lens
import           Data.Bits
import           Data.Dense          hiding (lerp)
import           Data.Dense.Index
import           Data.Dense.Storable as S
import           Data.Foldable
import           Data.Semigroup
import           Foreign

import           Halide.Type

-- Halide deimension ---------------------------------------------------

-- | The halide type to describe the min, extent and stride of a single
--   dimension for a 'HalideBuffer'.
data HalideDimension = HalideDimension
  { hldMin    :: !Int32
  , hldExtent :: !Int32
  , hldStride :: !Int32
  , hldFlags  :: !Word32
  } deriving (Show, Eq)

instance Storable HalideDimension where
  sizeOf _ = 16
  poke ptr (HalideDimension m e s f) = do
    pokeElemOff (castPtr ptr) 0 m
    pokeElemOff (castPtr ptr) 1 e
    pokeElemOff (castPtr ptr) 2 s
    pokeElemOff (castPtr ptr) 3 f
  peek ptr = do
    m <- peekElemOff (castPtr ptr) 0
    e <- peekElemOff (castPtr ptr) 1
    s <- peekElemOff (castPtr ptr) 2
    f <- peekElemOff (castPtr ptr) 3
    return (HalideDimension m e s f)
  alignment _ = 4

class Stride f where
  layoutStride :: Layout f -> f Int

instance Stride V1 where layoutStride _ = V1 1
instance Stride V2 where layoutStride (V2 x _) = V2 1 x
instance Stride V3 where layoutStride (V3 x y _) = V3 1 x y
instance Stride V4 where layoutStride (V4 x y z _) = V4 1 x y z

makeHalideDimensions
  :: Applicative f
  => f Int32 -- ^ minimum element (usually zero)
  -> f Int32 -- ^ extent
  -> f Int32 -- ^ stride
  -> f HalideDimension
makeHalideDimensions m e s = HalideDimension <$> m <*> e <*> s <*> pure 0

-- | Use a halide dimension of a layout.
withLayout
  :: (Shape f, Stride f, Applicative f)
  => Layout f
  -> (Int32 -> Ptr HalideDimension -> IO a)
  -> IO a
withLayout l f =
  withArrayLen (toList (makeHalideDimensions zero (fromIntegral <$> l) (fromIntegral <$> layoutStride l)))
    $ \n -> f (fromIntegral n)

-- Halide buffer flags -------------------------------------------------

-- | Flags used for a 'HalideBuffer'. Current flags are:
--
--   - 'HalideBufferFlagHostDirty'
--   - 'HalideBufferFlagDeviceDirty'
newtype HalideBufferFlags = HalideBufferFlags Word64
  deriving (Show, Eq, Ord, Storable)

instance Semigroup HalideBufferFlags where
  HalideBufferFlags a <> HalideBufferFlags b = HalideBufferFlags (a.|.b)
instance Monoid HalideBufferFlags where
  mempty  = HalideBufferFlags 0
  mappend = (<>)

pattern HalideBufferFlagHostDirty :: HalideBufferFlags
pattern HalideBufferFlagHostDirty = HalideBufferFlags 1

pattern HalideBufferFlagDeviceDirty :: HalideBufferFlags
pattern HalideBufferFlagDeviceDirty = HalideBufferFlags 2

-- Halide buffer type --------------------------------------------------

-- | The interface used to interpret the device handle for a
--   'HalideBuffer'.
data HalideDeviceInterface

-- | The buffer type used to transfer data to and from a halide program
--   (both AOT and JIT).
data HalideBuffer = HalideBuffer
  { hlbDevice          :: !Word64
  , hlbDeviceInterface :: !(Ptr HalideDeviceInterface)
  , hlbHost            :: !(Ptr Word8)
  , hlbFlags           :: !HalideBufferFlags
  , hlbType            :: !HalideTypeRep
  , hlbDimensions      :: !Int32
  , hlbShape           :: !(Ptr HalideDimension)
  , hlbPadding         :: !(Ptr ())
  }

instance Storable HalideBuffer where
  sizeOf _ = 56
  poke ptr HalideBuffer {..} = do
    pokeByteOff ptr 0  hlbDevice          -- +8 (Word64)
    pokeByteOff ptr 8  hlbDeviceInterface -- +8 (Ptr HDI)
    pokeByteOff ptr 16 hlbHost            -- +8 (Ptr Word8)
    pokeByteOff ptr 24 hlbFlags           -- +8 (buffer flags)
    pokeByteOff ptr 32 hlbType            -- +4 (HalideType)
    pokeByteOff ptr 36 hlbDimensions      -- +4 (Int32)
    pokeByteOff ptr 40 hlbShape           -- +8 (Int64)
    pokeByteOff ptr 48 hlbPadding         -- +8 (Ptr ())
  peek ptr = do
    hlbDevice          <- peekByteOff ptr 0
    hlbDeviceInterface <- peekByteOff ptr 8
    hlbHost            <- peekByteOff ptr 16
    hlbFlags           <- peekByteOff ptr 24
    hlbType            <- peekByteOff ptr 32
    hlbDimensions      <- peekByteOff ptr 36
    hlbShape           <- peekByteOff ptr 40
    hlbPadding         <- peekByteOff ptr 48
    return HalideBuffer {..}
  alignment _ = 4

hostHalideBuffer
  :: HTypeable a
  => Int32
  -> Ptr HalideDimension
  -> Ptr a
  -> HalideBuffer
hostHalideBuffer n dims ptr = HalideBuffer
  { hlbDevice          = 0
  , hlbDeviceInterface = nullPtr
  , hlbHost            = castPtr ptr
  , hlbFlags           = mempty
  , hlbType            = hrep ptr
  , hlbDimensions      = n
  , hlbShape           = dims
  , hlbPadding         = nullPtr
  }

withSArray
  :: (Applicative f, Stride f, Shape f, Storable a, HTypeable a)
  => SArray f a -> (HalideBuffer -> IO b) -> IO b
withSArray a f =
  withLayout (a^.layout) $ \n dimPtr ->
    S.unsafeWithPtr a $ \dataPtr ->
      f (hostHalideBuffer n dimPtr dataPtr)

withSArrayPtr
  :: (Applicative f, Stride f, Shape f, Storable a, HTypeable a)
  => SArray f a -> (Ptr HalideBuffer -> IO b) -> IO b
withSArrayPtr a f = withSArray a $ \hb -> with hb f

createWithBuffer
  :: forall f a b. (Applicative f, Stride f, Shape f, Storable a, HTypeable a)
  => Layout f -> (HalideBuffer -> IO b) -> IO (SArray f a, b)
createWithBuffer l f = do
  fptr <- mallocForeignPtrBytes (sizeOf (undefined :: a) * shapeSize l)
  b <- withLayout l $ \n dimPtr ->
         withForeignPtr fptr $ \dataPtr ->
           f (hostHalideBuffer n dimPtr dataPtr)
  return (S.unsafeFromForeignPtr l fptr, b)

createTupleWithBuffer
  :: forall f a b. (Applicative f, Stride f, Shape f, HTypeable a, HTypeable b, Storable a, Storable b)
  => Layout f -> (Ptr HalideBuffer -> IO ()) -> IO (SArray f a, SArray f b)
createTupleWithBuffer l f = do
  aFptr <- mallocForeignPtrBytes (sizeOf (undefined :: a) * shapeSize l)
  bFptr <- mallocForeignPtrBytes (sizeOf (undefined :: b) * shapeSize l)
  withLayout l $ \dimN dimPtr ->
    withForeignPtr aFptr $ \aPtr ->
      withForeignPtr bFptr $ \bPtr -> do
        let a = hostHalideBuffer dimN dimPtr aPtr
            b = hostHalideBuffer dimN dimPtr bPtr
        withArray [a, b] f
  let a = S.unsafeFromForeignPtr l aFptr
      b = S.unsafeFromForeignPtr l bFptr
  return (a,b)

