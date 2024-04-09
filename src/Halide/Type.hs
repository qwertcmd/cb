{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Halide.Type where

import           Data.Bits
import           Data.Int
import           Data.Word
import           Foreign.Storable

-- | Type to describe a halide type. This has the same representation as
--   as the @Type@ halide type.
newtype HalideTypeRep = HalideTypeRep Word32
  deriving (Show, Eq, Ord, Storable)

-- | The code for the type of Type:
--     - 'HalideTypeInt'
--     - 'HalideTypeUInt'
--     - 'HalideTypeFloat'
--     - 'HalideTypeHandle'
newtype HalideTypeCode = HalideTypeCode Word8
  deriving (Show, Eq, Ord, Storable)

pattern HalideTypeInt :: HalideTypeCode
pattern HalideTypeInt = HalideTypeCode 0
pattern HalideTypeUint :: HalideTypeCode
pattern HalideTypeUint = HalideTypeCode 1
pattern HalideTypeFloat :: HalideTypeCode
pattern HalideTypeFloat = HalideTypeCode 2
pattern HalideTypeHandle :: HalideTypeCode
pattern HalideTypeHandle = HalideTypeCode 3

-- halideTypeCode :: Lens' HalideTypeRep HalideTypeCode
-- halideTypeCode = undefined

-- halideTypePrecision :: Lens' HalideTypeRep Word8
-- halideTypePrecision = undefined

-- halideTypeLanes :: Lens' HalideTypeRep Word16
-- halideTypeLanes = undefined

mkType :: HalideTypeCode -> Word8 -> Word16 -> HalideTypeRep
mkType (HalideTypeCode code) bits lanes = HalideTypeRep $
  f 0 code .|. f 8 bits .|. f 16 lanes
  where
  f n x = fromIntegral x `shiftL` n

mkInt :: Word8 -> HalideTypeRep
mkInt n = mkType HalideTypeInt n 1

mkUInt :: Word8 -> HalideTypeRep
mkUInt n = mkType HalideTypeUint n 1

mkFloat :: Word8 -> HalideTypeRep
mkFloat n = mkType HalideTypeFloat n 1

-- | These are the only valid halide types.
data HType a where
  HDouble :: HType Double
  HFloat  :: HType Float
  HWord8  :: HType Word8
  HWord16 :: HType Word16
  HWord32 :: HType Word32
  HWord64 :: HType Word64
  HInt8   :: HType Int8
  HInt16  :: HType Int16
  HInt32  :: HType Int32
  HInt64  :: HType Int64
  -- HPtr    :: HType (Ptr b)

-- | Class of valid halide types.
class HTypeable a where
  -- | Evidence this is a valid halide type.
  htype :: HType a

instance HTypeable Double where htype = HDouble
instance HTypeable Float where htype = HFloat
instance HTypeable Word8 where htype = HWord8
instance HTypeable Word16 where htype = HWord16
instance HTypeable Word32 where htype = HWord32
instance HTypeable Word64 where htype = HWord64
instance HTypeable Int8 where htype = HInt8
instance HTypeable Int16 where htype = HInt16
instance HTypeable Int32 where htype = HInt32
instance HTypeable Int64 where htype = HInt64
-- instance HTypeable (Ptr a) where htype = HPtr

htypeFor :: HTypeable a => proxy a -> HType a
htypeFor _ = htype
{-# INLINE htypeFor #-}

hrep :: forall proxy a. HTypeable a => proxy a -> HalideTypeRep
hrep p =
  case htypeFor p of
    HFloat  -> mkFloat 32
    HDouble -> mkFloat 64
    HWord8  -> mkUInt 8
    HWord16 -> mkUInt 16
    HWord32 -> mkUInt 32
    HWord64 -> mkUInt 64
    HInt8   -> mkInt 8
    HInt16  -> mkInt 16
    HInt32  -> mkInt 32
    HInt64  -> mkInt 64
    -- HPtr    -> mkType HalideTypeHandle 64 1
{-# INLINE hrep #-}

