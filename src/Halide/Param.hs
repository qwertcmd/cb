{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Halide.Param where

import           Control.Monad
import           Data.Proxy
import           Foreign
import           Foreign.C

import           Data.Dense
import           Halide.Buffer
import           Halide.Type
import           Language.C.Inline.Cpp as C
import           Prelude               hiding (exp)

import GHC.ForeignPtr
import Data.Coerce

context cppCtx

include "<Halide.h>"
using "namespace Halide"

-- ImageParam ----------------------------------------------------------

-- | An input image for a halide function.
data ImageParam

foreign import ccall "new_image_param"
  new_image_param :: HalideTypeRep -> CInt -> CString -> IO (Ptr ImageParam)

foreign import ccall "&delete_image_param"
  delete_image_param :: FinalizerPtr ImageParam

-- | A parameter for a function.
newtype ArrayParam (f :: * -> *) a = ArrayParam (ForeignPtr ImageParam)

-- | Create a new parameter with the given name.
newArrayParam
  :: forall f a. (Additive f, Foldable f, HTypeable a)
  => String -> IO (ArrayParam f a)
newArrayParam nm = do
  let ty = hrep (Proxy @a)
      d  = fromIntegral $ length (zero :: f Int)
  imgPtr <- withCString nm $ new_image_param ty d
  ArrayParam <$> newForeignPtr delete_image_param imgPtr

set_image_param :: Ptr ImageParam -> Ptr HalideBuffer -> IO ()
set_image_param (castPtr -> imgParam) (castPtr -> hb) = [block| void {
  Buffer<> img(*((halide_buffer_t*)$(void* hb)));
  ((ImageParam*)$(void* imgParam))->set(img);
}|]

withArrayParam :: ArrayParam f a -> (Ptr ImageParam -> IO b) -> IO b
withArrayParam (ArrayParam fptr) = withForeignPtr fptr

-- | Set the input array of an 'ArrayParam'. This is useful when
--   jitting.
setArrayParam
  :: (Applicative f, Stride f, Shape f, Storable a, HTypeable a)
  => ArrayParam f a -> SArray f a -> IO ()
setArrayParam (ArrayParam param) a =
  withSArray a $ \hb ->
  with hb $ \hbPtr ->
  withForeignPtr param $ \paramPtr ->
    set_image_param paramPtr hbPtr
{-# INLINE setArrayParam #-}

set_bounds :: Ptr ImageParam -> Int -> (Int,Int) -> IO ()
set_bounds
  (castPtr -> imgParam)
  (fromIntegral -> dim)
  (fromIntegral -> lb,fromIntegral -> ub)
  = [block| void {
  ImageParam input = *((ImageParam*)$(void* imgParam));
  input.dim($(int dim)).set_bounds($(int lb), $(int ub));
  }|]

setBounds :: ForeignPtr ImageParam -> Int -> (Int,Int) -> IO ()
setBounds fptr dim (a,b) = withForeignPtr fptr $ \ptr ->
  set_bounds ptr dim (a,b)

-- Scalar parameters ---------------------------------------------------

data Param

-- | A scalar parameter that can be set
newtype Scalar a = Scalar (ForeignPtr Param)

-- new_param :: HalideType -> CString -> IO (Ptr Param)
-- new_param (HalideType ty) nmPtr = castPtr <$>
--   [block| void* {
--     Type ty = static_cast<Type>($(uint32_t ty));
--     if (ty == UInt(8)) { return new Param<uint_8>(nm); }
--     else if (ty == (UInt(16))  { return new Param<uint16_t>(nm); }
--     else if (ty == (UInt(32))  { return new Param<uint32_t>(nm); }
--     else if (ty == (UInt(64))  { return new Param<uint64_t>(nm); }
--     else if (ty == (Int(8))    { return new Param<int8_t>(nm); }
--     else if (ty == (Int(16))   { return new Param<int16_t>(nm); }
--     else if (ty == (Int(32))   { return new Param<int32_t>(nm); }
--     else if (ty == (Int(64))   { return new Param<int64_t>(nm); }
--     else if (ty == (Float(64)) { return new Param<float>(nm); }
--     else if (ty == (Float(64)) { return new Param<double>(nm); }
--     else return NULL;
--   }|]

-- Damn template arguments. If there's a better way to do this I'd like
-- to know.

new_param_int8 :: CString -> IO (Ptr Param)
new_param_int8 nm = castPtr <$> [exp| void* {new Param<int8_t>($(char* nm))} |]
new_param_int16 :: CString -> IO (Ptr Param)
new_param_int16 nm = castPtr <$> [exp| void* {new Param<int16_t>($(char* nm))} |]
new_param_int32 :: CString -> IO (Ptr Param)
new_param_int32 nm = castPtr <$> [exp| void* {new Param<int32_t>($(char* nm))} |]
new_param_int64 :: CString -> IO (Ptr Param)
new_param_int64 nm = castPtr <$> [exp| void* {new Param<int64_t>($(char* nm))} |]

new_param_uint8 :: CString -> IO (Ptr Param)
new_param_uint8 nm = castPtr <$> [exp| void* {new Param<uint8_t>($(char* nm))} |]
new_param_uint16 :: CString -> IO (Ptr Param)
new_param_uint16 nm = castPtr <$> [exp| void* {new Param<uint16_t>($(char* nm))} |]
new_param_uint32 :: CString -> IO (Ptr Param)
new_param_uint32 nm = castPtr <$> [exp| void* {new Param<uint32_t>($(char* nm))} |]
new_param_uint64 :: CString -> IO (Ptr Param)
new_param_uint64 nm = castPtr <$> [exp| void* {new Param<uint64_t>($(char* nm))} |]

new_param_float :: CString -> IO (Ptr Param)
new_param_float nm = castPtr <$> [exp| void* {new Param<float>($(char* nm))} |]
new_param_double :: CString -> IO (Ptr Param)
new_param_double nm = castPtr <$> [exp| void* {new Param<double>($(char* nm))} |]

foreign import ccall "&delete_param_int8" delete_param_int8 :: FinalizerPtr Param
foreign import ccall "&delete_param_int16" delete_param_int16 :: FinalizerPtr Param
foreign import ccall "&delete_param_int32" delete_param_int32 :: FinalizerPtr Param
foreign import ccall "&delete_param_int64" delete_param_int64 :: FinalizerPtr Param
foreign import ccall "&delete_param_uint8" delete_param_uint8 :: FinalizerPtr Param
foreign import ccall "&delete_param_uint16" delete_param_uint16 :: FinalizerPtr Param
foreign import ccall "&delete_param_uint32" delete_param_uint32 :: FinalizerPtr Param
foreign import ccall "&delete_param_uint64" delete_param_uint64 :: FinalizerPtr Param
foreign import ccall "&delete_param_float" delete_param_float :: FinalizerPtr Param
foreign import ccall "&delete_param_double" delete_param_double :: FinalizerPtr Param

newScalarParam :: forall a. HTypeable a => String -> IO (Scalar a)
newScalarParam nm = withCString nm $ \nmPtr -> do
  let (f,d) = case htype :: HType a of
        HInt8   -> (new_param_int8, delete_param_int8)
        HInt16  -> (new_param_int16, delete_param_int16)
        HInt32  -> (new_param_int32, delete_param_int32)
        HInt64  -> (new_param_int64, delete_param_int64)
        HWord8  -> (new_param_uint8, delete_param_int8)
        HWord16 -> (new_param_uint16, delete_param_int16)
        HWord32 -> (new_param_uint32, delete_param_int32)
        HWord64 -> (new_param_uint64, delete_param_int64)
        HFloat  -> (new_param_float, delete_param_float)
        HDouble -> (new_param_double, delete_param_double)
  paramPtr <- f nmPtr
  Scalar <$> newForeignPtr d paramPtr

set_param_int8 :: Ptr Param -> Int8 -> IO ()
set_param_int8 (castPtr -> paramPtr) a =
  [block| void { ((Param<int8_t>*)$(void* paramPtr))->set($(int8_t a)); }|]
set_param_int16 :: Ptr Param -> Int16 -> IO ()
set_param_int16 (castPtr -> paramPtr) a =
  [block| void { ((Param<int16_t>*)$(void* paramPtr))->set($(int16_t a)); } |]
set_param_int32 :: Ptr Param -> Int32 -> IO ()
set_param_int32 (castPtr -> paramPtr) a =
  [block| void { ((Param<int32_t>*)$(void* paramPtr))->set($(int32_t a)); } |]
set_param_int64 :: Ptr Param -> Int64 -> IO ()
set_param_int64 (castPtr -> paramPtr) a =
  [block| void { ((Param<int64_t>*)$(void* paramPtr))->set($(int64_t a)); } |]

set_param_uint8 :: Ptr Param -> Word8 -> IO ()
set_param_uint8 (castPtr -> paramPtr) a =
  [block| void { ((Param<uint8_t>*)$(void* paramPtr))->set($(uint8_t a)); } |]
set_param_uint16 :: Ptr Param -> Word16 -> IO ()
set_param_uint16 (castPtr -> paramPtr) a =
  [block| void { ((Param<uint16_t>*)$(void* paramPtr))->set($(uint16_t a)); } |]
set_param_uint32 :: Ptr Param -> Word32 -> IO ()
set_param_uint32 (castPtr -> paramPtr) a =
  [block| void { ((Param<uint32_t>*)$(void* paramPtr))->set($(uint32_t a)); } |]
set_param_uint64 :: Ptr Param -> Word64 -> IO ()
set_param_uint64 (castPtr -> paramPtr) a =
  [block| void { ((Param<uint64_t>*)$(void* paramPtr))->set($(uint64_t a)); } |]

set_param_float :: Ptr Param -> Float -> IO ()
set_param_float (castPtr -> paramPtr) (CFloat -> a) =
  [block| void { ((Param<float>*)$(void* paramPtr))->set($(float a)); } |]
set_param_double :: Ptr Param -> Double -> IO ()
set_param_double (castPtr -> paramPtr) (CDouble -> a) =
  [block| void { ((Param<double>*)$(void* paramPtr))->set($(double a)); } |]

setScalar :: forall a. HTypeable a => Scalar a -> a -> IO ()
setScalar (Scalar fptr) a = withForeignPtr fptr $ \ptr ->
  case htype :: HType a of
    HInt8   -> set_param_int8 ptr a
    HInt16  -> set_param_int16 ptr a
    HInt32  -> set_param_int32 ptr a
    HInt64  -> set_param_int64 ptr a
    HWord8  -> set_param_uint8 ptr a
    HWord16 -> set_param_uint16 ptr a
    HWord32 -> set_param_uint32 ptr a
    HWord64 -> set_param_uint64 ptr a
    HFloat  -> set_param_float ptr a
    HDouble -> set_param_double ptr a

withScalar :: Scalar a -> (Ptr Param -> IO b) -> IO b
withScalar (Scalar fptr) f = withForeignPtr fptr f

-- Argument ------------------------------------------------------------

data Expr

data ArgKind
  = InputScalar
      { scalarMin :: Maybe (ForeignPtr Expr)
      , scalarMax :: Maybe (ForeignPtr Expr)
      , scalarDef :: Maybe (ForeignPtr Expr)
      }
  | InputBuffer Word8
  | OutputBuffer Word8

-- | Arguments are used for ahead of time compilation. They can be
--   either scalar or array inputs. Arguments have a type and a name,
--   the type of the argument must match the result of the 'Func' or
--   you'll get a runtime error.
data Argument = Argument
  { argName       :: String
  , argKind       :: ArgKind
  , argDimensions :: Word8
  , argType       :: HalideTypeRep
  }

-- foreign import ccall "scalar_arg"
--   scalar_arg :: Ptr Param -> IO (Ptr Argument)

doubleScalar :: Ptr Param -> IO (Ptr Argument)
doubleScalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<double> p = *((Param<double>*)$(void* paramPtr));
    return new Argument(p);
  }|]

floatScalar :: Ptr Param -> IO (Ptr Argument)
floatScalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<float> p = *((Param<float>*)$(void* paramPtr));
    return new Argument(p);
  }|]

uint8Scalar :: Ptr Param -> IO (Ptr Argument)
uint8Scalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<uint8_t> p = *((Param<uint8_t>*)$(void* paramPtr));
    return new Argument(p);
  }|]

uint16Scalar :: Ptr Param -> IO (Ptr Argument)
uint16Scalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<uint16_t> p = *((Param<uint16_t>*)$(void* paramPtr));
    return new Argument(p);
  }|]

uint32Scalar :: Ptr Param -> IO (Ptr Argument)
uint32Scalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<uint32_t> p = *((Param<uint32_t>*)$(void* paramPtr));
    return new Argument(p);
  }|]

uint64Scalar :: Ptr Param -> IO (Ptr Argument)
uint64Scalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<uint64_t> p = *((Param<uint64_t>*)$(void* paramPtr));
    return new Argument(p);
  }|]

int8Scalar :: Ptr Param -> IO (Ptr Argument)
int8Scalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<int8_t> p = *((Param<int8_t>*)$(void* paramPtr));
    return new Argument(p);
  }|]

int16Scalar :: Ptr Param -> IO (Ptr Argument)
int16Scalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<int16_t> p = *((Param<int16_t>*)$(void* paramPtr));
    return new Argument(p);
  }|]

int32Scalar :: Ptr Param -> IO (Ptr Argument)
int32Scalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<int32_t> p = *((Param<int32_t>*)$(void* paramPtr));
    return new Argument(p);
  }|]

int64Scalar :: Ptr Param -> IO (Ptr Argument)
int64Scalar (castPtr -> paramPtr)  =
  castPtr <$> [block| void* {
    Param<int64_t> p = *((Param<int64_t>*)$(void* paramPtr));
    return new Argument(p);
  }|]

newtype Arg = Arg (ForeignPtr Argument)

scalarArg :: HTypeable a => Scalar a -> IO Arg
scalarArg s@(Scalar fptr) = withForeignPtr fptr $ \ptr -> do
  argPtr <- case htypeFor s of
    HFloat  -> floatScalar ptr
    HDouble -> doubleScalar ptr
    HWord8  -> uint8Scalar ptr
    HWord16 -> uint16Scalar ptr
    HWord32 -> uint32Scalar ptr
    HWord64 -> uint64Scalar ptr
    HInt8   -> int8Scalar ptr
    HInt16  -> int16Scalar ptr
    HInt32  -> int32Scalar ptr
    HInt64  -> int64Scalar ptr
  Arg <$> newForeignPtr delete_arg argPtr

foreign import ccall "image_arg"
  image_arg :: Ptr ImageParam -> IO (Ptr Argument)

foreign import ccall "&delete_arg"
  delete_arg :: FinalizerPtr Argument

-- | Create an Argument
-- argFromParam :: ForeignPtr Param -> IO (ForeignPtr Argument)
-- argFromParam paramPtr = do
--   argPtr <- withForeignPtr paramPtr scalar_arg
--   newForeignPtr delete_arg argPtr

arrayArg :: ArrayParam f a -> IO Arg
arrayArg (ArrayParam fptr) = do
  argPtr <- withForeignPtr fptr image_arg
  Arg <$> newForeignPtr delete_arg argPtr

withArgs :: forall a. [Arg] -> (CInt -> Ptr (Ptr Argument) -> IO a) -> IO a
-- withArgs = coerce (withForeignPtrArray :: [ForeignPtr Argument] -> (CInt -> Ptr (Ptr Argument) -> IO b) -> IO b)
withArgs args = withForeignPtrArray (coerce args)

withForeignPtrArray :: [ForeignPtr a] -> (CInt -> Ptr (Ptr a) -> IO b) -> IO b
withForeignPtrArray fptrs f = do
  let ptrs = map unsafeForeignPtrToPtr fptrs
  b <- withArrayLen ptrs $ \n -> f (fromIntegral n)
  mapM_ touchForeignPtr fptrs
  return b

