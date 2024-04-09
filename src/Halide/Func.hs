{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Halide.Func where

import           Data.Dense
import qualified Data.Dense.Storable as S
import           Foreign
import           Foreign.C
import           Foreign.ForeignPtr  (withForeignPtr)
import           Foreign.Storable    (Storable)

import           Halide.Buffer
import           Halide.Param
import           Halide.Target
import           Halide.Var
import           Halide.Type

-- | The halide type for functions.
data Func

-- | Create a new unnamed halide Ptr Func.
foreign import ccall "new_func_unnamed"
  new_func_unnamed :: IO (Ptr Func)

foreign import ccall "new_func_named"
  new_func_named :: CString -> IO (Ptr Func)

foreign import ccall "&delete_func"
  delete_func :: FinalizerPtr Func

newtype Function (f :: * -> *) a = Function (ForeignPtr Func)

-- | Create a new halide function with the given name.
newFunction :: IO (Function f a)
newFunction = do
  ptr <- new_func_unnamed
  Function <$> newForeignPtr delete_func ptr

-- | Create a new halide function with the given name.
newNamedFunction :: String -> IO (Function f a)
newNamedFunction s = do
  ptr <- withCString s new_func_named
  Function <$> newForeignPtr delete_func ptr

createWithPtr
  :: forall a. Storable a
  => V2 Int -> (Ptr a -> IO ()) -> IO (SArray V2 a)
createWithPtr l f = do
  fptr <- mallocForeignPtrArray (size l)
  withForeignPtr fptr f
  return $ S.unsafeFromForeignPtr l fptr

withFunction :: Function f a -> (Ptr Func -> IO b) -> IO b
withFunction (Function f) = withForeignPtr f

-- scheduling ----------------------------------------------------------

foreign import ccall "parallel_func"
  parallel_func :: Ptr Func -> CString -> IO ()

parallel :: Function f a -> Var -> IO ()
parallel f (Var v) =
  withFunction f $ \funcPtr ->
  withCString v $ \nmPtr ->
    parallel_func funcPtr nmPtr

foreign import ccall "parallel_func"
  vectorise_func :: Ptr Func -> CString -> Int32 -> IO ()

vectorise :: Function f a -> Var -> Int -> IO ()
vectorise f (Var v) n =
  withFunction f $ \funcPtr ->
  withCString v $ \nmPtr ->
    vectorise_func funcPtr nmPtr (fromIntegral n)

-- Realizing -----------------------------------------------------------

-- | The destination of a function realisation. Effectively a vector of
--   buffers.
data Realization

-- foreign import ccall "from_buffers"
--   from_buffers :: CInt -> Ptr Int32 -> Ptr Buffer_t -> IO (Ptr Realization)

foreign import ccall "from_new_buffers"
  from_new_buffers :: CInt -> Ptr HalideBuffer -> IO (Ptr Realization)

foreign import ccall "realize"
  realize :: Ptr Func -> Ptr Realization -> IO ()

realise
  :: (Applicative f, Stride f, Shape f, Storable a, HTypeable a)
  => Function f a -> Layout f -> IO (SArray f a)
realise (Function fptr) l = fmap fst $
  withForeignPtr fptr $ \func ->
  createWithBuffer l $ \hb ->
    with hb $ \hbPtr -> do
      real <- from_new_buffers 1 hbPtr
      realize func real

-- actualiseInto
--   :: (Applicative f, Stride f, Shape f, Storable a, HTypeable a)
--   => Function f a -> MSArray f RealWorld a -> IO ()
-- actualiseInto (Function fptr) marray

realise2
  :: (Applicative f, Stride f, Shape f,
      Storable a, HTypeable a,
      Storable b, HTypeable b)
  => Ptr Func -> Layout f -> IO (SArray f a, SArray f b)
realise2 f l =
  createTupleWithBuffer l $ \hbPtr -> do
    real <- from_new_buffers 2 hbPtr
    realize f real

-- Compiling -----------------------------------------------------------

foreign import ccall "to_file"
  to_file
    :: Ptr Func
    -> CString
    -> CInt
    -> Ptr (Ptr Argument)
    -> CString
    -> Ptr Target
    -> IO ()

toFile
  :: Function f a -- ^ function to aot compile
  -> FilePath -- ^ name of file
  -> [Arg] -- ^ arguments of function
  -> String -- ^ aot name of function (leave blank to use use function's initialised name)
  -> Target -- ^ target to compile for
  -> IO ()
toFile func path args name target =
  withFunction func $ \funcPtr ->
  withCString path $ \pathPtr ->
  withArgs args $ \n argPtr ->
  withCString name $ \namePtr ->
  withTarget target $ \targetPtr ->
    to_file funcPtr pathPtr n argPtr namePtr targetPtr

foreign import ccall "to_object"
  to_object
    :: Ptr Func
    -> CString
    -> CInt
    -> Ptr (Ptr Argument)
    -> CString
    -> Ptr Target
    -> IO ()

toObject
  :: Function f a -- ^ function to aot compile
  -> FilePath -- ^ name of file
  -> [Arg] -- ^ arguments of function
  -> String -- ^ aot name of function (leave blank to use use function's initialised name)
  -> Target -- ^ target to compile for
  -> IO ()
toObject func path args name target =
  withFunction func $ \funcPtr ->
  withCString path $ \pathPtr ->
  withArgs args $ \n argPtr ->
  withCString name $ \namePtr ->
  withTarget target $ \targetPtr ->
    to_object funcPtr pathPtr n argPtr namePtr targetPtr

foreign import ccall "to_bitcode"
  to_bitcode
    :: Ptr Func
    -> CString
    -> CInt
    -> Ptr (Ptr Argument)
    -> CString
    -> Ptr Target
    -> IO ()

toBC
  :: Function f a -- ^ function to aot compile
  -> FilePath -- ^ name of file
  -> [Arg] -- ^ arguments of function
  -> String -- ^ aot name of function (leave blank to use use function's initialised name)
  -> Target -- ^ target to compile for
  -> IO ()
toBC func path args name target =
  withFunction func $ \funcPtr ->
  withCString path $ \pathPtr ->
  withArgs args $ \n argPtr ->
  withCString name $ \namePtr ->
  withTarget target $ \targetPtr ->
    to_bitcode funcPtr pathPtr n argPtr namePtr targetPtr

