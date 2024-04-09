{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Halide.Var where

import           Data.Coerce
import           Data.Proxy
import           Foreign
import           Foreign.C
import           GHC.OverloadedLabels
import           GHC.TypeLits
import           Language.C.Inline.Cpp as C

context cppCtx -- (halideCtx <> fptrCtx <> bufferCtx)

include "<Halide.h>"
using "namespace Halide"

verbatim "void delete_var(Var* x) { delete x; }"

foreign import ccall "&delete_var"
  delete_var :: FinalizerPtr Var

-- | A Halide @Var@ type.
--
--   In halide, variables are completely defined by their name. Two
--   variables with the same name can be interchanged.
newtype Var = Var String
  deriving (Show, Eq, Ord)

instance KnownSymbol x => IsLabel x Var where
  fromLabel = coerce (symbolVal (Proxy @ x))

newVar :: Var -> IO (ForeignPtr Var)
newVar (Var x) = withCString x $ \cptr -> do
  varPtr <- castPtr <$>
    [block| void* { return (void*)(new Var($(char* cptr))); }|]
  newForeignPtr delete_var varPtr

withVar :: Var -> (Ptr Var -> IO a) -> IO a
withVar (Var x) f = withCString x $ \cptr -> do
  vPtr <- castPtr <$>
    [block| void* { return new Var($(char* cptr)); }|]
  a <- f vPtr
  let vPtr' = castPtr vPtr
  [block| void { delete ((Var*)$(void* vPtr')); } |]
  return a

