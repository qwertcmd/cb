{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Module exposing a 'Context' to inline C++ code.  We only have used
-- this for experiments, so use with caution.  See the C++ tests to see
-- how to build inline C++ code.
module Halide.Inline
  ( module Language.C.Inline
  , module Halide.Inline
  ) where

import qualified Data.Map                            as M
import           Data.Monoid                         (mempty, (<>))
import           Foreign
import           Foreign.ForeignPtr                  (withForeignPtr)
import           Language.C.Inline
import           Language.C.Inline.Context
import           Language.C.Inline.HaskellIdentifier
import qualified Language.C.Types                    as C
import           Language.Haskell.TH                 (Exp, Q, Type)
import qualified Language.Haskell.TH                 as TH
import qualified Language.Haskell.TH.Syntax          as TH

import           Halide.Buffer
import           Halide.Func
import           Halide.Param
import           Halide.Target
import           Halide.Var


-- | Emits an @using@ directive, e.g.
--
-- @
-- C.using "namespace std" ==> using namespace std
-- @
-- usingHalide :: String -> TH.DecsQ
-- usingHalide = do
--   include "<Halide.h>"
--   verbatim "using namespace Halide;"

-- | Declares a @using@ for the cpp file:
--
-- @
-- using "namespace Halide" => using namespace Halide;
-- @
using :: String -> TH.Q [TH.Dec]
using s = verbatim $ "using " ++ s ++ ";"

------------------------------------------------------------------------
-- inline-c
------------------------------------------------------------------------

tyName :: String -> C.TypeSpecifier
tyName s = C.TypeName nm where
  Right nm = C.cIdentifierFromString s

-- | A context that uses a @.cpp@ extension and wraps all functions in
--   @extern \"C\"@.
cppCtx :: Context
cppCtx = baseCtx <> mempty
  { ctxFileExtension = Just "cpp"
  -- { ctxForeignSrcLang = Just TH.LangCxx
  , ctxOutput        = Just $ \s -> concat ["extern \"C\" {\n", s ,"}"]
  , ctxTypesTable    = M.fromList
      [ (C.Double, [t|Double|])
      , (C.Float, [t|Float|])
      ]
  }

-- | @cppCtx@ with extras halide internal types for marshalling between
--   the two:
-- @
-- 'Func' <-> Func
-- 'HalideBuffer' <-> halide_buffer_t
-- 'Argument' <-> Argument
-- 'ImageParam' <-> ImageParam
-- 'Var' <-> Var
-- 'Target' <-> Target
-- @
halideCtx :: Context
halideCtx = cppCtx <> mempty
  { ctxTypesTable = M.fromList
      [ (tyName "Func", [t|Func|])
      , (tyName "halide_buffer_t", [t|HalideBuffer|])
      , (tyName "Argument", [t|Argument|])
      , (tyName "ImageParam", [t|ImageParam|])
      , (tyName "Var", [t|Var|])
      , (tyName "Target", [t|Target|])
      ]
  , ctxAntiQuoters = halideAntiQuoters
  }

-- | This AntiQuoter allows you to quote an 'SArray' as a
--   @hallide_buffer_t*@. This can then be converted to a halide
--   @Buffer<T>@ (where @T@ is the corresponding hallide type of the
--   elements of the array) for use as an input array for a 'Func'.
--
--   The usage is @$buffer:a@ for @a :: SArray f a@ which gives a
--   @hallide_buffer_t*@. Usually you'll want to put this in a
--   @Buffer<T>@ using the buffer initialiser
--   @Buffer<float>(*$buffer:a)@ (note here @*@ is used to dereference the
--   @hallide_buffer_t*@, it's not part of the @inline-c@ syntax).
--
--  @
-- addOne :: 'SArray' 'V2' 'Int32' -> 'IO' ('SArray' 'V2' 'Int32')
-- addOne a = do
--   func <- 'newFunction'
--   ['block'| void {
--     Buffer<int32_t> input(*$buffer:a);
--     Func f = *$(Func* func);
--     Var x,y;
--     f(x,y) = input(x,y) + 1;
--   } |]
--   'actualise' func ('V2' 8 6)
--  @
--
--  It is imporant to note that in situations like this it is __your__
--  responsabiliy to ensure that any input arrays for functions like
--  this are still alive when calling actualise.
bufferCtx :: Context
bufferCtx = mempty
  { ctxAntiQuoters = M.fromList
      [ ("buffer", SomeAntiQuoter buffAQ)
      , ("var", SomeAntiQuoter varAQ)
      , ("scalar", SomeAntiQuoter scalarAQ)
      , ("target", SomeAntiQuoter targetAQ)
      , ("image-param", SomeAntiQuoter imgAQ)
      , ("array-param", SomeAntiQuoter arrAQ)
      , ("func", SomeAntiQuoter funcAQ)
      ]
  }

halideAntiQuoters :: M.Map String SomeAntiQuoter
halideAntiQuoters = M.fromList
  [ ("buffer", SomeAntiQuoter buffAQ)
  , ("var", SomeAntiQuoter varAQ)
  , ("scalar", SomeAntiQuoter scalarAQ)
  , ("target", SomeAntiQuoter targetAQ)
  , ("image-param", SomeAntiQuoter imgAQ)
  , ("array-param", SomeAntiQuoter arrAQ)
  , ("func", SomeAntiQuoter funcAQ)
  ]

-- | Many Halide antiquoters are of the same form. They take a haskell
--   data type and have a corresponding fixed C++ type.
mkAQ :: String -> String -> Q Type -> (Q Exp -> Q Exp) -> AntiQuoter HaskellIdentifier
mkAQ cidentString errorString hType hExpression = AntiQuoter
  { aqParser = do
      hId <- C.parseIdentifier
      let cId = mangleHaskellIdentifier hId
          Right bufferIdent = C.cIdentifierFromString cidentString
      let bufferTypeName = C.TypeName bufferIdent
      return (cId, C.Ptr [] (C.TypeSpecifier mempty bufferTypeName), hId)
  , aqMarshaller = \_purity _cTypes _cTy cId -> do
      hsTy  <- hType
      hsExp <- hExpression $ getHsVariable errorString cId
      return (hsTy, hsExp)
  }

getHsVariable :: String -> HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

buffAQ :: AntiQuoter HaskellIdentifier
buffAQ = mkAQ "HalideBuffer" "halide_buffer_t" [t|Ptr HalideBuffer|] (\nm -> [| withSArrayPtr $nm |])

varAQ :: AntiQuoter HaskellIdentifier
varAQ = mkAQ "Var" "var" [t|Ptr Var|] (\nm -> [| withVar $nm |])

scalarAQ :: AntiQuoter HaskellIdentifier
scalarAQ = mkAQ "Param" "scalar" [t|Ptr Param|] (\nm -> [| withScalar $nm |])

targetAQ :: AntiQuoter HaskellIdentifier
targetAQ = mkAQ "Target" "target" [t|Ptr Target|] (\nm -> [| withTarget $nm |])

imgAQ :: AntiQuoter HaskellIdentifier
imgAQ = mkAQ "ImageParam" "image-parameter" [t|Ptr ImageParam|] (\nm -> [| withForeignPtr $nm |])

arrAQ :: AntiQuoter HaskellIdentifier
arrAQ = mkAQ "ImageParam" "image-parameter" [t|Ptr ImageParam|] (\nm -> [| withArrayParam $nm |])

funcAQ :: AntiQuoter HaskellIdentifier
funcAQ = mkAQ "Func" "function" [t|Ptr Func|] (\nm -> [| withFunction $nm |])

