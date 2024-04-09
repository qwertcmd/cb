{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- This example uses Halide's just in time (jit) and ahead of time (aot)
-- compilation for a simple function that doubles the contents of a 2D
-- array of floats.
--
-- Compilation requires a number of steps. Make sure libHalide is
-- available and DYLD_LIBRARY_PATH is set (see halide-tutorial for
-- details).
--
-- # first run template haskell splice to generate cpp file
-- ghc -O -c basic.hs
-- # compile cpp snippet
-- g++ -c -O basic.cpp -std=c++11 -o basic_c.o
-- # link together
-- ghc -O basic.hs basic_c.o -lHalide -lstdc++
-- # run
-- ./basic
--
-- This should print out the array with its values doubled using the jit
-- compiler. It should also generate a doubler.o file which is used for
-- the basic_aot.hs example.

import Data.Dense.Storable as S
import Data.Dense
import Control.Lens

import Halide

context halideCtx

include "<Halide.h>"
using "namespace Halide"

myFunction :: SArray V2 Float -> IO ()
myFunction a = do
  -- Construct a new ArrayParam which will be the input array.
  inputParam <- newArrayParam @V2 @Float "input"

  -- Construct a halide function
  doubler <- newNamedFunction @V2 @Float "doubler"

  -- define the function
  [block| void {
    ImageParam input = *$array-param:inputParam;
    Func f = *$func:doubler;
    Var x("x"), y("y");
    f(x,y) = input(x,y)*2;
  }|]

  parallel doubler #y
  vectorise doubler #x 4

  -- set the input array
  setArrayParam inputParam a

  -- realise the function into a new array using jit compilation
  a' <- realise doubler (a^.layout)
  print a'

  -- or compile the function to an llvm bitcode file
  inputArg <- arrayArg inputParam
  toObject doubler "doubler.o" [inputArg] "" hostTarget
  toBC doubler "doubler.bc" [inputArg] "" hostTarget

main :: IO ()
main = myFunction arr
  where
    arr = S.fromListInto_ (V2 1024 1024) (fromInteger <$> [0 .. 1024*1024])


