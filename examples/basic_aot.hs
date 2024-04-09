
-- This runs the object file generated in basics.hs. Make sure you
-- compile and run basics.hs before trying this.
--
-- Halide's ahead of time compilation (aot) embeds the runtime in the
-- object file so there's no need to link to libHalide. To run this use
--
-- @
-- ghc -O basic_aot.hs doubler.o
-- ./basic_aot
-- @

import Data.Dense.Storable as S
import Data.Dense
import Control.Lens
import Foreign (Ptr, with)

import System.IO.Unsafe

import Halide.Buffer

foreign import ccall unsafe "doubler"
  doubler_c :: Ptr HalideBuffer -> Ptr HalideBuffer -> IO ()

doubler :: SArray V2 Float -> SArray V2 Float
doubler a = fst . unsafePerformIO $
  withSArrayPtr a $ \inputBuffer ->
  createWithBuffer (a^.layout) $ \out ->
  with out $ \outputBuffer ->
    doubler_c inputBuffer outputBuffer

main :: IO ()
main = print $ doubler arr
  where
  arr = S.fromListInto_ (V2 2 3) [1,2,3,40,50,60]


