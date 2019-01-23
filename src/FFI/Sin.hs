{-# LANGUAGE ForeignFunctionInterface #-}
module FFI.Sin where

import           Foreign
import           Foreign.C.Types

foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble

fastsin :: Double -> Double
fastsin = realToFrac . c_sin . realToFrac
