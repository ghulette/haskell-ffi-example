{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.C

-- unsafe means that callbacks are not allowed
-- If this function had a side effect it should be declared IO
foreign import ccall unsafe "math.h sin" c_sin :: CDouble -> CDouble

main :: IO ()
main = do
  putStrLn $ show $ c_sin pi
  