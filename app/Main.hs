module Main where

import FFI.Sin

main :: IO ()
main = do
  print $ fastsin (pi / 2)
