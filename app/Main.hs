module Main where

import FFIDemo

main :: IO ()
main = do
  print $ fastsin (pi / 2)
