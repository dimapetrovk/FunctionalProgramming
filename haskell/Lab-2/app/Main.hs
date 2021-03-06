module Main where

import Gaussiam

main :: IO ()
main = do
   file <- readFile "input.txt"
   -- transform line to number array
   let equations = map toNumbers $ lines file
   -- resolve system
   print  $ getSolution equations
