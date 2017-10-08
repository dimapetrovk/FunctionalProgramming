module Main where

import Gaussiam

main :: IO ()
main = do
   file <- readFile "input1.txt"
   let equations = map toNumbers $ lines file
   print  $ getSolution equations
