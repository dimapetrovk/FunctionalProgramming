module Gaussiam
    ( toNumbers,
      getSolution
    ) where

import System.Environment
import Control.Parallel.Strategies

-- transform line of numbers to number array
toNumbers :: String -> [Double]
toNumbers line = map read $ words line ::[Double]

-- normalize row numbers
normalize :: Int -> [Double] -> [Double]
normalize index equation = map (/ (equation !! index)) equation

-- multiply number and each number from array
multiply :: Double -> [Double] -> [Double]
multiply m eq = map (m*) eq

-- transform system to triangle system, reverse it, get solution of triangle system, reverse again to get right order of answer
getSolution::[[Double]] -> [Double]
getSolution system = reverse $ getSolutionRec (reverse $ toTriangular system) []

-- get solution 
getSolutionRec::[[Double]]->[Double]->[Double]
getSolutionRec [] _= []
getSolutionRec (row:rows) params = 
	let row' = reverse row 
	    x0 = head row' 
	    coefs = tail row'
	    res = x0 - (sum $ zipWith (*) params coefs)
	     in res : (getSolutionRec rows (params++[res]))

toTriangular :: [[Double]] -> [[Double]]
toTriangular system = toTriangularRec system 0

-- process system iteration successively
toTriangularRec :: [[Double]] -> Int -> [[Double]]
toTriangularRec [] index = []
toTriangularRec (curRow:rows) index = 
	let normalized = normalize index curRow in
	    normalized : (toTriangularRec (runEval (processSystem rows index curRow)) (index + 1)) -- need to be evaluated seq 


-- process system rows parallel
processSystem :: [[Double]]  -> Int-> [Double] -> Eval [[Double]]
processSystem system index row  = rseq $ parMap (rpar) (processSingleRow row index) system -- need to be evaluated parallel

processSingleRow::[Double]->Int->[Double]->[Double]
processSingleRow pivotRow index curRow = zipWith (-) curRow ( (curRow !! index) `multiply`  (normalize index pivotRow))