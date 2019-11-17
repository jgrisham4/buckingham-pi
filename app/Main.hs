module Main where

import           BuckinghamPi
import           Numeric.LinearAlgebra.HMatrix (nullspace)

rho = DimensionalVariable "rho" [(Mass   , 1.0)  , (Length , -3.0)]
vel = DimensionalVariable "v"   [(Length , 1.0)  , (Time   , -1.0)]
l   = DimensionalVariable "l"   [(Length , 1.0)]
mu  = DimensionalVariable "mu"  [(Mass   , 1.0)  , (Length , -1.0)  , (Time , -1.0)]
a   = DimensionalVariable "a"   [(Length , 1.0)  , (Time   , 1.0)]

main :: IO ()
main = print $ nullspace $ buildMatrix [rho, vel, l, mu] [Mass, Length, Time]
