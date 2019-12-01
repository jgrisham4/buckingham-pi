module Main where

import           BuckinghamPi
import           FileOperations
import           Numeric.LinearAlgebra.HMatrix

rho = DimensionalVariable "rho" [(Mass   , 1.0)  , (Length , -3.0)]
vel = DimensionalVariable "v"   [(Length , 1.0)  , (Time   , -1.0)]
l   = DimensionalVariable "l"   [(Length , 1.0)]
mu  = DimensionalVariable "mu"  [(Mass   , 1.0)  , (Length , -1.0)  , (Time , -1.0)]
a   = DimensionalVariable "a"   [(Length , 1.0)  , (Time   , -1.0)]

dimVars = [rho, vel, l, mu, a]
funUnits = [Mass, Length, Time]

--writeDimensionlessGroups :: [DimensionlessVariable] -> FilePath -> IO ()
main :: IO ()
main = fmap (\d -> generatePiGroups d [0..(numFundamentalUnits dimVars - 1)]) (readVariablesFromFile "./app/dimensional_variables") >>= (\v -> writeDimensionlessGroups v "dimensionless_groups")
