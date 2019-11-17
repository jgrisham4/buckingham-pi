module BuckinghamPi
    (
    FundamentalUnit(..),
    DimensionalVariable(..),
    DimensionlessVariable(..),
    computeNumberOfGroups,
    buildMatrix,
    ) where

import           Control.Applicative
import           Data.List                     (nub)
import           Numeric.LinearAlgebra.Data
import           Numeric.LinearAlgebra.HMatrix

-- Define sum type for fundamental units (M,L,T,Theta).
data FundamentalUnit = Mass | Length | Time | Temperature deriving (Eq,Show)

-- Define type synonym for Exponent.
type Exponent = Double

-- A variable is defined by the name, along with units, which is a list of tuples which contains
-- the FundamentalUnit along with an exponent.
data DimensionalVariable = DimensionalVariable {name :: String, units :: [(FundamentalUnit, Exponent)]} deriving (Eq,Show)

-- Define data type for a dimensionless variable.
data DimensionlessVariable = DimensionlessVariable {dimensionalVariables :: [DimensionalVariable], exponents :: [Exponent]} deriving (Eq,Show)

getFundamentalUnits :: DimensionalVariable -> [FundamentalUnit]
getFundamentalUnits dimVar = map fst $ units dimVar

getUniqueFundamentalUnits :: [DimensionalVariable] -> [FundamentalUnit]
getUniqueFundamentalUnits = nub . concatMap getFundamentalUnits

numFundamentalUnits :: [DimensionalVariable] -> Int
numFundamentalUnits = length . getUniqueFundamentalUnits

-- Define function that computes the number of dimensionless groups given a list of dimensional variables.
computeNumberOfGroups :: [DimensionalVariable] -> Int
computeNumberOfGroups dimensionalVars = length dimensionalVars - numFundamentalUnits dimensionalVars

-- Define function that returns nonrepeating variables.
getNonrepeatingVars :: [DimensionalVariable] -> [Int] -> [DimensionalVariable]
getNonrepeatingVars dimVars repeatedIndices = [dimVars !! i | i <- [0..(length dimVars - 1)], i `notElem` repeatedIndices]

-- Define a function that returns the exponent on a given unit in the given dimensional variable.
getExponent :: FundamentalUnit -> DimensionalVariable -> Exponent
getExponent unit dimVar
  | unit `elem` map fst (units dimVar) = snd $ head $ filter (\u -> fst u == unit) $ units dimVar
  | otherwise = 0.0

-- Define a function that returns the equation that must be solved.
getEquation ::  [DimensionalVariable] -> FundamentalUnit -> [Exponent]
getEquation varCombination funUnit = map (getExponent funUnit) varCombination

getVariableCombinations :: [DimensionalVariable] -> [DimensionalVariable] -> [[DimensionalVariable]]
getVariableCombinations repeatingVars = map (\x -> repeatingVars ++ [x])

buildMatrix :: [DimensionalVariable] -> [FundamentalUnit] -> Matrix Double
buildMatrix varCombination funUnits = fromLists $ map (getEquation varCombination) funUnits

-- Define function which takes a list of dimensional variables along with a list of integers
-- that identifies which are the repeated variables, and returns a list of dimensionless variables.
--generatePiGroups :: [DimensionalVariable] -> [Int] -> [DimensionlessVariable]
--generatePiGroups dimVars repeatedIndices = zipWith DimensionlessVariable varCombinations ((toList $ lhs <\> rhs) :: [Exponent])
--  where
--    repeatingVars = [dimVars !! i | i <- repeatedIndices]
--    nonrepeatingVars = getNonrepeatingVars dimVars repeatedIndices
--    nGroups = computeNumberOfGroups dimVars
--    funUnits = getUniqueFundamentalUnits dimVars
--    varCombinations = map (\x -> repeatingVars ++ [x]) nonrepeatingVars -- :: [[DimensionalVariable]]
--    equations = (map (getEquation funUnits)) repeatingVars <*> nonrepeatingVars
--    lhs = fromLists $ map init equations
--    rhs = fromList $ map last equations
--    soln = lhs <\> rhs
