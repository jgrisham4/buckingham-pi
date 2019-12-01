module FileOperations
  (
  readVariablesFromFile,
  writeDimensionlessGroups
  ) where

import           BuckinghamPi
import           Control.Applicative
import qualified Data.List.Split     as S
import qualified Text.Regex.TDFA     as R

-- This function converts from a string to a fundamental unit.  There has to be
-- a better way to do this.
createFundamentalUnit :: String -> FundamentalUnit
createFundamentalUnit unitName
  | unitName == "Mass"        = Mass
  | unitName == "Temperature" = Temperature
  | unitName == "Length"      = Length
  | unitName == "Time"        = Time
  | otherwise                 = error ("Unrecognized unit type" ++ unitName)

extractFundamentalUnits :: String -> [FundamentalUnit]
extractFundamentalUnits rhs = map createFundamentalUnit $ R.getAllTextMatches (rhs R.=~ "(Mass|Length|Time|Temperature)")

extractExponents :: String -> [Exponent]
extractExponents rhs = map read $ R.getAllTextMatches (rhs R.=~ "(-*[0-9]+)")

getSecond :: [a] -> a
getSecond l = l !! 1

-- Reads a file and splits each line at ":".
readFileAndSplit :: FilePath -> IO [[String]]
readFileAndSplit path = fmap (fmap (S.splitOn ":") . lines)  (readFile path)

-- Extracts dimensional variable names from split lines.
getAllNames :: [[String]] -> [String]
getAllNames = map (filter (/=' ') . head)

-- Extracts all fundamental units from split lines.
getAllFundamentalUnits :: [[String]] -> [[FundamentalUnit]]
getAllFundamentalUnits = map (extractFundamentalUnits . getSecond)

-- Extracts all exponents from split lines.
getAllExponents :: [[String]] -> [[Exponent]]
getAllExponents = map (extractExponents . getSecond)

getAllUnits :: [[String]] -> [[(FundamentalUnit,Exponent)]]
getAllUnits splitLines = zipWith zip (getAllFundamentalUnits splitLines) (getAllExponents splitLines)

-- Reads data from the given file and returns dimensional variables.
readVariablesFromFile :: FilePath -> IO [DimensionalVariable]
readVariablesFromFile path = liftA2 (zipWith DimensionalVariable) names units
  where
    splitLines = readFileAndSplit path -- IO [[String]]
    names = getAllNames <$> splitLines
    units = getAllUnits <$> splitLines

writeDimensionlessGroups :: [DimensionlessVariable] -> FilePath -> IO ()
writeDimensionlessGroups groups path = writeFile path (foldl (\l r -> l ++ r ++ "\n") "" (map show groups))
