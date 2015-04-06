module FengShui (CSV(CSV), parseCSV, toCSV, principalComponents) where

import Control.Monad (liftM2)
import Data.Complex (Complex( (:+) ))
import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)
import Data.Packed.Matrix
import Data.Maybe (catMaybes)
import Numeric.Container
import Numeric.Extra (floatToDouble)
import Numeric.LinearAlgebra.Algorithms


data CSV a = CSV [String] [[a]] deriving Show

readCell :: String -> Maybe Float
readCell s = case s of
               "?" -> Nothing
               ""  -> Just 0
               s'  -> Just $ read s'

parseCSV :: String -> CSV (Maybe Float)
parseCSV s = CSV labels parsed
  where rows = map (splitOn ",") $ lines s
        labels = head rows
        parsed = map (map readCell) $ tail rows

average :: [Float] -> Float
average = liftM2 (/) sum (fromIntegral . length)

normalizeCSV :: CSV (Maybe Float) -> CSV Float
normalizeCSV (CSV labels rows) = CSV labels rows'
  where avg i = let defined = catMaybes $ map (!! i) rows
                 in average defined
        rows' = map byRow rows
        byRow r = map replacement $ zip [0..] r
        replacement (i, x) = case x of
                               Just x' -> x'
                               Nothing -> avg i

meanColumns :: [[Float]] -> [Float]
meanColumns = map average . transpose

meanCSV :: CSV Float -> CSV Float
meanCSV (CSV l rows) = CSV l rows'
  where rows' = map (zipWith subtract means) rows
        means = meanColumns rows

toCSV :: String -> CSV Float
toCSV = normalizeCSV . parseCSV

fromCSV :: Element a => CSV a -> Matrix a
fromCSV (CSV _ rows) = fromLists rows

nthColumn :: Element a => Matrix a -> Int -> [a]
nthColumn m n = toList . flatten . takeColumns 1 $ dropColumns n m

principalComponents :: CSV Float -> [[Double]]
principalComponents csv@(CSV labels _) = map (map realPart . nthColumn eigs) [0..lastCol]
  where m = fromCSV csv
        cov = m `multiply` trans m
        complex = cmap ((:+ 0) . floatToDouble) cov
        lastCol = length labels - 1
        eigs = snd $ eig complex

