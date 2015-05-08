module FengShui ( CSV(CSV)
                , parseCSV
                , toCSV
                , principalComponents
                , clamp
                , labels
                , onlyCols
                ) where

import Control.Monad (liftM2)
import Data.Complex (Complex( (:+) ))
import Data.List (intercalate, transpose, elemIndex)
import Data.List.Split (splitOn)
import Data.Packed.Matrix
import Data.Maybe (catMaybes, isJust)
import Debug.Trace (trace)
import Numeric.Container
import Numeric.Extra (floatToDouble)
import Numeric.LinearAlgebra.Algorithms


data CSV a = CSV [String] [[a]] deriving Show

showTrace :: Show a => a -> a
showTrace = trace =<< show

labels :: CSV a -> [String]
labels (CSV l _) = l

onlyCols :: [String] -> CSV a -> CSV a
onlyCols ls (CSV labels rows) = CSV ls rows'
  where rows' = map (map snd . filter go . zip [0..]) rows
        indices = showTrace $ catMaybes $ map (flip elemIndex labels) ls
        go (a, b) = elem a indices

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

stdDev :: [Float] -> Float
stdDev xs = sqrt . average $ map go xs
  where go x = square (x - avg)
        square x = x * x
        avg = average xs

normalizeCSV :: CSV (Maybe Float) -> CSV Float
normalizeCSV (CSV labels rows) = CSV labels rows'
  where avg i = let defined = catMaybes $ map (!! i) rows
                 in average defined
        rows' = map byRow rows
        byRow r = map replacement $ zip [0..] r
        replacement (i, x) = case x of
                               Just x' -> x'
                               Nothing -> avg i

meanAndStdCSV :: CSV Float -> CSV Float
meanAndStdCSV (CSV l rows) = CSV l rows'
  where rows' = map (zipWith3 go means devs) rows
        means = map average $ transpose rows
        devs = map stdDev $ transpose rows
        go m d r = (r - m) / d

toCSV :: String -> CSV Float
toCSV = meanAndStdCSV . normalizeCSV . parseCSV

fromCSV :: Element a => CSV a -> Matrix a
fromCSV (CSV _ rows) = fromLists rows

nthColumn :: Element a => Matrix a -> Int -> [a]
nthColumn m n = toList . flatten . takeColumns 1 $ dropColumns n m

principalComponents :: CSV Float -> [[Double]]
principalComponents csv@(CSV labels _) = map (map realPart . nthColumn final) [0..lastCol]
  where m = fromCSV csv
        cov = m `multiply` trans m
        toComplex = cmap ((:+ 0) . floatToDouble)
        complex = toComplex cov
        lastCol = length labels - 1
        eigs = snd $ eig complex
        final = multiply (toComplex $ trans m) eigs

clamp :: Double -> [[Double]] -> [[Double]]
clamp t = fmap (fmap go)
  where go x = if abs x < t
                  then 0
                  else x

