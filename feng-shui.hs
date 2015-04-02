import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

data CSV a = CSV [String] [[a]] deriving Show

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                   "" -> []
                   s' -> w : wordsWhen p s''
                     where (w, s'') = break p s'


readCell :: String -> Maybe Float
readCell s = case s of
               "?" -> Nothing
               ""  -> Just 0
               s'  -> Just $ read s'

parseCSV :: String -> CSV (Maybe Float)
parseCSV s = CSV labels parsed
  where rows = map (wordsWhen (==',')) $ lines s
        labels = head rows
        parsed = map (map readCell) $ tail rows

normalizeCSV :: CSV (Maybe Float) -> CSV Float
normalizeCSV (CSV labels rows) = CSV labels rows'
  where avg i = let defined = catMaybes $ map (!! i) rows
                 in sum defined / (fromIntegral $ length defined)
        rows' = map byRow rows
        byRow r = map replacement $ zip [0..] r
        replacement (i, x) = case x of
                               Just x' -> x'
                               Nothing -> avg i

toCSV :: String -> CSV Float
toCSV = normalizeCSV . parseCSV

matlabFriendly :: Show a => CSV a -> String
matlabFriendly (CSV _ rows) = unlines $ map (intercalate "," . map show) rows

main = do
    file <- head <$> getArgs
    contents <- readFile file
    let matlab = matlabFriendly $ toCSV contents
    writeFile ("processed_" ++ file) matlab

