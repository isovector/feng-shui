import Control.Applicative ((<$>))
import System.Environment (getArgs)
import FengShui

main = do
    file <- head <$> getArgs
    contents <- readFile file
    let matlab = fromCSV $ toCSV contents
    putStrLn $ show matlab

