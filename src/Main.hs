import Control.Applicative ((<$>))
import Control.Monad (forM_)
import System.Environment (getArgs)
import FengShui

main = do
    file <- head <$> getArgs
    contents <- readFile file
    let pcs = head . principalComponents $ toCSV contents
    forM_ pcs (putStrLn . show)
    return ()

