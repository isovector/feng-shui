import Control.Applicative ((<$>))
import Control.Monad (forM_)
import System.Environment (getArgs)
import FengShui
import Debug.Trace (trace)


showTrace :: Show a => a -> a
showTrace = trace =<< show

main = do
    file <- head <$> getArgs
    contents <- readFile file
    let cols = [ "piano" ]
        csv = onlyCols cols $ toCSV contents
        pcs = head . clamp 0 . principalComponents $ csv
    forM_ (zip pcs $ labels csv) (putStrLn . show)
    return ()

