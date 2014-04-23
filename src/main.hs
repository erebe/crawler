import System.Environment(getArgs)
import Eztv(fetchEzrtvSeries)


main :: IO ()
main = do
    args <- getArgs
    ss <- fetchEzrtvSeries args
    print ss
    return ()







