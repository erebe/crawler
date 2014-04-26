import System.Environment(getArgs)
import Eztv(fetchEztvSeries)
import Youtube(fetchYoutubeChannels)


main :: IO ()
main = do
    args <- getArgs
    ss <- fetchYoutubeChannels args
    print ss
    return ()






