-- import System.Environment(getArgs)
import qualified Eztv
import qualified Youtube
import System.Directory(getHomeDirectory)
import Control.Applicative((<$>))
import Control.Monad(join)
import Data.Maybe(fromMaybe)
import Control.Concurrent.Async(async, wait)


data Crawlable = Channels [Youtube.Channel]
               | Series [Eztv.Serie]

                deriving (Show, Read)

loadConfigFile :: IO [(String, [String])]
loadConfigFile = do
    configFile <- join $ readFile <$> (++ "/.config/crawler.rc") <$> getHomeDirectory
    return $ read configFile


main :: IO ()
main = do
    -- args <- getArgs
    config <- loadConfigFile
    let actions = [ Series <$> Eztv.fetchSeries (getFromConfig "eztv" config)
                  , Channels <$> Youtube.fetchChannels (getFromConfig "youtube" config)
                  ]  :: [IO Crawlable]

    jobs <- sequence $ async <$> actions
    xs <- mapM wait jobs
    print xs

    where
        getFromConfig key cfg = fromMaybe [] $ lookup key cfg


