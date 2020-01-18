module Main (main) where

import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp
import qualified Database.Beam.Postgres as Beam
import qualified Control.Exception as Exception 
import qualified Data.Aeson as Aeson
import           System.Environment             ( getArgs )
import           Control.Concurrent (threadDelay)

import qualified API
import qualified MonadStack

-- ugly hack until i learn more about exceptions
keepTrying :: IO a -> IO a
keepTrying action =
    Exception.catch @Exception.SomeException
        action
        $ \e -> do
            print e
            threadDelay $ 100_000
            keepTrying action

instance Aeson.FromJSON Beam.ConnectInfo

withConfigFile :: FilePath -> IO ()
withConfigFile configPath = do
    Just connInfo <- Aeson.decodeFileStrict' @Beam.ConnectInfo configPath
    conn <- keepTrying $ Beam.connect $ connInfo
    putStrLn "server running"
    Warp.run 8080 $ serve API.apiProxy $ MonadStack.hoistedServer conn

develMain = withConfigFile "../build-system/build-src/dev-host/server-config.json"


main :: IO ()
main = do
    [configPath] <- getArgs
    withConfigFile configPath
