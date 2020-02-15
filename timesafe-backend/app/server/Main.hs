module Main (main) where

import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp
import qualified Database.Beam.Postgres as Beam
import qualified Control.Exception as Exception 
import qualified Data.Aeson as Aeson
import           System.Environment             ( getArgs )
import           Control.Concurrent (threadDelay)
import qualified Database.Postgres.Temp as PgTemp
import qualified Database.PostgreSQL.Simple as Pg
import System.FilePath ((</>))

import qualified API
import Database.Beam.Migrate.Simple
import qualified MonadStack
import Database.Beam.Postgres.Migrate (migrationBackend)
import qualified Migration

-- ugly hack until i learn more about exceptions
keepTrying :: IO a -> IO a
keepTrying action =
    Exception.catch @Exception.SomeException
        action
        $ \e -> do
            print e
            threadDelay $ 2_000_000
            keepTrying action

instance Aeson.FromJSON Beam.ConnectInfo

withConfigFile :: FilePath -> IO ()
withConfigFile configPath = do
    Just connInfo <- Aeson.decodeFileStrict' @Beam.ConnectInfo configPath
    conn <- keepTrying $ Beam.connect $ connInfo
    putStrLn "server running"
    Warp.run 8080 $ serve API.apiProxy $ MonadStack.hoistedServer conn


executeSqlFile :: Pg.Connection -> FilePath -> IO Int64
executeSqlFile conn path = do
    queryText <- readFile path
    Pg.execute_ conn $ fromString queryText

develMain :: IO (Either PgTemp.StartError ())
develMain =
    PgTemp.with $ \db -> do
        conn <- Pg.connectPostgreSQL $ PgTemp.toConnectionString db
        Beam.runBeamPostgres conn $ autoMigrate migrationBackend Migration.migrationDb
        
        _ <- executeSqlFile conn $ "sql" </> "populate_db.sql"

        putStrLn "server running"
        Warp.run 8080 $ serve API.apiProxy $ MonadStack.hoistedServer conn

main :: IO ()
main = do
    [configPath] <- getArgs
    withConfigFile configPath
