{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import qualified API
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import Database.Beam.Migrate.Simple
import qualified Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Migrate (migrationBackend)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.Postgres.Temp as PgTemp
import qualified Migration
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Auth.Server
import Servant.Server
import qualified Server
import System.Environment (getArgs)
import System.FilePath ((</>))

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

secret = "\224\156\FSz82\175\212)k\128n\190\203\156\246\165\210\198<#\168\199\132\237\153e{O\172C\157\167nhp\197\192=\EOT\155P\249\FSD\DC3\215\253\166 \192\174\161A\188\196\191D8\FS\174\133\202s\200\185\137h\ENQ\160\b5(7\137B\233\188\234\"\187\173\227au\DELs@\142L\180h\vK\180\SOHk\214\SYN\GS\198KdC\169\243\213\252Y\199G\239\FS\207@\206s\145\EM\151\132\244Ic?<\146!\132R\242U@j\190\149CM,\180\153P\SYN\166GI\182V!\ETB<\FSP\181\196\f\191\208>jf\156v.\217\141}\228\145\255\b\EOT\199\252>V\200B\184+\US\241N+/\151{\217\144n\142\230\177\201\&0\SO\202C\139\254\202\180 5C\153\248\144\227\201\134\128N\EOT\186\178U\158\199\206\v\245\223\246\207\248_\222;T\174[\164\228\218,\227\f\226p\235\238\152\253\141\203=\vH\219J\240\249\187\171\248"

executeSqlFile :: Pg.Connection -> FilePath -> IO Int64
executeSqlFile conn path = do
  queryText <- readFile path
  Pg.execute_ conn $ fromString queryText

develMain :: IO (Either PgTemp.StartError ())
develMain =
  PgTemp.with $ \db -> do
    conn <- Pg.connectPostgreSQL $ PgTemp.toConnectionString db
    Beam.runBeamPostgres conn $ createSchema migrationBackend Migration.migrationDb
    _ <- executeSqlFile conn $ "sql" </> "populate_db.sql"
    putStrLn "server running"
    Warp.run 8080 $
      Server.mkApp
        conn
        ( defaultCookieSettings
            { cookieIsSecure = NotSecure,
              cookieXsrfSetting = Nothing
            },
          defaultJWTSettings $ fromSecret secret
        )

-- withConfigFile :: FilePath -> IO ()
-- withConfigFile configPath = do
--   Just connInfo <- Aeson.decodeFileStrict' @Beam.ConnectInfo configPath
--   conn <- keepTrying $ Beam.connect $ connInfo
--   putStrLn "server running"
--   Warp.run 8080 $ Server.mkApp conn defaultCookieSettings $ defaultJWTSettings $

-- main :: IO ()
-- main = do
--   [configPath] <- getArgs
--   withConfigFile configPath

main = undefined
