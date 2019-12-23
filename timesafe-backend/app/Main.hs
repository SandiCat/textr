module Main (main) where

import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp
import qualified Database.Beam.Postgres as Beam
import qualified Control.Exception as Exception 

import qualified API
import qualified MonadStack

-- ugly hack until i learn more about exceptions
keepTrying :: IO a -> IO a
keepTrying action =
    Exception.catch @Exception.SomeException
        action
        (const $ keepTrying action)

main :: IO ()
main = do
    conn <- keepTrying $ Beam.connect $
        Beam.defaultConnectInfo
            { Beam.connectPassword = "password" 
            , Beam.connectHost = "db"
            }
    putStrLn "server running"
    Warp.run 8080 $ serve API.apiProxy $ MonadStack.hoistedServer conn
