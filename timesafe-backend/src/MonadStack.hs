module MonadStack (hoistedServer) where


import qualified Database.Beam.Postgres as Beam 
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Except (throwError)
import qualified Servant.Server as Servant

import qualified Server
import qualified API

type AppM a = ExceptT Servant.ServerError Beam.Pg a

-- there is not monad transformer version of Pg, which is a shame, so we have to
-- hoist in a roundabout way

appMToHandler :: Connection -> AppM a -> Servant.Handler a
appMToHandler conn appM = do
    res <- liftIO $ Beam.runBeamPostgres conn $ runExceptT appM
    case res of
        Left err -> throwError err
        Right a -> return a

hoistedServer :: Connection -> Servant.Server API.API
hoistedServer conn =
    Servant.hoistServer API.apiProxy (appMToHandler conn) Server.server
