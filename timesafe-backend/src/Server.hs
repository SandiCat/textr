{-# LANGUAGE TypeApplications #-}

module Server
  ( mkApp,
  )
where

import API
import qualified API
import Capabilities
import Control.Monad.Error
import Control.Monad.Except (throwError)
import Database.Beam
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
  ( Pg,
    Postgres,
  )
import qualified Database.Beam.Postgres as Beam
import Database.Beam.Schema.Tables (WithConstraint)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple (Connection)
import DerivedTypes
import Network.Wai (Application)
import Schema
import Servant
import Servant.API
import Servant.Auth.Server
import Servant.Server
import qualified Servant.Server as Servant

type AppM = ExceptT Servant.ServerError Beam.Pg

-- there is not monad transformer version of Pg, which is a shame, so we have to
-- hoist in a roundabout way
-- TODO: write in a simpler way with some kind of mapInner :: (m a -> m' b) -> t m a -> t m b
appMToHandler :: Connection -> AppM a -> Servant.Handler a
appMToHandler conn appM = do
  res <- liftIO $ Beam.runBeamPostgres conn $ runExceptT appM
  case res of
    Left err -> throwError err
    Right a -> return a

type AuthConfig = (CookieSettings, JWTSettings)

hoistedServer :: Connection -> AuthConfig -> Servant.Server API.API
hoistedServer conn cfg =
  Servant.hoistServerWithContext API.apiProxy (Proxy @'[CookieSettings, JWTSettings]) (appMToHandler conn) $ Server.server cfg

instance FromJWT Schema.UserAccID

instance ToJWT Schema.UserAccID

mkApp :: Connection -> AuthConfig -> Application
mkApp conn cfg@(cookieSettings, jwtSettings) =
  serveWithContext
    API.apiProxy
    (cookieSettings :. jwtSettings :. EmptyContext)
    $ hoistedServer conn cfg

server :: AuthConfig -> ServerT API.API AppM
server cfg =
  ( protected
      :<|> login cfg
  )
    :<|> (return "hello world 1221")

protected :: AuthResult UserAccID -> ServerT API.ProtectedAPI AppM
protected (Servant.Auth.Server.Authenticated userId) = nextPost userId :<|> swipe userId
protected _ = throwAll err401

login ::
  (MonadError ServerError m, MonadIO m) =>
  AuthConfig ->
  DerivedTypes.Login ->
  m
    ( Headers
        '[ Header "Set-Cookie" SetCookie,
           Header "Set-Cookie" SetCookie
         ]
        NoContent
    )
login (cookieSettings, jwtSettings) loginInfo =
  let userId = _lUserId loginInfo
   in if userId == Schema.UserID 1
        then do
          mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
          case mApplyCookies of
            Nothing -> do
              liftIO $ putStrLn "wrong login"
              throwError err401
            Just applyCookies -> pure $ applyCookies NoContent
        else throwError err401

nextPost :: forall m. (MonadPostgres m) => UserAccID -> m (Maybe DisplayPost)
nextPost userId =
  fmap (fmap makeDisplayPost) $ runSelectReturningOne $ select
    $ limit_ 1
    $ do
      post <- all_ $ _dbPost db
      user <- all_ $ _dbUserAcc db
      guard_ $ _postAuthor post `references_` user
      return (post, user)

swipe :: forall m. (MonadPostgres m) => UserAccID -> SwipeDecision -> m NoContent
swipe userId swipeDecision =
  NoContent
    <$ ( runInsert $ insert (_dbSwipe db) $ insertValues $ one $
           Swipe
             (_sdPostId swipeDecision)
             undefined
             (_sdChoice swipeDecision)
       )

allRows ::
  ( MonadPostgres m,
    Database Postgres db,
    FromBackendRow Postgres (table Identity),
    Beamable table
  ) =>
  DatabaseEntity Postgres db (TableEntity table) ->
  m [table Identity]
allRows table = runSelectReturningList $ select $ all_ table

rowById ::
  _ => -- the constraints are too scary to name
  DatabaseEntity be db (TableEntity table) ->
  PrimaryKey table Identity ->
  m (Maybe (table Identity))
rowById table id =
  runSelectReturningOne
    $ select
    $ filter_ (\row -> primaryKey row ==. val_ id)
    $ all_ table
