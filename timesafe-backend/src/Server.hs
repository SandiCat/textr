{-# LANGUAGE TypeApplications #-}

module Server where

import API
import qualified API
import Capabilities
import qualified Control.Exception.Lifted
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Beam
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Backend.SQL.BeamExtensions
import qualified Database.Beam.Migrate.Simple as Beam
import Database.Beam.Postgres
  ( Pg,
    Postgres,
  )
import qualified Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.Beam.Schema.Tables (WithConstraint)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Options as PgOpts
import qualified Database.Postgres.Temp as PgTemp
import DerivedTypes
import qualified Migration
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

withTempDb ::
  forall m a.
  ( MonadBaseControl IO m,
    MonadIO m
  ) =>
  (PgTemp.DB -> m a) ->
  m (Either PgTemp.StartError a)
withTempDb f =
  runExceptT $
    Control.Exception.Lifted.bracket @(ExceptT PgTemp.StartError m)
      (ExceptT $ liftIO @m $ PgTemp.startConfig config)
      (liftIO . PgTemp.stop)
      (lift . f)
  where
    config =
      mempty
        { PgTemp.connectionOptions =
            mempty
              { -- PgOpts.user = Last $ Just "postgres",
                -- user is the same as linux user 🤔
                PgOpts.password = Last $ Just "password"
              }
        }

connectAndCreateSchema :: MonadIO m => PgTemp.DB -> m (Either PgTemp.StartError Pg.Connection)
connectAndCreateSchema db = runExceptT $ do
  conn <- liftIO $ Pg.connectPostgreSQL $ PgTemp.toConnectionString db
  _ <- liftIO $ Beam.runBeamPostgres conn $ Beam.createSchema migrationBackend Migration.migrationDb
  return conn

withTemporaryConnection ::
  forall m a.
  ( MonadBaseControl IO m,
    MonadIO m
  ) =>
  (Pg.Connection -> m a) ->
  m (Either PgTemp.StartError a)
withTemporaryConnection withConn =
  let withDb :: PgTemp.DB -> ExceptT PgTemp.StartError m ()
      withDb db = do
        conn <- ExceptT $ connectAndCreateSchema db
        lift $ withConn conn
   in runExceptT $ do
        ret <- withTempDb withDb
        liftEither ret

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
  m (Headers '[Header "Set-Cookie" SetCookie] NoContent)
login (cookieSettings, jwtSettings) loginInfo =
  let userId = _lUserId loginInfo
   in if userId == Schema.UserID 1
        then do
          mSessCookie <- liftIO $ makeSessionCookie cookieSettings jwtSettings userId
          case mSessCookie of
            Just sessCookie ->
              return $ addHeader sessCookie NoContent
            Nothing ->
              throwError err500
        else throwError err401

nextPost :: forall m. (MonadPostgres m) => UserAccID -> m (Maybe DisplayPost)
nextPost userId =
  fmap (fmap makeDisplayPost) $ runSelectReturningOne $ select
    $ limit_ 1
    $ do
      author <- all_ $ _dbUserAcc db
      guard_ $ primaryKey author /=. val_ userId -- don't recommend one's own posts
      post <- all_ $ _dbPost db
      guard_ $ _postAuthor post `references_` author -- only on posts by this author

      -- find all the swipes by the logged in user on one of `author`'s posts
      swipe <- all_ $ _dbSwipe db
      guard_ $ _swipeWhoSwiped swipe ==. val_ userId -- only swipes by logged in user
      guard_ $ not_ $ _swipePost swipe `references_` post
      return (post, author)

swipe :: forall m. (MonadPostgres m) => UserAccID -> SwipeDecision -> m NoContent
swipe userId swipeDecision =
  NoContent
    <$ ( runInsert $ insert (_dbSwipe db) $ insertValues $ one $
           Swipe
             (_sdPostId swipeDecision)
             userId
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
