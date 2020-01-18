module Server
    ( server
    )
where

import           Servant.Server
import           Servant.API
import           API
import           Schema
import           Database.Beam
import           Database.Beam.Backend.SQL      ( BeamSqlBackend )
import           Database.Beam.Schema.Tables    ( WithConstraint )
import           Database.Beam.Postgres         ( Postgres
                                                , Pg
                                                )
import           Database.PostgreSQL.Simple     ( Connection )
import           Capabilities

server :: MonadPostgres m => ServerT API.API m
server =
    (
        (allRows (_dbFruit db) :<|> rowById (_dbFruit db) . FruitKey) :<|>
        (allRows (_dbPost db) :<|> rowById (_dbPost db) . PostKey)
    ) :<|>
    ( return "hello world 12211" )

allRows
    :: ( MonadPostgres m
       , Database Postgres db
       , FromBackendRow Postgres (table Identity)
       , Beamable table
       )
    => DatabaseEntity Postgres db (TableEntity table)
    -> m [table Identity]
allRows table = runSelectReturningList $ select $ all_ table

rowById
    :: _ -- the constraints are too scary to name
    => DatabaseEntity be db (TableEntity table)
    -> PrimaryKey table Identity
    -> m (Maybe (table Identity))
rowById table id =
    runSelectReturningOne
        $ select
        $ filter_ (\row -> primaryKey row ==. val_ id)
        $ all_ table