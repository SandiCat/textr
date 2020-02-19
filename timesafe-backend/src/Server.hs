module Server
  ( server,
  )
where

import API
import Capabilities
import Database.Beam
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
  ( Pg,
    Postgres,
  )
import Database.Beam.Schema.Tables (WithConstraint)
import Database.PostgreSQL.Simple (Connection)
import Schema
import Servant.API
import Servant.Server

server :: MonadPostgres m => ServerT API.API m
server =
  ( (allRows (_dbPost db) :<|> rowById (_dbPost db) . PostKey . SqlSerial)
  )
    :<|> (return "hello world 1221")

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
