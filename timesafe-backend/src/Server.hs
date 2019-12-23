module Server (server) where

import Servant.Server
import Servant.API
import API
import Schema
import Database.Beam
import Database.Beam.Postgres (Postgres, Pg)
import Database.PostgreSQL.Simple (Connection)
import MonadConstraints


server :: MonadPostgres m => ServerT API.API m
server =
    ( allFruits :<|> fruitById ) 
    :<|> return "Hello world" 

allFruits :: MonadPostgres m => m [Fruit]
allFruits =
    runSelectReturningList $ select $ all_ $ (_dbFruit db)

fruitById :: MonadPostgres m => Int -> m (Maybe Fruit)
fruitById id = 
    runSelectReturningOne 
        $ select
        $ filter_ (\fruit -> _fruitId fruit ==. val_ id)
        $ all_ $ (_dbFruit db)