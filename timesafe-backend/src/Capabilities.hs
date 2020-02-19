module Capabilities where

import Database.Beam (MonadBeam)
import Database.Beam.Postgres (Postgres)

type MonadPostgres m = MonadBeam Postgres m
