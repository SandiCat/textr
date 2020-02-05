module Migration where

import Database.Beam.Migrate
import Database.Beam.Postgres (Postgres)
import Capabilities
import qualified Schema

migrationDb :: CheckedDatabaseSettings Postgres Schema.DB
migrationDb = defaultMigratableDbSettings
