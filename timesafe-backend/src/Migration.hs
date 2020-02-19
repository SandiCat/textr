module Migration where

import Capabilities
import Database.Beam.Migrate
import Database.Beam.Postgres (Postgres)
import qualified Schema

migrationDb :: CheckedDatabaseSettings Postgres Schema.DB
migrationDb = defaultMigratableDbSettings
