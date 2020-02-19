{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate          ( HasDefaultSqlDataType
                                                , defaultSqlDataType
                                                )
import qualified Database.Beam.Backend.SQL as SQL 
import Database.Beam.Postgres (Postgres)

data Sex
    = Male
    | Female
    deriving (Show, Read, Enum, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance Aeson.ToJSON Sex
instance Aeson.FromJSON Sex

-- perhaps it would be better to marshal it using postgres' support for json, but this is simpler
instance HasSqlValueSyntax be String => HasSqlValueSyntax be Sex where
    sqlValueSyntax = autoSqlValueSyntax

instance HasDefaultSqlDataType Postgres Sex where
    defaultSqlDataType _ _ _ = SQL.varCharType Nothing Nothing

instance FromBackendRow Postgres Sex where
    fromBackendRow = fromMaybe (error "invalid format") . readMaybe <$> fromBackendRow
    -- docs say this function can't fail so i'm using error. the guide throws an exception as well


data Gender
    = Cis Sex
    | TransTo Sex
    | Other (Maybe Text) -- | an alternative gender could be named arbitrarily or be left unspecified
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance Aeson.ToJSON Gender
instance Aeson.FromJSON Gender

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Gender where
    sqlValueSyntax = autoSqlValueSyntax

instance HasDefaultSqlDataType Postgres Gender where
    defaultSqlDataType _ _ _ = SQL.varCharType Nothing Nothing

instance FromBackendRow Postgres Gender where
    fromBackendRow =
        fromMaybe (error "invalid format") . readMaybe <$> fromBackendRow

data Choice
    = Accepted
    | Declined
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance Aeson.ToJSON Choice
instance Aeson.FromJSON Choice

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Choice where
    sqlValueSyntax = autoSqlValueSyntax

instance HasDefaultSqlDataType Postgres Choice where
    defaultSqlDataType _ _ _ = SQL.varCharType Nothing Nothing

instance FromBackendRow Postgres Choice where
    fromBackendRow =
        fromMaybe (error "invalid format") . readMaybe <$> fromBackendRow
