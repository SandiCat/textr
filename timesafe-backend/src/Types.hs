{-# LANGUAGE DeriveAnyClass #-}

module Types where

import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP

data Sex
    = Male
    | Female
    deriving (Show, Enum, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance Aeson.ToJSON Sex
instance Aeson.FromJSON Sex

data Gender
    = Cis Sex
    | TransTo Sex
    | Other (Maybe Text) -- | an alternative gender could be named arbitrarily or be left unspecified
    deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance Aeson.ToJSON Gender
instance Aeson.FromJSON Gender
