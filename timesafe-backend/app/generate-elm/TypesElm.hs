{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypesElm where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Database.Beam.Backend.SQL.Types
import Language.Haskell.To.Elm
import Servant.To.Elm
import qualified Types

-- -- this is based on the ToJSON and FromJSON definitions for `SqlSerial a` which basically
-- -- just encode/decode it as `a`. what if it wrapped it? how would i know if this code works? scary stuff
-- instance HasElmType a => HasElmType (SqlSerial a) where
--     elmType = elmType @a

-- instance HasElmDecoder Aeson.Value a => HasElmDecoder Aeson.Value (SqlSerial a) where
--     elmDecoder = elmDecoder @Aeson.Value @a

-- instance HasElmEncoder Aeson.Value a => HasElmEncoder Aeson.Value (SqlSerial a) where
--     elmEncoder = elmEncoder @Aeson.Value @a

-- instance HasElmType Types.Sex where
--     elmDefinition = Just $ deriveElmTypeDefinition @Types.Sex
--         defaultOptions
--         "Generated.Sex.Sex"

-- instance HasElmDecoder Aeson.Value Types.Sex where
--     elmDecoderDefinition = Just $ deriveElmJSONDecoder @Types.Sex
--         defaultOptions
--         Aeson.defaultOptions
--         "Generated.Sex.decoder"

-- instance HasElmEncoder Aeson.Value Types.Sex where
--     elmEncoderDefinition = Just $ deriveElmJSONEncoder @Types.Sex
--         defaultOptions
--         Aeson.defaultOptions
--         "Generated.Sex.encoder"

-- instance HasElmType Types.Gender where
--     elmDefinition = Just $ deriveElmTypeDefinition @Types.Gender
--         defaultOptions
--         "Generated.Gender.Gender"

-- instance HasElmDecoder Aeson.Value Types.Gender where
--     elmDecoderDefinition = Just $ deriveElmJSONDecoder @Types.Gender
--         defaultOptions
--         Aeson.defaultOptions
--         "Generated.Gender.decoder"

-- instance HasElmEncoder Aeson.Value Types.Gender where
--     elmEncoderDefinition = Just $ deriveElmJSONEncoder @Types.Gender
--         defaultOptions
--         Aeson.defaultOptions
--         "Generated.Gender.encoder"
