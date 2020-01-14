{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypesElm where

import           Servant.To.Elm
import           Language.Haskell.To.Elm
import qualified Data.Aeson                    as Aeson
import qualified Data.Char                     as Char

import qualified Types


instance HasElmType Types.Sex where
    elmDefinition = Just $ deriveElmTypeDefinition @Types.Sex
        defaultOptions
        "Generated.Sex.Sex"

instance HasElmDecoder Aeson.Value Types.Sex where
    elmDecoderDefinition = Just $ deriveElmJSONDecoder @Types.Sex
        defaultOptions
        Aeson.defaultOptions
        "Generated.Sex.decoder"

instance HasElmEncoder Aeson.Value Types.Sex where
    elmEncoderDefinition = Just $ deriveElmJSONEncoder @Types.Sex
        defaultOptions
        Aeson.defaultOptions
        "Generated.Sex.encoder"


instance HasElmType Types.Gender where
    elmDefinition = Just $ deriveElmTypeDefinition @Types.Gender
        defaultOptions
        "Generated.Gender.Gender"

instance HasElmDecoder Aeson.Value Types.Gender where
    elmDecoderDefinition = Just $ deriveElmJSONDecoder @Types.Gender
        defaultOptions
        Aeson.defaultOptions
        "Generated.Gender.decoder"

instance HasElmEncoder Aeson.Value Types.Gender where
    elmEncoderDefinition = Just $ deriveElmJSONEncoder @Types.Gender
        defaultOptions
        Aeson.defaultOptions
        "Generated.Gender.encoder"
