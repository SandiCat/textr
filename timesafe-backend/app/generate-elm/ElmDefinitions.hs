{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ElmDefinitions where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Typeable
import Database.Beam.Backend.SQL.Types
import qualified DerivedTypes
import Language.Elm.Definition
import Language.Elm.Name
import Language.Haskell.To.Elm
import qualified Schema
import Servant.To.Elm
import qualified Types

removePrefix :: Text -> Options
removePrefix fieldPrefixT =
  Options $ \label ->
    if prefix `isPrefixOf` label
      then lowerFirst $ drop (length prefix) label
      else error [i|incorrect prefix found on label #{label} of #{fieldPrefix}|]
  where
    fieldPrefix = toString fieldPrefixT
    prefix = map Char.toLower fieldPrefix
    lowerFirst (x : xs) = Char.toLower x : xs

simpleElmDefinition :: forall a. _ => Text -> Options -> Maybe Definition
simpleElmDefinition typeName options =
  Just $
    deriveElmTypeDefinition @a
      options
      (Qualified ["Generated", typeName] typeName)

simpleElmDecoderDefinition :: forall a. _ => Text -> Options -> Maybe Definition
simpleElmDecoderDefinition typeName options =
  Just $
    deriveElmJSONDecoder @a
      options
      Aeson.defaultOptions
      (Qualified ["Generated", typeName] "decoder")

simpleElmEncoderDefinition :: forall a. _ => Text -> Options -> Maybe Definition
simpleElmEncoderDefinition typeName options =
  Just $
    deriveElmJSONEncoder @a
      options
      Aeson.defaultOptions
      (Qualified ["Generated", typeName] "encoder")

-- this is based on the ToJSON and FromJSON definitions for `SqlSerial a` which basically
-- just encode/decode it as `a`. what if it wrapped it? how would i know if this code works? scary stuff
instance HasElmType a => HasElmType (SqlSerial a) where
  elmType = elmType @a

instance HasElmDecoder Aeson.Value a => HasElmDecoder Aeson.Value (SqlSerial a) where
  elmDecoder = elmDecoder @Aeson.Value @a

instance HasElmEncoder Aeson.Value a => HasElmEncoder Aeson.Value (SqlSerial a) where
  elmEncoder = elmEncoder @Aeson.Value @a

-- DEFINITIONS

instance HasElmType Types.Sex where
  elmDefinition = simpleElmDefinition @Types.Sex "Sex" defaultOptions

instance HasElmDecoder Aeson.Value Types.Sex where
  elmDecoderDefinition = simpleElmDecoderDefinition @Types.Sex "Sex" defaultOptions

instance HasElmEncoder Aeson.Value Types.Sex where
  elmEncoderDefinition = simpleElmEncoderDefinition @Types.Sex "Sex" defaultOptions

instance HasElmType Types.Gender where
  elmDefinition = simpleElmDefinition @Types.Gender "Gender" defaultOptions

instance HasElmDecoder Aeson.Value Types.Gender where
  elmDecoderDefinition = simpleElmDecoderDefinition @Types.Gender "Gender" defaultOptions

instance HasElmEncoder Aeson.Value Types.Gender where
  elmEncoderDefinition = simpleElmEncoderDefinition @Types.Gender "Gender" defaultOptions

instance HasElmType Schema.PostID where
  elmDefinition = simpleElmDefinition @Schema.PostID "PostID" defaultOptions

instance HasElmDecoder Aeson.Value Schema.PostID where
  elmDecoderDefinition = simpleElmDecoderDefinition @Schema.PostID "PostID" defaultOptions

instance HasElmEncoder Aeson.Value Schema.PostID where
  elmEncoderDefinition = simpleElmEncoderDefinition @Schema.PostID "PostID" defaultOptions

instance HasElmType DerivedTypes.DisplayPost where
  elmDefinition = simpleElmDefinition @DerivedTypes.DisplayPost "Post" (removePrefix "_dp")

instance HasElmDecoder Aeson.Value DerivedTypes.DisplayPost where
  elmDecoderDefinition = simpleElmDecoderDefinition @DerivedTypes.DisplayPost "Post" (removePrefix "_dp")

instance HasElmEncoder Aeson.Value DerivedTypes.DisplayPost where
  elmEncoderDefinition = simpleElmEncoderDefinition @DerivedTypes.DisplayPost "Post" (removePrefix "_dp")
