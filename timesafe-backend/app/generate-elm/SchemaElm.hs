{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SchemaElm where

import Servant.To.Elm
import Language.Haskell.To.Elm
import Data.Typeable 
import Language.Elm.Name
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Schema
import qualified Types
import Database.Beam.Backend.SQL.Types

removePrefix :: String -> Options
removePrefix typeName =
    Options $ \label ->
        if prefix `isPrefixOf` label then
            lowerFirst $ drop (length prefix) label
        else
            error [i|incorrect prefix found on label #{label} of #{typeName}|]
    where
        prefix = '_' : map Char.toLower typeName
        lowerFirst (x:xs) = Char.toLower x : xs

options :: forall a. ElmBoilerplate a => Proxy a -> Options
options proxy = removePrefix $ map Char.toLower $ elmUppercaseName proxy

elmDefinition :: forall a. Proxy a -> String -> Maybe Definition
elmDefinition proxy typeName = Just $ deriveElmTypeDefinition @a
    (removePrefix $ map Char.toLower typeName)
    (Qualified ["Generated", toText typeName] $ toText typeName

elmDecoderDefinition :: forall a. Proxy a -> String -> Maybe Definition
elmDecoderDefinition proxy typeName = Just $ deriveElmTypeDefinition @a
    (removePrefix $ map Char.toLower typeName)
    (Qualified ["Generated", toText typeName] "decoder")

elmEncoderDefinition :: forall a. Proxy a -> String -> Maybe Definition
elmEncoderDefinition proxy typeName = Just $ deriveElmTypeDefinition @a
    (removePrefix $ map Char.toLower typeName)
    (Qualified ["Generated", toText typeName] "encoder")

-- this is based on the ToJSON and FromJSON definitions for `SqlSerial a` which basically
-- just encode/decode it as `a`. what if it wrapped it? how would i know if this code works? scary stuff
instance HasElmType a => HasElmType (SqlSerial a) where
    elmType = elmType @a

instance HasElmDecoder Aeson.Value a => HasElmDecoder Aeson.Value (SqlSerial a) where
    elmDecoder = elmDecoder @Aeson.Value @a

instance HasElmEncoder Aeson.Value a => HasElmEncoder Aeson.Value (SqlSerial a) where
    elmEncoder = elmEncoder @Aeson.Value @a