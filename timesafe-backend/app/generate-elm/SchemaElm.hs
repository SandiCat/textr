{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SchemaElm where

import Servant.To.Elm
import Language.Haskell.To.Elm
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char

import qualified Schema
import TypesElm

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

fruitOptions :: Options
fruitOptions = removePrefix "fruit"

instance HasElmType Schema.Fruit where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Schema.Fruit fruitOptions "Generated.Fruit.Fruit"

instance HasElmDecoder Aeson.Value Schema.Fruit where
    elmDecoderDefinition = Just $ deriveElmJSONDecoder @Schema.Fruit
        fruitOptions
        Aeson.defaultOptions
        "Generated.Fruit.decoder"

instance HasElmEncoder Aeson.Value Schema.Fruit where
    elmEncoderDefinition = Just $ deriveElmJSONEncoder @Schema.Fruit
        fruitOptions
        Aeson.defaultOptions
        "Generated.Fruit.encoder"


postOptions :: Options
postOptions = removePrefix "post"

instance HasElmType Schema.Post where
    elmDefinition = Just $ deriveElmTypeDefinition @Schema.Post
        postOptions
        "Generated.Post.Post"

instance HasElmDecoder Aeson.Value Schema.Post where
    elmDecoderDefinition = Just $ deriveElmJSONDecoder @Schema.Post
        postOptions
        Aeson.defaultOptions
        "Generated.Post.decoder"

instance HasElmEncoder Aeson.Value Schema.Post where
    elmEncoderDefinition = Just $ deriveElmJSONEncoder @Schema.Post
        postOptions
        Aeson.defaultOptions
        "Generated.Post.encoder"
