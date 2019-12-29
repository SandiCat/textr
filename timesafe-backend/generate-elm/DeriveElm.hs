{-# LANGUAGE TemplateHaskell #-}

module DeriveElm where

import Servant.Elm
import qualified Schema

deriveElmDef defaultOptions ''Schema.Fruit
