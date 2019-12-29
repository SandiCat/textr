module Main where

import           Servant.Elm
import System.FilePath ((</>))

import qualified API
import qualified Schema
import qualified DeriveElm

main :: IO ()
main = 
    generateElmModuleWith 
        defElmOptions
        ["Generated", "Api"]
        defElmImports
        (".." </> "timesafe-frontend" </> "src")
        [ DefineElm $ Proxy @Schema.Fruit
        ]
        API.apiProxy
