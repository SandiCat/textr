module Main where

import           Servant.To.Elm
import           Language.Haskell.To.Elm        ( jsonDefinitions )
import qualified Language.Elm.Pretty           as Pretty
import           System.FilePath                ( (</>), (<.>) )
import qualified System.Directory              as Dir
import qualified Data.HashMap.Lazy             as HashMap
import           Data.List                      ( foldl1' )

import qualified API
import qualified Schema
import qualified Types

import TypesElm
import SchemaElm


frontendSrcDir = ".." </> "timesafe-frontend" </> "src"

main :: IO ()
main = do
    let definitions =
            map (elmEndpointDefinition "Config.urlBase" ["Generated", "Api"])
                (elmEndpoints @API.REST)
                <> jsonDefinitions @Schema.Fruit
                <> jsonDefinitions @Schema.Post
                <> jsonDefinitions @Types.Sex
                <> jsonDefinitions @Types.Gender

        modules = Pretty.modules definitions

    forM_ (HashMap.toList modules) $ \(moduleName, contents) -> do
        let moduleRelDir = foldl1' (</>) $ map toString moduleName
        writeFileText (frontendSrcDir </> moduleRelDir <.> "elm") (show contents)
