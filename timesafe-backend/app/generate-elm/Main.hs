module Main where

import qualified API
import qualified Data.HashMap.Lazy as HashMap
import Data.List (foldl1')
import qualified Language.Elm.Pretty as Pretty
import Language.Haskell.To.Elm (jsonDefinitions)
import qualified Schema
import SchemaElm
import Servant.To.Elm
import qualified System.Directory as Dir
import System.FilePath ((<.>), (</>))
import qualified Types

frontendElmSrcDir = ".." </> "timesafe-frontend" </> "src" </> "elm"

main :: IO ()
main = do
  return ()
-- let definitions =
--         map (elmEndpointDefinition "Config.urlBase" ["Generated", "Api"])
--             (elmEndpoints @API.REST)
--             <> jsonDefinitions @Schema.Post

--     modules = Pretty.modules definitions

-- forM_ (HashMap.toList modules) $ \(moduleName, contents) -> do
--     let moduleRelDir = foldl1' (</>) $ map toString moduleName
--     writeFileText (frontendElmSrcDir </> moduleRelDir <.> "elm") (show contents)
