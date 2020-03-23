{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified API
import qualified Data.HashMap.Lazy as HashMap
import Data.List (foldl1')
import qualified DerivedTypes
import ElmDefinitions
import qualified Language.Elm.Pretty as Pretty
import Language.Haskell.To.Elm (jsonDefinitions)
import qualified Schema
import Servant.API
import qualified Servant.Auth.Server as SAS
import Servant.To.Elm
import qualified System.Directory as Dir
import System.FilePath ((<.>), (</>))
import qualified System.Process as Process
import qualified Types

elmSrcDir = "src" </> "elm"

frotendDir = ".." </> "timesafe-frontend"

frontendElmSrcDir = frotendDir </> elmSrcDir

instance HasElmEndpoints restOfApi => HasElmEndpoints (SAS.Auth auths a :> restOfApi) where
  elmEndpoints' = elmEndpoints' @restOfApi

main :: IO ()
main = do
  let definitions =
        map
          (elmEndpointDefinition "Config.urlBase" ["Generated", "Api"])
          (elmEndpoints @API.FrontendAPI)
          <> jsonDefinitions @DerivedTypes.DisplayPost
          <> jsonDefinitions @DerivedTypes.Login
          <> jsonDefinitions @DerivedTypes.SwipeDecision
          <> jsonDefinitions @Schema.PostID
          <> jsonDefinitions @Schema.UserAccID
          <> jsonDefinitions @Types.Sex
          <> jsonDefinitions @Types.Gender
          <> jsonDefinitions @Types.Choice
      modules = Pretty.modules definitions
  forM_ (HashMap.toList modules) $ \(moduleName, contents) -> do
    let moduleRelDir = foldl1' (</>) $ map toString moduleName
    writeFileText (frontendElmSrcDir </> moduleRelDir <.> "elm") (show contents)
  (_, _, _, ph) <-
    Process.createProcess $
      (Process.proc "yarn" ["elm-format", "--yes", elmSrcDir </> "Generated"])
        { Process.cwd = Just frotendDir
        }
  Process.waitForProcess ph
  putStrLn "done"
