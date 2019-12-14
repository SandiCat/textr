module Main (main) where

import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp

import qualified API
import qualified Server

main :: IO ()
main = Warp.run 80 $ serve API.apiProxy Server.server
