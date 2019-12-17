module Main (main) where

import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp

import qualified API
import qualified Server

main :: IO ()
main = do
    putStrLn "server running"
    Warp.run 8080 $ serve API.apiProxy Server.server
    putStrLn "server halted"
