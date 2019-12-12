module Server where

import Servant.Server
import Servant.API
import qualified Data.Map as Map
import API


memStore :: Map Int Text
memStore = fromList 
    [ (2, "broj dva")
    , (1, "we are number one")
    , (5, "2+2")
    ]


server :: Server API.API
server =
    return "Hello world" :<|>
    \id -> return $ (id,) <$> Map.lookup id memStore
