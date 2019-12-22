module Server where

import Servant.Server
import Servant.API
import qualified Data.Map as Map
import API
import Schema
import Database.Beam


memStore :: Map FruitID Fruit
memStore = Map.fromList 
    $ map (\x -> (primaryKey x, x))
    [ Fruit 0 "avocado" (Just 32.2)
    , Fruit 1 "melon" Nothing
    ]


server :: Server API.API
server =
    ( (return $ toList memStore)
    :<|> (\id -> return $ Map.lookup (FruitKey id) memStore)
    ) 
    :<|> return "Hello world" 
