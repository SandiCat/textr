{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API
import qualified Schema

type API =
    "api" :> (
        ( "fruits" :> 
            (Get '[JSON] [Schema.Fruit]
            :<|> Capture "id" Int :> Get '[JSON] (Maybe Schema.Fruit)
            )
        ) :<|>
        ( "posts" :>
            (Get '[JSON] [Schema.Post]
            :<|> Capture "id" Int :> Get '[JSON] (Maybe Schema.Post)
            )
        )
    )

apiProxy :: Proxy API
apiProxy = Proxy