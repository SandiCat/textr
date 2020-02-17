{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API
import qualified Schema

type REST = 
    ( "posts" :>
        (Get '[JSON] [Schema.Post]
        :<|> Capture "id" Int :> Get '[JSON] (Maybe Schema.Post)
        )
    ) 

type API =
    "api" :> (
        REST :<|>
        ( "test" :> Get '[PlainText] Text
        )
    )

apiProxy :: Proxy API
apiProxy = Proxy