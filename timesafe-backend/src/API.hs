{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API
import Schema

type API =
    "fruits" :> 
        (Get '[JSON] [Fruit]
        :<|> Capture "id" Int :> Get '[JSON] (Maybe Fruit)
        )
    :<|> "home"  :> Get '[PlainText] Text

apiProxy :: Proxy API
apiProxy = Proxy