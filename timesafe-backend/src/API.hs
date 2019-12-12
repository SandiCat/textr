{-# LANGUAGE TypeOperators #-}

module API where

import Servant.API

type API =
    "home"  :> Get '[PlainText] Text :<|>
    "thing" :> Capture "id" Int :> Get '[JSON] (Maybe (Int, Text))

apiProxy :: Proxy API
apiProxy = Proxy