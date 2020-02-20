{-# LANGUAGE TypeOperators #-}

module API where

import qualified DerivedTypes
import qualified Schema
import Servant.API

-- | API that is used by the frontend application (in this case, by elm)
type FrontendAPI =
  "next_post" :> Get '[JSON] DerivedTypes.DisplayPost

type API =
  "api"
    :> ( FrontendAPI
           :<|> ( "test" :> Get '[PlainText] Text
                )
       )

apiProxy :: Proxy API
apiProxy = Proxy
