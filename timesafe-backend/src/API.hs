{-# LANGUAGE TypeOperators #-}

module API where

import qualified DerivedTypes
import qualified Schema
import Servant
import Servant.API
import Servant.Auth.Server

type ProtectedAPI =
  "next_post" :> Get '[JSON] (Maybe DerivedTypes.DisplayPost) -- TODO: post request
    :<|> "swipe" :> ReqBody '[JSON] DerivedTypes.SwipeDecision :> PostNoContent '[JSON] NoContent

-- | API that is used by the frontend application (in this case, by elm)
type FrontendAPI =
  (Auth '[Cookie] Schema.UserAccID :> ProtectedAPI)
    :<|> ( "login"
             :> ReqBody '[JSON] DerivedTypes.Login
             :> PostNoContent '[JSON]
                  ( Headers
                      '[Header "Set-Cookie" SetCookie]
                      NoContent
                  )
         )

type API =
  "api"
    :> ( FrontendAPI
           :<|> ( "test" :> Get '[PlainText] Text
                )
       )

apiProxy :: Proxy API
apiProxy = Proxy
