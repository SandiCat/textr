{-# LANGUAGE TemplateHaskell #-}

module DerivedTypes where

import qualified Data.Aeson as Aeson
import Database.Beam (primaryKey)
import qualified Generics.SOP as SOP
import Optics
import qualified Schema
import qualified Types

-- | A post as sent to the frontend
data DisplayPost
  = DisplayPost
      { _dpAge :: Int,
        _dpGender :: Types.Gender,
        _dpPostId :: Schema.PostID,
        _dpBody :: Text,
        _dpNickname :: Text
      }
  deriving (Show, Generic)

makeLenses ''DisplayPost

instance Aeson.FromJSON DisplayPost

instance Aeson.ToJSON DisplayPost

instance SOP.Generic DisplayPost

instance SOP.HasDatatypeInfo DisplayPost

makeDisplayPost :: (Schema.Post, Schema.UserAcc) -> DisplayPost
makeDisplayPost (post@Schema.Post {..}, Schema.UserAcc {..}) =
  DisplayPost
    { _dpAge = _userAge,
      _dpGender = _userGender,
      _dpPostId = primaryKey post,
      _dpBody = _postBody,
      _dpNickname = _postNickname
    }

-- | Represents the choice of a user about another user, whether they "swiped left or right"
-- Sent from the frotend
data SwipeDecision
  = SwipeDecision
      { _sdPostId :: Schema.PostID,
        _sdChoice :: Types.Choice
      }
