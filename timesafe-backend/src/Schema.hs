{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- defined in package.yaml, redefined here for floskell

module Schema where

import qualified Data.Aeson as Aeson
import Database.Beam
import Database.Beam.Backend.SQL.Types
import qualified Generics.SOP as SOP
import qualified Types

db :: DatabaseSettings be DB
db = defaultDbSettings

data DB f
  = DB
      { _dbUserAcc :: f (TableEntity UserAccT),
        _dbPost :: f (TableEntity PostT),
        _dbSwipe :: f (TableEntity SwipeT)
      }
  deriving (Generic, Database be)

data UserAccT f -- user is a reserved word in postgres
  = UserAcc
      { _userId :: C f (SqlSerial Int),
        _userAge :: C f Int,
        _userGender :: C f Types.Gender
      }
  deriving (Generic, Beamable)

data PostT f
  = Post
      { _postId :: C f (SqlSerial Int),
        _postAuthor :: PrimaryKey UserAccT f,
        _postBody :: C f Text,
        _postNickname :: C f Text
      }
  -- + max and min age, set of seeking genders, date posted, post edited, active
  deriving (Generic, Beamable)

data SwipeT f
  = Swipe
      { _swipePost :: PrimaryKey PostT f,
        _swipeWhoSwiped :: PrimaryKey UserAccT f,
        _swipeChoice :: C f Types.Choice
      }
  deriving (Generic, Beamable)

-- BOILERPLATE
type UserAcc = UserAccT Identity

deriving instance Show UserAcc

deriving instance Eq UserAcc

instance Aeson.FromJSON UserAcc

instance Aeson.ToJSON UserAcc

instance SOP.Generic UserAcc

instance SOP.HasDatatypeInfo UserAcc

instance Table UserAccT where
  data PrimaryKey UserAccT f = UserID (C f (SqlSerial Int))
    deriving (Generic, Beamable)

  primaryKey = UserID <$> _userId

type UserAccID = PrimaryKey UserAccT Identity

deriving instance Show UserAccID

deriving instance Eq UserAccID

deriving instance Ord UserAccID

instance Aeson.FromJSON UserAccID

instance Aeson.ToJSON UserAccID

instance SOP.Generic UserAccID

instance SOP.HasDatatypeInfo UserAccID

type Post = PostT Identity

deriving instance Show Post

deriving instance Eq Post

instance Aeson.FromJSON Post

instance Aeson.ToJSON Post

instance SOP.Generic Post

instance SOP.HasDatatypeInfo Post

instance Table PostT where
  data PrimaryKey PostT f = PostID (C f (SqlSerial Int))
    deriving (Generic, Beamable)

  primaryKey = PostID <$> _postId

type PostID = PrimaryKey PostT Identity

deriving instance Show PostID

deriving instance Eq PostID

deriving instance Ord PostID

instance Aeson.FromJSON PostID

instance Aeson.ToJSON PostID

instance SOP.Generic PostID

instance SOP.HasDatatypeInfo PostID

type Swipe = SwipeT Identity

deriving instance Show Swipe

deriving instance Eq Swipe

instance Aeson.FromJSON Swipe

instance Aeson.ToJSON Swipe

instance SOP.Generic Swipe

instance SOP.HasDatatypeInfo Swipe

instance Table SwipeT where
  data PrimaryKey SwipeT f
    = SwipeID (PrimaryKey PostT f) (PrimaryKey UserAccT f)
    deriving (Generic, Beamable)

  primaryKey = SwipeID <$> _swipePost <*> _swipeWhoSwiped

type SwipeID = PrimaryKey SwipeT Identity

deriving instance Show SwipeID

deriving instance Eq SwipeID

deriving instance Ord SwipeID

instance Aeson.FromJSON SwipeID

instance Aeson.ToJSON SwipeID

instance SOP.Generic SwipeID

instance SOP.HasDatatypeInfo SwipeID
