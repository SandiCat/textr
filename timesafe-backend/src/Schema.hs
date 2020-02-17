{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
-- defined in package.yaml, redefined here for floskell
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Database.Beam
import Database.Beam.Backend.SQL.Types
import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import qualified Types

db :: DatabaseSettings be DB
db = defaultDbSettings

data DB f =
    DB
    { _dbUser :: f (TableEntity UserT)
    , _dbPost :: f (TableEntity PostT)
    , _dbSwipe :: f (TableEntity SwipeT)
    }
    deriving (Generic,Database be)

data UserT f =
    User
    { _userId :: C f (SqlSerial Int)
    , _userAge :: C f Int
    , _userGender :: C f Types.Gender
    }
    deriving (Generic,Beamable,SOP.Generic,SOP.HasDatatypeInfo)

data PostT f =
    Post
    { _postId :: C f (SqlSerial Int)
    , _postAuthor :: PrimaryKey UserT f
    , _postBody :: C f Text
    , _postNickname :: C f Text
    }
-- + max and min age, set of seeking genders, date posted, post edited, active 
    deriving (Generic,Beamable,SOP.Generic,SOP.HasDatatypeInfo)

data SwipeT f =
    Swipe
    { _swipePost :: PrimaryKey PostT f
    , _swipeWhoSwiped :: PrimaryKey UserT f
    , _swipeChoice :: C f Types.Choice
    }
    deriving (Generic,Beamable,SOP.Generic,SOP.HasDatatypeInfo)

-- BOILERPLATE
type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance Aeson.ToJSON User

instance Table UserT where
    data PrimaryKey UserT f = UserKey (C f (SqlSerial Int))
            deriving (Generic,Beamable)

    primaryKey = UserKey <$> _userId

type UserID = PrimaryKey UserT Identity

deriving instance Show UserID

deriving instance Eq UserID

deriving instance Ord UserID

instance Aeson.ToJSON UserID

type Post = PostT Identity

deriving instance Show Post

deriving instance Eq Post

instance Aeson.ToJSON Post

instance Table PostT where
    data PrimaryKey PostT f = PostKey (C f (SqlSerial Int))
            deriving (Generic,Beamable)

    primaryKey = PostKey <$> _postId

type PostID = PrimaryKey PostT Identity

deriving instance Show PostID

deriving instance Eq PostID

deriving instance Ord PostID

instance Aeson.ToJSON PostID

type Swipe = SwipeT Identity

deriving instance Show Swipe

deriving instance Eq Swipe

instance Aeson.ToJSON Swipe

instance Table SwipeT where
    data PrimaryKey SwipeT f =
        SwipeKey (PrimaryKey PostT f) (PrimaryKey UserT f)
            deriving (Generic,Beamable)

    primaryKey = SwipeKey <$> _swipePost <*> _swipeWhoSwiped

type SwipeID = PrimaryKey SwipeT Identity

deriving instance Show SwipeID

deriving instance Eq SwipeID

deriving instance Ord SwipeID

instance Aeson.ToJSON SwipeID