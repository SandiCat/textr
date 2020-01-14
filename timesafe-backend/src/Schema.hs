{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Schema where

import           Database.Beam
import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import qualified Types


db :: DatabaseSettings be DB
db = defaultDbSettings 

data DB f = DB
    { _dbFruit:: f (TableEntity FruitT)
    } deriving (Generic, Database be)



data FruitT f = Fruit
    { _fruitId :: C f Int
    , _fruitName :: C f Text
    , _fruitSugarContent :: C f (Maybe Double)
    } deriving (Generic, Beamable, SOP.Generic, SOP.HasDatatypeInfo)

type Fruit = FruitT Identity
deriving instance Show Fruit
deriving instance Eq Fruit
instance Aeson.ToJSON Fruit

instance Table FruitT where
    data PrimaryKey FruitT f =
        FruitKey (C f Int)
        deriving (Generic, Beamable)
    primaryKey = FruitKey <$> _fruitId

type FruitID = PrimaryKey FruitT Identity
deriving instance Show FruitID
deriving instance Eq FruitID
deriving instance Ord FruitID
instance Aeson.ToJSON FruitID


data PostT f = Post
    { _postId :: C f Int
    , _postBody :: C f Text
    , _postNickname :: C f Text
    , _postAge :: C f Int
    , _postGender :: C f Types.Gender
    } deriving (Generic, Beamable, SOP.Generic, SOP.HasDatatypeInfo)

type Post = PostT Identity
deriving instance Show Post
deriving instance Eq Post
instance Aeson.ToJSON Post

instance Table PostT where
    data PrimaryKey PostT f =
        PostKey (C f Int)
        deriving (Generic, Beamable)
    primaryKey = PostKey <$> _postId

type PostID = PrimaryKey PostT Identity
deriving instance Show PostID
deriving instance Eq PostID
deriving instance Ord PostID
instance Aeson.ToJSON PostID