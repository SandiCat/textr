{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Schema where

import           Database.Beam
import qualified Data.Aeson as Aeson


data DB f = DB
    { _dbFruit:: f (TableEntity FruitT)
    } deriving (Generic, Database be)



data FruitT f = Fruit
    { _fruitId :: C f Int
    , _fruitName :: C f Text
    , _fruitSugarContent :: C f (Maybe Float)
    } deriving (Generic, Beamable)



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