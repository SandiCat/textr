module Main where

import Database.Beam
import Database.Beam.Postgres
import Schema
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Post" $ do
    it "can be queried in the db" $ do
      let connInfo = ConnectInfo "localhost" 5432 "postgres" "password" "postgres"
      conn <- connect connInfo
      Just post <-
        runBeamPostgresDebug putStrLn conn $ runSelectReturningOne
          $ select
          $ filter_ (\x -> _postId x ==. val_ 2)
          $ all_
          $ (_dbPost db)
      print post
      return ()
