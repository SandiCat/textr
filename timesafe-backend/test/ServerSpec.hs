module ServerSpec where

import Capabilities
import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial (..))
import Database.Beam.Postgres
import DerivedTypes
import Schema
import Server
import Test.Hspec
import Types

postKey :: Int -> PostID
postKey = PostID . SqlSerial

userKey :: Int -> UserAccID
userKey = UserID . SqlSerial

insertSwipes :: Connection -> [Swipe] -> Expectation
insertSwipes conn swipes =
  (() <$) . runBeamPostgresDebug putStrLn conn
    $ runInsert
    $ insert (_dbSwipe db)
    $ insertValues swipes

nextPostShouldBe :: Connection -> UserAccID -> PostID -> Expectation
nextPostShouldBe conn userId postId = do
  res <-
    runBeamPostgresDebug putStrLn conn $
      nextPost userId
  _dpPostId <$> res `shouldBe` Just postId

spec :: Spec
spec =
  around ((() <$) . withTemporaryConnection) $ describe "nextPost"
    $ it "doesn't show posts by users you already swiped on"
    $ \conn -> do
      _ <- runBeamPostgresDebug putStrLn conn $ do
        runInsert
          $ insert (_dbUserAcc db)
          $ insertValues
            [ UserAcc (SqlSerial 0) 22 (Cis Male),
              UserAcc (SqlSerial 1) 22 (Cis Male),
              UserAcc (SqlSerial 2) 22 (Cis Male),
              UserAcc (SqlSerial 3) 22 (Cis Male),
              UserAcc (SqlSerial 4) 22 (Cis Male)
            ]
        runInsert
          $ insert (_dbPost db)
          $ insertValues
            [ Post (SqlSerial 0) (userKey 0) "body" "nickname",
              Post (SqlSerial 1) (userKey 0) "body" "nickname",
              Post (SqlSerial 2) (userKey 0) "body" "nickname",
              Post (SqlSerial 3) (userKey 1) "body" "nickname",
              Post (SqlSerial 4) (userKey 1) "body" "nickname",
              Post (SqlSerial 5) (userKey 1) "body" "nickname",
              Post (SqlSerial 6) (userKey 2) "body" "nickname",
              Post (SqlSerial 7) (userKey 2) "body" "nickname",
              Post (SqlSerial 8) (userKey 2) "body" "nickname",
              Post (SqlSerial 9) (userKey 2) "body" "nickname",
              Post (SqlSerial 10) (userKey 4) "body" "nickname",
              Post (SqlSerial 11) (userKey 4) "body" "nickname"
            ]
      nextPostShouldBe
        conn
        (userKey 0)
        (postKey 0)
      insertSwipes conn [Swipe (postKey 0) (userKey 1) Accepted]
