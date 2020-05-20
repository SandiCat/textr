module ServerSpec where

import Capabilities
import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial (..))
import Database.Beam.Postgres
import DerivedTypes
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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
  res <- runBeamPostgres conn $ nextPost userId
  _dpPostId <$> res `shouldBe` Just postId

placeholder :: Text
placeholder = "this shouldn't matter"

genGender :: Gen Gender
genGender =
  Gen.choice
    [ Cis <$> Gen.enumBounded @_ @Sex,
      TransTo <$> Gen.enumBounded @_ @Sex,
      pure $ Other Nothing,
      pure $ Other $ Just placeholder
    ]

genUser :: SqlSerial Int -> Gen UserAcc
genUser id =
  UserAcc
    <$> pure id
    <*> Gen.int (Range.linear 12 70)
    <*> genGender

genIndexedList :: (MonadGen m) => Range.Range Int -> (Int -> m a) -> m [a]
genIndexedList range f = do
  len <- Gen.int range
  forM [1 .. len] f

spec :: Spec
spec =
  around ((() <$) . withTemporaryConnection) $ describe "nextPost"
    $ it "returns a post that satisfies a number of conditions"
    $ \conn ->
      require $ property $ do
        users <- forAll $ genIndexedList (Range.linear 10 1000) $ \i -> genUser (SqlSerial i)
        posts <- forAll $ genIndexedList (Range.linear 10 1000) $ \i -> do
          author <- Gen.element users
          return $ Post (SqlSerial i) (primaryKey author) placeholder placeholder
        liftIO $ runBeamPostgresDebug putStrLn conn $ runInsert $ insert (_dbUserAcc db) $ insertValues users
        liftIO $ runBeamPostgresDebug putStrLn conn $ runInsert $ insert (_dbPost db) $ insertValues posts
        -- request a post, check conditions, swipe on it, repeat
        numSwipes <- forAll $ Gen.int $ Range.linear 0 200
        replicateM_ numSwipes $ do
          user <- forAll $ Gen.element users
          maybePost <- liftIO $ runBeamPostgres conn $ nextPost $ primaryKey user
          case maybePost of
            Nothing -> do
              label "got nothing"
              -- TODO: check if there were any posts to choose from
              discard
            Just displayPost -> do
              label "got a post"
              -- find the author and the post of the DisplayPost
              Just (post, author) <- liftIO $ runBeamPostgres conn $ runSelectReturningOne $ select $ do
                post <- all_ $ _dbPost db
                guard_ $ val_ (_dpPostId displayPost) `references_` post
                user <- all_ $ _dbUserAcc db
                guard_ $ _postAuthor post `references_` user
                return (post, user)
              -- don't recommend my own posts
              primaryKey author /== primaryKey user
              -- don't recommend posts i've swiped on already
              timesSwiped :: Int <- liftIO $ runBeamPostgres conn $ fmap length $ runSelectReturningList $ select $ do
                swipe <- all_ $ _dbSwipe db
                guard_ $ _swipeWhoSwiped swipe `references_` val_ user
                guard_ $ _swipePost swipe `references_` val_ post
                return swipe
              timesSwiped === 0
              -- swipe on the post gotten
              choice <- forAll $ Gen.enumBounded @_ @Choice
              ($> ()) . liftIO . runBeamPostgres conn
                $ swipe (primaryKey user)
                $ SwipeDecision (primaryKey post) choice
