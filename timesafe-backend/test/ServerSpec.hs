module ServerSpec where

import Capabilities
import qualified Control.Exception.Lifted as Except
import qualified Control.Monad.Error as Error
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Pool as Pool
import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial (..))
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.Postgres.Temp as PgTemp
import DerivedTypes
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Property as Property
import qualified Hedgehog.Range as Range
import Schema
import Server
import Test.Hspec
import Types
import qualified Control.Monad.Trans.Resource as Resource
import Control.Monad.Morph (hoist)

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

eitherToProperty :: Monad m => PropertyT m (Either err a) -> PropertyT m a
eitherToProperty = flip (>>=) $ \case
  Left err ->
    failure
  Right a ->
    return a

spec :: Spec
spec =
  around ((() <$) . Server.withConnection) $ describe "nextPost"
    $ it "returns a post that satisfies a number of conditions"
    $ \conn ->
      require $ prop_nextPost conn

prop_nextPost ::  Connection -> Property
prop_nextPost conn =
  property $
      hoist Resource.runResourceT $
        do
          () <$ Resource.allocate (Pg.begin conn) (const $ Pg.rollback conn)

          users <- forAll $ genIndexedList (Range.linear 10 1000) $ \i -> genUser (SqlSerial i)
          posts <- forAll $ genIndexedList (Range.linear 10 1000) $ \i -> do
            author <- Gen.element users
            return $ Post (SqlSerial i) (primaryKey author) placeholder placeholder
          evalIO $ runBeamPostgres conn $ runInsert $ insert (_dbUserAcc db) $ insertValues users
          evalIO $ runBeamPostgres conn $ runInsert $ insert (_dbPost db) $ insertValues posts
          -- request a post, check conditions, swipe on it, repeat
          numSwipes <- forAll $ Gen.int $ Range.linear 0 $ length posts `div` 10
          replicateM_ numSwipes $ do
            -- takeMVar lock
            user <- forAll $ Gen.element users
            maybePost <- evalIO $ runBeamPostgres conn $ nextPost $ primaryKey user
            case maybePost of
              Nothing -> do
                label "got nothing"
                for_ posts $ \post -> do
                  swipesByMe <-
                    evalIO $ runBeamPostgres conn
                      $ runSelectReturningList
                      $ select
                      $ filter_
                        ( \swipe ->
                            _swipePost swipe `references_` val_ post
                              &&. _swipeWhoSwiped swipe `references_` val_ user
                        )
                      $ all_
                      $ _dbSwipe db
                  assert $
                    or -- every post is either
                      [ _postAuthor post == primaryKey user, -- made by me
                        not $ null swipesByMe -- already seen
                      ]
              Just displayPost -> do
                label "got a post"
                -- find the author and the post of the DisplayPost
                Just (post, author) <- evalIO $ runBeamPostgres conn $ runSelectReturningOne $ select $ do
                  post <- all_ $ _dbPost db
                  guard_ $ val_ (_dpPostId displayPost) `references_` post
                  user <- all_ $ _dbUserAcc db
                  guard_ $ _postAuthor post `references_` user
                  return (post, user)
                -- don't recommend my own posts
                primaryKey author /== primaryKey user
                -- don't recommend posts i've swiped on already
                timesSwiped :: Int <- evalIO $ runBeamPostgres conn $ fmap length $ runSelectReturningList $ select $ do
                  swipe <- all_ $ _dbSwipe db
                  guard_ $ _swipeWhoSwiped swipe `references_` val_ user
                  guard_ $ _swipePost swipe `references_` val_ post
                  return swipe
                timesSwiped === 0
                -- swipe on the post gotten
                choice <- forAll $ Gen.enumBounded @_ @Choice
                ($> ()) . evalIO . runBeamPostgres conn
                  $ swipe (primaryKey user)
                  $ SwipeDecision (primaryKey post) choice
