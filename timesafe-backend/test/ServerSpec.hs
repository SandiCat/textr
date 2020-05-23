module ServerSpec where

import Capabilities
import qualified Control.Exception.Lifted as Except
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
import qualified Hedgehog.Range as Range
import Schema
import Server
import Test.Hspec
import Types

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
  around ((() <$) . Server.withPool) $ describe "nextPost"
    $ it "returns a post that satisfies a number of conditions"
    $ \pool ->
      require $ prop_nextPost undefined undefined pool

-- debugSqlException2 = do
--   logRef <- newIORef []
--   Right db <- PgTemp.start
--   Right conn <- Server.connectAndCreateSchema db
--   check $ prop_nextPost logRef conn
--   l <- readIORef logRef
--   let Just h = viaNonEmpty head l
--   PgTemp.stop db
--   return @IO $ h

-- testHowPropsWork =
--   let checkIfZero 0 = 0
--       checkIfZero _ = error "counter not zero"
--       withCounter :: (MonadIO m, MonadBaseControl IO m) => IORef Int -> m a -> m a
--       withCounter var =
--         Except.bracket_
--           (atomicModifyIORef' var (checkIfZero >>> (+ 1) >>> (,())))
--           (atomicModifyIORef' var ((subtract 1) >>> (,())))
--       prop var =
--         property $ withCounter var $ do
--           counter <- readIORef var
--           assert $ counter == 0
--           i <- forAll $ Gen.int $ Range.linear 0 100000
--           cover 30 "even" $ i `mod` 2 == 0
--    in do
--         var <- newIORef @IO (0 :: Int)
--         check $ prop var
--         finalCount <- readIORef var
--         print finalCount

-- testHowPropsWork2 =
--   Server.withTemporaryConnection @IO $ \conn ->
--     check $ withTests 1000 $ property $ do
--       liftIO $ Pg.begin conn
--       prevUsers <- liftIO $ runBeamPostgres conn $ runSelectReturningList $ select $ all_ $ _dbUserAcc db
--       assert $ null prevUsers
--       users <- forAll $ genIndexedList (Range.linear 10 1000) $ \i -> genUser (SqlSerial i)
--       liftIO $ runBeamPostgres conn $ runInsert $ insert (_dbUserAcc db) $ insertValues users
--       liftIO $ Pg.rollback conn

testWithLock = do
  lock <- newMVar ()
  Server.withPool $ \pool ->
    check $ prop_nextPost undefined lock pool

prop_nextPost :: IORef _ -> MVar () -> Pool.Pool Connection -> Property
prop_nextPost logVar lock pool =
  -- prop_nextPost :: Connection -> Property
  -- prop_nextPost conn =
  withShrinks 0
    $ property
    $
    --Server.withinUncommittedTransaction conn $
    Pool.withResource pool
    $ \conn ->
      do
        takeMVar lock
        liftIO $ Pg.begin conn
        prevUsers <- liftIO $ runBeamPostgres conn $ runSelectReturningList $ select $ all_ $ _dbUserAcc db
        assert $ null prevUsers
        users <- forAll $ genIndexedList (Range.linear 10 1000) $ \i -> genUser (SqlSerial i)
        posts <- forAll $ genIndexedList (Range.linear 10 1000) $ \i -> do
          author <- Gen.element users
          return $ Post (SqlSerial i) (primaryKey author) placeholder placeholder
        -- Except.handle @_ @SqlError
        --   ( \e -> do
        --       putStrLn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        --       print e
        --       print users
        --       let loop x = loop x
        --       -- loop undefined
        --       return undefined
        --   )
        identity
          ( do
              -- prevUs <- liftIO $ runBeamPostgres conn $ runSelectReturningList $ select $ all_ $ _dbUserAcc db
              -- prevPosts <- liftIO $ runBeamPostgres conn $ runSelectReturningList $ select $ all_ $ _dbPost db
              -- atomicModifyIORef' logVar (\log -> ((prevUs, prevPosts, users, posts) : log, ()))
              liftIO $ runBeamPostgres conn $ runInsert $ insert (_dbUserAcc db) $ insertValues users
              liftIO $ runBeamPostgres conn $ runInsert $ insert (_dbPost db) $ insertValues posts
          )
        -- request a post, check conditions, swipe on it, repeat
        numSwipes <- forAll $ Gen.int $ Range.linear 0 $ length posts `div` 10
        replicateM_ numSwipes $ do
          user <- forAll $ Gen.element users
          maybePost <- liftIO $ runBeamPostgres conn $ nextPost $ primaryKey user
          case maybePost of
            Nothing -> do
              label "got nothing"
              for_ posts $ \post -> do
                swipesByMe <-
                  liftIO $ runBeamPostgres conn
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
        liftIO $ Pg.rollback conn
        putMVar lock ()
