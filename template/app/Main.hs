{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import RIO hiding (logError, logInfo, (^.))
import Prelude (putStrLn)

import Control.Arrow (Arrow (arr))
import Data.Aeson.Types (ToJSON (..), object, (.=))
import Database.Beam qualified as B
import Database.Beam.Migrate qualified as BM
import Database.Beam.Migrate.SQL.Tables qualified as BMST
import Database.Beam.Migrate.Simple qualified as BMS
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres qualified as BP
import Database.Beam.Postgres.Migrate qualified as BPM
import Database.Beam.Query
  ( all_
  , guard_
  , leftJoin_
  , references_
  , val_
  , (==.)
  )
import Database.Beam.Query qualified as BQ
import Database.PostgreSQL.Simple qualified as P
import Network.HTTP.Types.Status (status400)
import Web.Scotty.Trans (ActionT, Parsable, ScottyT, get, json, post, status, text)
import Web.Scotty.Trans qualified as S

---------------------------------------------------------------------------------

type ExampleDB = ExampleDBv2

pattern ExampleDB
  :: f (B.TableEntity PersonTv2)
  -> f (B.TableEntity BlogPostTv1)
  -> ExampleDBv2 f
pattern ExampleDB {person, blogPost} =
  ExampleDBv2 {person, blogPost}

migrateDB
  :: P.Connection
  -> IO (Maybe (BMS.CheckedDatabaseSettings Postgres ExampleDB))
migrateDB conn =
  BP.runBeamPostgresDebug putStrLn conn
    $ BMS.bringUpToDateWithHooks
      (BMS.defaultUpToDateHooks {BMS.runIrreversibleHook = pure True})
      BPM.migrationBackend
    $ arr id
    >>> initialSetup
    >>> secondStep

-----------------------------------------

data ExampleDBv2 f = ExampleDBv2
  { person :: f (B.TableEntity PersonTv2)
  , blogPost :: f (B.TableEntity BlogPostTv1)
  }
  deriving stock (Generic)
  deriving anyclass (B.Database be)

secondStep
  :: BMS.MigrationSteps
      Postgres
      (BMS.CheckedDatabaseSettings Postgres ExampleDBv1)
      (BMS.CheckedDatabaseSettings Postgres ExampleDBv2)
secondStep = BMS.migrationStep "second_setup" $ \case
  ExampleDBv1 {person, blogPost} -> do
    person' <- migratePersonTv2 person
    blogPost' <- BM.alterTable blogPost pure
    pure ExampleDBv2 {person = person', blogPost = blogPost'}

-----------------------------------------

data ExampleDBv1 f = ExampleDBv1
  { person :: f (B.TableEntity PersonTv1)
  , blogPost :: f (B.TableEntity BlogPostTv1)
  }
  deriving stock (Generic)
  deriving anyclass (B.Database be)

initialSetup
  :: BMS.MigrationSteps
      Postgres
      ()
      (BMS.CheckedDatabaseSettings Postgres ExampleDBv1)
initialSetup = BMS.migrationStep "initial_setup" $ \_ -> do
  person <- createPersonTv1
  blogPost <- createBlogPostTv1
  pure ExampleDBv1 {person, blogPost}

---------------------------------------------------------------------------------

type PersonT = PersonTv2

pattern Person
  :: B.C f Text
  -> B.C f (Maybe Int64)
  -> B.C f Text
  -> PersonTv2 f
pattern Person {name, age, gender} =
  PersonTv2 {name, age, gender}

pattern PersonId
  :: B.C f Text
  -> B.PrimaryKey PersonT f
pattern PersonId {name} =
  PersonIdv2 {name}

instance ToJSON (PersonT Identity)
instance ToJSON (B.PrimaryKey PersonT Identity)

-----------------------------------------

data PersonTv2 f = PersonTv2
  { name :: B.C f Text
  , age :: B.C f (Maybe Int64)
  , gender :: B.C f Text
  }
  deriving stock (Generic)
  deriving anyclass (B.Beamable)

migratePersonTv2
  :: BMS.CheckedDatabaseEntity Postgres ExampleDBv1 (B.TableEntity PersonTv1)
  -> BMS.Migration
      Postgres
      ( BMS.CheckedDatabaseEntity
          Postgres
          ExampleDBv2
          (B.TableEntity PersonTv2)
      )
migratePersonTv2 = flip BM.alterTable $ \v1 -> do
  gender <-
    BM.addColumn
      $ BMST.field
        "gender"
        (B.varchar (Just 255))
        BMST.notNull
        (BM.defaultTo_ (val_ "文鳥"))
  return PersonTv2 {name = v1.name, age = v1.age, gender}

instance B.Table PersonTv2 where
  newtype PrimaryKey PersonTv2 f = PersonIdv2
    { name :: B.C f Text
    }
    deriving stock (Generic)
    deriving anyclass (B.Beamable)
  primaryKey p = PersonIdv2 {name = p.name}
deriving stock instance Show (PersonTv2 Identity)
deriving stock instance Show (B.PrimaryKey PersonTv2 Identity)

-----------------------------------------

data PersonTv1 f = PersonTv1
  { name :: B.C f Text
  , age :: B.C f (Maybe Int64)
  }
  deriving stock (Generic)
  deriving anyclass (B.Beamable)

instance B.Table PersonTv1 where
  newtype PrimaryKey PersonTv1 f = PersonIdv1
    { name :: B.C f Text
    }
    deriving stock (Generic)
    deriving anyclass (B.Beamable)
  primaryKey p = PersonIdv1 {name = p.name}
deriving stock instance Show (PersonTv1 Identity)
deriving stock instance Show (B.PrimaryKey PersonTv1 Identity)

createPersonTv1
  :: BMS.Migration
      Postgres
      ( BMS.CheckedDatabaseEntity
          Postgres
          ExampleDBv1
          (B.TableEntity PersonTv1)
      )
createPersonTv1 =
  BM.createTable "people"
    $ PersonTv1
      { name = BM.field "name" (B.varchar (Just 255)) BM.notNull BM.unique
      , age = BM.field "age" (B.maybeType B.int)
      }

---------------------------------------------------------------------------------

type BlogPostT = BlogPostTv1

pattern BlogPost
  :: B.C f Text
  -> B.PrimaryKey PersonT f
  -> BlogPostTv1 f
pattern BlogPost {title, authorName} =
  BlogPostTv1 {title, authorName}

pattern BlogPostId
  :: B.C f Text
  -> B.PrimaryKey PersonT f
  -> B.PrimaryKey BlogPostT f
pattern BlogPostId {title, authorName} =
  BlogPostIdv1 {title, authorName}

instance ToJSON (BlogPostT Identity)
instance ToJSON (B.PrimaryKey BlogPostT Identity)

-----------------------------------------

data BlogPostTv1 f = BlogPostTv1
  { title :: B.C f Text
  , authorName :: B.PrimaryKey PersonT f
  }
  deriving stock (Generic)
  deriving anyclass (B.Beamable)

instance B.Table BlogPostTv1 where
  data PrimaryKey BlogPostTv1 f = BlogPostIdv1
    { title :: B.C f Text
    , authorName :: B.PrimaryKey PersonT f
    }
    deriving stock (Generic)
    deriving anyclass (B.Beamable)
  primaryKey p = BlogPostIdv1 {title = p.title, authorName = p.authorName}
deriving stock instance Show (BlogPostTv1 Identity)
deriving stock instance Show (B.PrimaryKey BlogPostTv1 Identity)

createBlogPostTv1
  :: BMS.Migration
      Postgres
      ( BMS.CheckedDatabaseEntity
          Postgres
          ExampleDBv1
          (B.TableEntity BlogPostTv1)
      )
createBlogPostTv1 =
  BM.createTable "blog_posts"
    $ BlogPostTv1
      { title = BM.field "title" (B.varchar (Just 255)) BM.notNull
      , authorName = PersonId $ BM.field "author_name" (B.varchar (Just 255)) BM.notNull
      }

---------------------------------------------------------------------------------

main :: IO ()
main = do
  let connInfo =
        P.ConnectInfo
          { connectHost = "localhost"
          , connectPort = 5432
          , connectUser = "postgres"
          , connectPassword = ""
          , connectDatabase = "postgres"
          }
  P.withConnect connInfo $ \conn -> do
    migrateDB conn >>= \case
      Nothing -> putStrLn "Migration failed"
      Just db' -> do
        let db = BMS.unCheckDatabase db'
        S.scottyT 3000 id $ scottyApp conn db

scottyApp :: P.Connection -> B.DatabaseSettings Postgres ExampleDB -> ScottyT IO ()
scottyApp conn db = do
  get "/users/:name" do
    name :: Text <- S.captureParam "name"
    p <- lift
      . BP.runBeamPostgresDebug putStrLn conn
      . BQ.runSelectReturningOne
      . BQ.select
      $ do
        p <- all_ db.person
        guard_ $ p.name ==. val_ name
        pure p
    case p of
      Nothing -> do
        status status400
        json $ object ["error" .= ("User not found" :: Text)]
      Just p' -> json p'
  post "/users/:name" do
    name <- S.captureParam "name"
    age <- queryParamM "age"
    r <-
      try
        . lift
        . BP.runBeamPostgresDebug putStrLn conn
        . BQ.runInsert
        . BQ.insert db.person
        $ BQ.insertValues [Person {name, age, gender = "文鳥"}]
    case r of
      Right _ -> json ()
      Left P.SqlError {sqlState = "23505"} -> do
        text "The name is already taken."
        status status400
      Left e -> throwIO e
  get "/users/:name/blog_posts" do
    name <- S.captureParam "name"
    bs <- lift
      . BP.runBeamPostgresDebug putStrLn conn
      . BQ.runSelectReturningList
      . BQ.select
      $ do
        p <- all_ db.person
        b <-
          leftJoin_
            (all_ db.blogPost)
            (\b -> b.authorName `references_` p)
        guard_ $ p.name ==. val_ name
        pure b
    json $ [b.title | Just b <- bs]
  post "/users/:name/blog_posts" do
    name <- S.captureParam "name"
    title <- S.queryParam "title"
    p <- lift
      . BP.runBeamPostgresDebug putStrLn conn
      . BQ.runSelectReturningOne
      . BQ.select
      $ do
        p <- all_ db.person
        guard_ $ p.name ==. val_ name
        pure p
    if isNothing p
      then do
        status status400
        json $ object ["error" .= ("User not found" :: Text)]
      else do
        r <-
          try
            . lift
            . BP.runBeamPostgresDebug putStrLn conn
            . BQ.runInsert
            . BQ.insert db.blogPost
            $ BQ.insertValues [BlogPost {title, authorName = PersonId name}]
        case r of
          Right _ -> json ()
          Left P.SqlError {sqlState = "23505"} -> do
            text "The user already has a blog post with the same title."
            status status400
          Left e -> throwIO e
    json ()
  get "/healthcheck" do
    liftIO $ putStrLn "healthcheck"
    json
      . object
      $ ["status" ..= "ok"]
  where
    k ..= (v :: Text) = k .= v
    queryParamM
      :: (Parsable a)
      => LText
      -> ActionT IO (Maybe a)
    queryParamM p =
      fmap Just (S.queryParam p)
        `S.rescue` \(_ :: SomeException) -> pure Nothing
