{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import RIO hiding (logError, logInfo, (^.))
import Prelude (putStrLn)

import Data.Aeson.Types (ToJSON (..), object, (.=))
import Database.Beam qualified as B
import Database.Beam.Migrate qualified as BM
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

data ExampleDB f = ExampleDB
  { person :: f (B.TableEntity PersonT)
  , blogPost :: f (B.TableEntity BlogPostT)
  }
  deriving stock (Generic)
  deriving anyclass (B.Database be)

data PersonT f = Person
  { name :: B.C f Text
  , age :: B.C f (Maybe Int64)
  }
  deriving stock (Generic)
  deriving anyclass (B.Beamable)

instance B.Table PersonT where
  newtype PrimaryKey PersonT f = PersonId
    { name :: B.C f Text
    }
    deriving stock (Generic)
    deriving anyclass (B.Beamable)
  primaryKey p = PersonId {name = p.name}
instance ToJSON (PersonT Identity)
instance ToJSON (B.PrimaryKey PersonT Identity)
deriving stock instance Show (PersonT Identity)
deriving stock instance Show (B.PrimaryKey PersonT Identity)

data BlogPostT f = BlogPost
  { title :: B.C f Text
  , authorName :: B.PrimaryKey PersonT f
  }
  deriving stock (Generic)
  deriving anyclass (B.Beamable)

instance B.Table BlogPostT where
  data PrimaryKey BlogPostT f = BlogPostId
    { title :: B.C f Text
    , authorName :: B.PrimaryKey PersonT f
    }
    deriving stock (Generic)
    deriving anyclass (B.Beamable)
  primaryKey p = BlogPostId {title = p.title, authorName = p.authorName}
instance ToJSON (BlogPostT Identity)
instance ToJSON (B.PrimaryKey BlogPostT Identity)
deriving stock instance Show (BlogPostT Identity)
deriving stock instance Show (B.PrimaryKey BlogPostT Identity)

initialSetup :: BMS.Migration Postgres (BMS.CheckedDatabaseSettings Postgres ExampleDB)
initialSetup = do
  person <-
    BM.createTable "people"
      $ Person
        { name = BM.field "name" (B.varchar (Just 255)) BM.notNull BM.unique
        , age = BM.field "age" (B.maybeType B.int)
        }
  blogPost <-
    BM.createTable "blog_posts"
      $ BlogPost
        { title = BM.field "title" (B.varchar (Just 255)) BM.notNull
        , authorName = PersonId $ BM.field "author_name" (B.varchar (Just 255)) BM.notNull
        }
  pure ExampleDB {person, blogPost}

initialSetupStep
  :: BMS.MigrationSteps
      Postgres
      ()
      (BMS.CheckedDatabaseSettings Postgres ExampleDB)
initialSetupStep = BMS.migrationStep "initial_setup" (const initialSetup)

migrateDB :: P.Connection -> IO (Maybe (BMS.CheckedDatabaseSettings Postgres ExampleDB))
migrateDB conn =
  BP.runBeamPostgresDebug putStrLn conn
    $ BMS.bringUpToDateWithHooks
      (BMS.defaultUpToDateHooks {BMS.runIrreversibleHook = pure True})
      BPM.migrationBackend
      initialSetupStep

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
        $ BQ.insertValues [Person {name, age}]
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
