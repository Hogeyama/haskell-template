{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import RIO hiding (logError, logInfo, (^.))
import Prelude (putStrLn)

import MyLib qualified

import Control.Monad.Logger
  ( LoggingT
  , logError
  , logInfo
  , runStdoutLoggingT
  )
import Data.Aeson.Types (ToJSON (..), object, (.=))
import Database.Esqueleto.Experimental
  ( from
  , leftJoin
  , select
  , table
  , val
  , where_
  , (:&) (..)
  , (==.)
  , (?.)
  , (^.)
  )
import Database.Esqueleto.Experimental qualified as E
import Database.Persist.Postgresql qualified as P
import Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )
import Database.PostgreSQL.Simple (SqlError (..))
import Network.HTTP.Types.Status (status400, status500)
import Web.Scotty.Trans as S

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Person
      name String
      age Int Maybe
      UniqueName name
      deriving Show
    BlogPost
      title String
      authorId PersonId
      deriving Show
  |]

instance ToJSON Person where
  toJSON (Person name age) =
    object
      [ "name" .= name
      , "age" .= age
      ]

main :: IO ()
main = do
  let connStr = "host=localhost dbname=postgres user=postgres port=5432"
  runStdoutLoggingT
    . P.withPostgresqlConn connStr
    $ runReaderT do
      u <- askUnliftIO
      $(logInfo) "Running migrations..."
      P.runMigration migrateAll
      $(logInfo) "Starting the server..."
      scottyT 3000 (unliftIO u) scottyApp

scottyApp :: ScottyT (ReaderT P.SqlBackend (LoggingT IO)) ()
scottyApp = do
  get "/users/:name" $ do
    name <- captureParam "name"
    tryAny (lift $ E.getBy (UniqueName name)) >>= \case
      Right Nothing -> do
        text "The user does not exist."
        status status400
      Right (Just (P.Entity _ person)) -> do
        json person
      Left e -> do
        lift $ $(logError) $ tshow e
        status status500
  post "/users/:name" $ do
    name <- captureParam "name"
    age <- queryParamM "age"
    try (lift $ E.insert $ Person name age) >>= \case
      Right _ -> do
        text $ fromString $ MyLib.hello name
      Left SqlError {sqlState = "23505"} -> do
        text "The name is already taken."
        status status400
      Left e -> do
        lift $ $(logError) $ tshow e
        status status500
  get "/users/:name/blog_posts" $ do
    name <- captureParam "name"
    blogPosts <- lift $ select do
      (people :& blogPosts) <-
        from
          $ table @Person
          `leftJoin` table @BlogPost
          `E.on` ( \(people :& blogPosts) ->
                    E.just (people ^. PersonId) ==. blogPosts ?. BlogPostAuthorId
                 )
      where_ $ people ^. PersonName ==. val name
      pure blogPosts
    json
      [ title
      | Just (BlogPost title _) <- fmap E.entityVal <$> blogPosts
      ]
  post "/users/:name/blog_posts" $ do
    name <- captureParam "name"
    lift (E.getBy (UniqueName name)) >>= \case
      Nothing -> do
        text "The user does not exist."
        status status400
      Just (P.Entity userId _) -> do
        title <- queryParam "title"
        _ <- lift $ E.insert $ BlogPost title userId
        text "The blog post has been created."
  get "/healthcheck" $ do
    liftIO $ putStrLn "healthcheck"
    json
      . object
      $ [ "status" ..= "ok"
        ]
  where
    k ..= (v :: Text) = k .= v
    queryParamM
      :: (Parsable a)
      => LText
      -> ActionT (ReaderT P.SqlBackend (LoggingT IO)) (Maybe a)
    queryParamM p =
      fmap Just (S.queryParam p)
        `rescue` \(_ :: SomeException) -> pure Nothing
