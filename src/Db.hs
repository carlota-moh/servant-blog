-- ref: https://github.com/Zelenya/elephants/blob/main/src/Elephants/PostgresqlSimple.hs
{-# LANGUAGE OverloadedStrings #-}

module Db
  ( runDb
  ) where

import qualified Constants
import           Database.PostgreSQL.Simple
import           Models                     (User (..))

runDb :: IO ()
runDb = do
  putStrLn "\nRunning database"
  let connectInfo =
        defaultConnectInfo
          { connectHost = Constants.host
          , connectPort = Constants.portNumber
          , connectDatabase = Constants.database
          , connectUser = Constants.user
          , connectPassword = Constants.password
          }
  withConnect connectInfo $ do
    insertStuff

insertStuff :: Connection -> IO ()
insertStuff connection = do
  -- insert single value using model
  insert1 <-
    execute
      connection
      "insert into users (id, name, age) values (?, ?, ?)"
      (User 1 "Charly" 25)
  putStrLn $ "Insert 1: " <> show insert1 <> " record(s)"
  -- insert multiple values without providing all values (can't use model)
  insert2 <-
    executeMany
      connection
      "insert into users (id, name) values (?, ?)"
      [(2 :: Int, "Dad" :: String), (3 :: Int, "Mom" :: String)]
  putStrLn $ "Insert 2: " <> show insert2 <> " record(s)"
