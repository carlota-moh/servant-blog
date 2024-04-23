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
  -- insert single value
  insert1 <-
    execute
      connection
      "insert into users (id, name, password) values (?, ?, ?)"
      (User 1 "Charly" "1999")
  putStrLn $ "Insert 1: " <> show insert1 <> " record(s)"
  -- insert multiple values
  insert2 <-
    executeMany
      connection
      "insert into users (id, name, password) values (?, ?, ?)"
      [User 2 "Dad" "23", User 3 "Mom" "47"]
  putStrLn $ "Insert 2: " <> show insert2 <> " record(s)"
