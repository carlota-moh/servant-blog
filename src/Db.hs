-- ref: https://github.com/Zelenya/elephants/blob/main/src/Elephants/PostgresqlSimple.hs
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Db
  ( runDb
  , connectInfo
  ) where

import qualified Constants
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (ConnectInfo (connectDatabase, connectHost, connectPassword, connectPort, connectUser),
                                             Connection, In (In), Only (Only),
                                             defaultConnectInfo, execute,
                                             executeMany, query, query_,
                                             withConnect)
import           Models                     (User (..))

type ConnectionFunction = Connection -> IO ()

connectInfo :: ConnectInfo
connectInfo =
  defaultConnectInfo
    { connectHost = Constants.host
    , connectPort = Constants.portNumber
    , connectDatabase = Constants.database
    , connectUser = Constants.user
    , connectPassword = Constants.password
    }

runDb :: ConnectionFunction -> IO ()
runDb fn = do
  putStrLn "\nRunning database"
  withConnect connectInfo fn
  putStrLn "Done!"

-- USAGE EXAMPLES
insertData :: ConnectionFunction
insertData connection = do
  -- insert single value using model
  insert1 <-
    execute
      connection
      "insert into users (id, name, age) values (?, ?, ?)"
      (User 1 "Charly" $ Just 25)
  putStrLn $ "Insert 1: " <> show insert1 <> " record(s)"
  -- insert multiple values without providing all values (can't use model)
  insert2 <-
    executeMany
      connection
      "insert into users (id, name) values (?, ?)"
      [(2 :: Int, "Dad" :: Text), (3 :: Int, "Mom" :: Text)]
  putStrLn $ "Insert 2: " <> show insert2 <> " record(s)"

queryData :: ConnectionFunction
queryData connection = do
  -- query without substitution
  query1 :: [(Int, Text, Maybe Int)] <- query_ connection "select * from users"
  putStrLn $ "Query 1: " <> show query1
  -- query using where
  query2 :: [User] <-
    query connection "select * from users where age = ?" (Only @Int 23)
  putStrLn $ "Query 2: " <> show query2
  -- query with where + in
  query3 :: [User] <-
    query connection "select * from users where name in ?"
      $ Only (In @[Text] ["Charly", "Gon"])
  putStrLn $ "Query 3: " <> show query3
