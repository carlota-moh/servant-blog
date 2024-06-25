{-# LANGUAGE DataKinds #-}

module Query
  (
  ) where

import           Lib                 (User (..), myApi)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

queryAllUsers :: ClientM [User]
queryOneUser :: Int -> ClientM User
queryDeleteUser :: Int -> ClientM NoContent
queryUpsertUser :: User -> ClientM NoContent
queryAllUsers :<|> queryOneUser :<|> queryDeleteUser :<|> queryUpsertUser =
  client myApi

runQuery :: (Show a) => ClientM a -> IO ()
runQuery query = do
  manager' <- newManager defaultManagerSettings
  queryResult <-
    runClientM query (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  case queryResult of
    Left err -> putStrLn $ "Error: " ++ show err
    Right result -> print result

-- USAGE EXAMPLES
queries :: ClientM ([User], User, NoContent, NoContent, NoContent)
queries = do
  allUsersRes <- queryAllUsers
  oneUserRes <- queryOneUser 1
  createUserRes <- queryUpsertUser (User 5 "Gon" $ Just 23)
  deleteUserRes <- queryDeleteUser 2
  updateUserRes <- queryUpsertUser (User 5 "Gonzalo" $ Just 22)
  return (allUsersRes, oneUserRes, createUserRes, deleteUserRes, updateUserRes)

runAllQueries :: IO ()
runAllQueries = do
  manager' <- newManager defaultManagerSettings
  queryResults <-
    runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  case queryResults of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (allUsersRes, oneUserRes, createUserRes, deleteUserRes, updateUserRes) -> do
      print allUsersRes
      print oneUserRes
      print createUserRes
      print deleteUserRes
      print updateUserRes
