{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Query where

import           Lib                      (User (..), myApi)
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

queryAllUsers :: ClientM [User]
queryOneUser :: Int -> ClientM User
queryCreateUser :: User -> ClientM User
queryDeleteUser :: Int -> ClientM [User]
queryAllUsers :<|> queryOneUser :<|> queryCreateUser :<|> queryDeleteUser =
  client myApi

queries :: ClientM ([User], User, User, [User])
queries = do
  allUsersRes <- queryAllUsers
  oneUserRes <- queryOneUser 1
  createUserRes <- queryCreateUser (User 3 "user3" "9901")
  deleteUserRes <- queryDeleteUser 2
  return (allUsersRes, oneUserRes, createUserRes, deleteUserRes)

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  queryResults <-
    runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  case queryResults of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (allUsersRes, oneUserRes, createUserRes, deleteUserRes) -> do
      print allUsersRes
      print oneUserRes
      print createUserRes
      print deleteUserRes
