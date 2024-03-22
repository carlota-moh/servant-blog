{-# LANGUAGE DataKinds #-}

module Query () where

import           Lib                 (User (..), myApi)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

queryAllUsers :: ClientM [User]
queryOneUser :: Int -> ClientM User
queryCreateUser :: User -> ClientM User
queryDeleteUser :: Int -> ClientM [User]
queryUpdateUser :: Int -> User -> ClientM [User]
queryAllUsers :<|> queryOneUser :<|> queryCreateUser :<|> queryDeleteUser :<|> queryUpdateUser =
  client myApi

queries :: ClientM ([User], User, User, [User], [User])
queries = do
  allUsersRes <- queryAllUsers
  oneUserRes <- queryOneUser 1
  createUserRes <- queryCreateUser (User 3 "user3" "9901")
  deleteUserRes <- queryDeleteUser 2
  updateUserRes <- queryUpdateUser 1 (User 1 "gonzalo" "5678")
  return (allUsersRes, oneUserRes, createUserRes, deleteUserRes, updateUserRes)

run :: IO ()
run = do
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
