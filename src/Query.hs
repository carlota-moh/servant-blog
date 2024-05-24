{-# LANGUAGE DataKinds #-}

module Query
  (
  ) where

import           Lib                 (User (..), myApi)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

-- TODO: createUser should return [User] or User
queryAllUsers :: ClientM [User]
queryOneUser :: Int -> ClientM [User]
queryCreateUser :: User -> ClientM ()
queryDeleteUser :: Int -> ClientM ()
queryUpdateUser :: Int -> User -> ClientM ()
queryAllUsers :<|> queryOneUser :<|> queryCreateUser :<|> queryDeleteUser :<|> queryUpdateUser =
  client myApi

queries :: ClientM ([User], [User], (), (), ())
queries = do
  allUsersRes <- queryAllUsers
  oneUserRes <- queryOneUser 1
  createUserRes <- queryCreateUser (User 5 "Gon" $ Just 23)
  deleteUserRes <- queryDeleteUser 2
  updateUserRes <- queryUpdateUser 5 (User 5 "Gonzalo" $ Just 22)
  return (allUsersRes, oneUserRes, createUserRes, deleteUserRes, updateUserRes)

-- TODO: handle errors?
runQueries :: IO ()
runQueries = do
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
