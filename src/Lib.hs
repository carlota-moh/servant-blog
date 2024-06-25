{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Lib
  ( User(..)
  , myServer
  , myApi
  ) where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Functor               (void)
import           Data.Pool                  (Pool, withResource)
import           Database.PostgreSQL.Simple (Connection, Only (Only), execute,
                                             query, query_)
import           Models                     (User (..))
import           Servant

type UsersAPI
  = "users" :> "list-all" :> Get '[ JSON] [User] 
      :<|> "users" :> Capture "usrId" Int :> Get '[ JSON] User 
      :<|> "deleteUser" :> Capture "usrId" Int :> Delete '[ JSON] NoContent 
      :<|> "upsetUser" :> ReqBody '[ JSON] User :> Put '[ JSON] NoContent

getUserById :: Pool Connection -> Int -> IO (Maybe User)
getUserById conns uid =
  handleDbResponse
    <$> withResource
          conns
          (\conn ->
             query
               conn
               "select * from users where id = ?"
               (Only uid :: Only Int))
  where
    handleDbResponse userResponse =
      case userResponse of
        []    -> Nothing
        (x:_) -> Just x

deleteUserById :: Pool Connection -> Int -> IO NoContent
deleteUserById conns uid = do
  _ <-
    withResource
      conns
      (\conn ->
         execute conn "delete from users where id = ?" (Only uid :: Only Int))
  return NoContent

createUserInDb :: Pool Connection -> User -> IO NoContent
createUserInDb conns newUser = do
  void
    $ withResource
        conns
        (\conn ->
           execute
             conn
             "insert into users (id, name, age) values (?, ?, ?)"
             newUser)
  return NoContent

updateUserInDb :: Pool Connection -> User -> IO NoContent
updateUserInDb conns userData = do
  void $ deleteUserById conns (userId userData)
  void $ createUserInDb conns userData
  return NoContent

myServer :: Pool Connection -> Server UsersAPI
myServer conns =
  allUsers :<|> oneUser :<|> deleteUser :<|> upsertUser
  where
    allUsers :: Handler [User]
    allUsers =
      liftIO $ withResource conns $ \conn -> query_ conn "SELECT * FROM users"
    oneUser :: Int -> Handler User
    oneUser uid = do
      userResponse <- liftIO $ getUserById conns uid
      case userResponse of
        Nothing -> throwError $ err404 {errBody = "User not found."}
        Just u  -> return u
    deleteUser :: Int -> Handler NoContent
    deleteUser uid = do
      userResponse <- liftIO $ getUserById conns uid
      case userResponse of
        Nothing -> throwError $ err404 {errBody = "User not found."}
        Just _  -> liftIO $ deleteUserById conns uid
    upsertUser :: User -> Handler NoContent
    upsertUser userData = do
      userResponse <- liftIO $ getUserById conns (userId userData)
      case userResponse of
        Nothing -> liftIO $ createUserInDb conns userData
        Just _  -> liftIO $ updateUserInDb conns userData

-- boilerplate
myApi :: Proxy UsersAPI
myApi = Proxy
