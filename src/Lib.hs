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
      :<|> "createUser" :> ReqBody '[ JSON] User :> Post '[ JSON] NoContent 
      :<|> "deleteUser" :> Capture "usrId" Int :> Delete '[ JSON] NoContent 
      :<|> "updateUser" :> Capture "usrId" Int :> ReqBody '[ JSON] User :> Put '[ JSON] NoContent

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

-- TODO: Where to handle success operation?
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

updateUserById :: Pool Connection -> Int -> User -> IO NoContent
updateUserById conns uid updatedUser = do
  void $ deleteUserById conns uid
  void $ createUserInDb conns updatedUser
  return NoContent

myServer :: Pool Connection -> Server UsersAPI
myServer conns =
  allUsers :<|> oneUser :<|> createUser :<|> deleteUser :<|> updateUser
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
    createUser :: User -> Handler NoContent
    createUser user = do
      userResponse <- liftIO $ getUserById conns (userId user)
      case userResponse of
        Nothing -> liftIO $ createUserInDb conns user
        Just _ ->
          throwError $ err409 {errBody = "User is already created in database!"}
    deleteUser :: Int -> Handler NoContent
    deleteUser uid = do
      userResponse <- liftIO $ getUserById conns uid
      case userResponse of
        Nothing -> throwError $ err404 {errBody = "User not found."}
        Just _  -> liftIO $ deleteUserById conns uid
    updateUser :: Int -> User -> Handler NoContent
    updateUser uid updatedUser = do
      userResponse <- liftIO $ getUserById conns uid
      case userResponse of
        Nothing -> throwError $ err404 {errBody = "User not found."}
        Just _  -> liftIO $ updateUserById conns uid updatedUser

-- boilerplate
myApi :: Proxy UsersAPI
myApi = Proxy
