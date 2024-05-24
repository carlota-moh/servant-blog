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
      :<|> "users" :> Capture "usrId" Int :> Get '[ JSON] (Maybe User) 
      :<|> "createUser" :> ReqBody '[ JSON] User :> Post '[ JSON] NoContent 
      :<|> "deleteUser" :> Capture "usrId" Int :> Delete '[ JSON] NoContent 
      :<|> "updateUser" :> Capture "usrId" Int :> ReqBody '[ JSON] User :> Put '[ JSON] NoContent

getUserById :: Pool Connection -> Int -> IO (Maybe User)
getUserById conns uid =
  handleUserResponse
    <$> withResource
          conns
          (\conn ->
             query
               conn
               "select * from users where id = ?"
               (Only uid :: Only Int))
  where
    handleUserResponse userResponse =
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

updateUserById :: Pool Connection -> Int -> User -> IO NoContent
updateUserById conns uid updatedUser = do
  targetUser <- getUserById conns uid
  case targetUser of
    -- Throw error here?
    Nothing -> return NoContent
    Just _ -> do
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
    oneUser :: Int -> Handler (Maybe User)
    oneUser uid = liftIO $ getUserById conns uid
    createUser :: User -> Handler NoContent
    createUser user = liftIO $ createUserInDb conns user
    deleteUser :: Int -> Handler NoContent
    deleteUser uid = liftIO (deleteUserById conns uid)
    updateUser :: Int -> User -> Handler NoContent
    updateUser uid u = liftIO (updateUserById conns uid u)

-- boilerplate
myApi :: Proxy UsersAPI
myApi = Proxy
