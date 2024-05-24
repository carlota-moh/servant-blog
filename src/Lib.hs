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
      :<|> "users" :> Capture "usrId" Int :> Get '[ JSON] [User] 
      :<|> "createUser" :> ReqBody '[ JSON] User :> Post '[ JSON] () 
      :<|> "deleteUser" :> Capture "usrId" Int :> Delete '[ JSON] () 
      :<|> "updateUser" :> Capture "usrId" Int :> ReqBody '[ JSON] User :> Put '[ JSON] ()

-- TODO: Handle errors with Maybe
getUserById :: Pool Connection -> Int -> IO [User]
getUserById conns uid =
  withResource
    conns
    (\conn ->
       query conn "select * from users where id = ?" (Only uid :: Only Int))

-- TODO: not empty return type
deleteUserById :: Pool Connection -> Int -> IO ()
deleteUserById conns uid = do 
  void $
    withResource
      conns
      (\conn -> execute conn "delete from users where id = ?" (Only uid :: Only Int))

createUserInDb :: Pool Connection -> User -> IO ()
createUserInDb conns newUser = do
  void
    $ withResource
        conns
        (\conn ->
           execute
             conn
             "insert into users (id, name, age) values (?, ?, ?)"
             newUser)

updateUserById :: Pool Connection -> Int -> User -> IO ()
updateUserById conns uid updatedUser = do
  targetUser <- getUserById conns uid
  case targetUser of
    [] -> return ()
    (x:xs) -> do
      deleteUserById conns uid
      createUserInDb conns updatedUser

myServer :: Pool Connection -> Server UsersAPI
myServer conns =
  allUsers :<|> oneUser :<|> createUser :<|> deleteUser :<|> updateUser
  where
    allUsers :: Handler [User]
    allUsers =
      liftIO $ withResource conns $ \conn -> query_ conn "SELECT * FROM users"
    oneUser :: Int -> Handler [User]
    oneUser uid = liftIO $ getUserById conns uid
    createUser :: User -> Handler ()
    createUser user = liftIO $ createUserInDb conns user
    deleteUser :: Int -> Handler ()
    deleteUser uid = liftIO (deleteUserById conns uid)
    updateUser :: Int -> User -> Handler ()
    updateUser uid u = liftIO (updateUserById conns uid u)

-- boilerplate
myApi :: Proxy UsersAPI
myApi = Proxy
