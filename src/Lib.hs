{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( User(..)
  , myServer
  , myApi
  ) where

import           Data.Aeson
import           Data.Foldable            (find)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data User = User
  { userId       :: Int
  , userName     :: String
  , userPassword :: String
  } deriving (Generic, Eq, Show)

instance ToJSON User where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON User

type UsersAPI
  = "users" :> "list-all" :> Get '[ JSON] [User] 
      :<|> "users" :> Capture "usrId" Int :> Get '[ JSON] User 
      :<|> "createUser" :> ReqBody '[ JSON] User :> Post '[ JSON] User 
      :<|> "deleteUser" :> Capture "usrId" Int :> Delete '[ JSON] [User] 
      :<|> "updateUser" :> Capture "usrId" Int :> ReqBody '[ JSON] User :> Put '[ JSON] [User]

myUsers :: [User]
myUsers = [User 0 "charly" "1234", User 1 "gon" "5678"]

-- TODO: move to utils
getUserById :: Int -> Maybe User
getUserById usrId = find (\user -> userId user == usrId) myUsers

-- TODO: move to utils
deleteUserFromList :: Int -> [User]
deleteUserFromList uid = filter (\user -> userId user /= uid) myUsers

-- Not super happy with this approach but I will change it when setting DB connection
updateUserFromList :: Int -> User -> [User]
updateUserFromList uid updatedUser = updatedUser : deleteUserFromList uid

-- TODO: change this to connect to DB
myServer :: Server UsersAPI
myServer = allUsers :<|> oneUser :<|> createUser :<|> deleteUser :<|> updateUser
  where
    allUsers = return myUsers
    oneUser :: Int -> Handler User
    oneUser uid = do 
      case getUserById uid of
        Nothing -> throwError $ err404 { errBody = "User not found." }
        Just u  -> return u
    createUser :: User -> Handler User
    createUser = return
    deleteUser :: Int -> Handler [User]
    deleteUser = return . deleteUserFromList
    updateUser :: Int -> User -> Handler [User]
    updateUser uid u = return $ updateUserFromList uid u

-- boilerplate
myApi :: Proxy UsersAPI
myApi = Proxy