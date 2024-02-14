{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
  ( startApp
  , myApp
  ) where

import           Data.Aeson
import           Data.Foldable            (find)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data User = User
  { _userId       :: Int
  , _userName     :: String
  , _userPassword :: String
  } deriving (Generic, Eq, Show)

instance ToJSON User where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON User

type UsersAPI
  = "users" :> "list-all" :> Get '[ JSON] [User] :<|> "users" :> Capture
      "usrId"
      Int :> Get '[ JSON] User

myUsers :: [User]
myUsers = [User 0 "charly" "1234", User 1 "gon" "5678"]

-- TODO: move to utils
getUserById :: Int -> Maybe User
getUserById usrId = find (\user -> _userId user == usrId) myUsers

checkUser :: Maybe User -> User
checkUser mbUser =
  case mbUser of
    Nothing -> User (-1) "invalid" ""
    Just u  -> u

-- TODO: change this to connect to DB
myServer :: Server UsersAPI
myServer = allUsers :<|> oneUser
  where
    allUsers = return myUsers
    oneUser :: Int -> Handler User
    oneUser = return . checkUser . getUserById

-- boilerplate
myApi :: Proxy UsersAPI
myApi = Proxy

myApp :: Application
myApp = serve myApi myServer

startApp :: IO ()
startApp = do 
  putStrLn "Running server on http://localhost:8080"
  run 8080 myApp
