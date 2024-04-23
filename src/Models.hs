{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Models
  ( User(..)
  ) where

import           Data.Aeson                 as JSON
import           Database.PostgreSQL.Simple (FromRow, ToRow)
import           GHC.Generics

data User = User
  { userId       :: Int
  , userName     :: String
  , userPassword :: String
  } deriving (Generic, Eq, Show)
    deriving anyclass (ToRow, FromRow)

instance ToJSON User where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON User
