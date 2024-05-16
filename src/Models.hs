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
  { userId   :: Int
  , userName :: String
  , userage  :: Maybe Int
  } deriving (Generic, Eq, Show)
    deriving anyclass (ToRow, FromRow, ToJSON, FromJSON)
