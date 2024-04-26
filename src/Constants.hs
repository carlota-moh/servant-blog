-- ref: https://github.com/Zelenya/elephants/blob/main/src/Hardcoded.hs
{-# LANGUAGE OverloadedStrings #-}

module Constants
  ( host
  , port
  , portNumber
  , database
  , user
  , password
  , connectionString
  ) where

import           Data.String (IsString (fromString))

host :: (IsString s) => s
host = "0.0.0.0" -- "localhost"

port :: (IsString s) => s
port = "5435"

portNumber :: (Integral i) => i
portNumber = 5435

database :: (IsString s) => s
database = "warehouse"

user :: (IsString s) => s
user = "postgres"

password :: (IsString s) => s
password = "password"

-- "host=localhost port=5432 user=postgres dbname=warehouse password=password"
connectionString :: (IsString s) => s
connectionString =
  stringify
    [ ("host", host)
    , ("port", port)
    , ("user", user)
    , ("dbname", database)
    , ("password", password)
    ]
  where
    stringify = fromString . unwords . map pair
    pair (key, value) = key <> "=" <> value
