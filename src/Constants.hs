-- ref: https://github.com/Zelenya/elephants/blob/main/src/Hardcoded.hs

module Constants
  ( host
  , port
  , portNumber
  , database
  , user
  , password
  , connectionString
  ) where

import           Data.String (fromString)
import Data.Word (Word16)

host :: String
host = "0.0.0.0" -- "localhost"

port :: String
port = "5435"

portNumber :: Word16
portNumber = 5435

database :: String
database = "warehouse"

user :: String
user = "postgres"

password :: String
password = "password"

-- "host=localhost port=5432 user=postgres dbname=warehouse password=password"
connectionString :: String
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
