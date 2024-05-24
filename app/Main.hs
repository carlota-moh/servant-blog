module Main
  ( main
  ) where

import           Data.Pool                  (Pool, defaultPoolConfig, newPool)
import           Database.PostgreSQL.Simple (Connection, close, connect)
import           Db                         (connectInfo)
import           Lib                        (myApi, myServer)
import           Network.Wai.Handler.Warp
import           Servant

initConnectionPool :: IO (Pool Connection)
initConnectionPool =
  newPool $ defaultPoolConfig (connect connectInfo) close 1 10

runApp :: Pool Connection -> IO ()
runApp pool = run 8080 (serve myApi $ myServer pool)

startApp :: IO ()
startApp = do
  pool <- initConnectionPool
  putStrLn "Running server on http://localhost:8080"
  runApp pool

main :: IO ()
main = startApp
