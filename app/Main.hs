module Main
  ( main
  ) where

import           Lib                      (myApi, myServer)
import           Migrations               (runMig)
import           Network.Wai.Handler.Warp
import           Servant

myApp :: Application
myApp = serve myApi myServer

startApp :: IO ()
startApp = do
  runMig
  putStrLn "Running server on http://localhost:8080"
  run 8080 myApp

main :: IO ()
main = startApp
