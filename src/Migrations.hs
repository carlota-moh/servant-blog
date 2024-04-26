{-# LANGUAGE OverloadedStrings #-}

module Migrations
  ( runMig
  ) where

import qualified Constants
import           Database.PostgreSQL.Simple           (connectPostgreSQL)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory),
                                                       MigrationResult (..),
                                                       defaultOptions,
                                                       runMigration)

runMig :: IO ()
runMig = do
  con <- connectPostgreSQL Constants.connectionString
  migrationRes <-
    runMigration con defaultOptions (MigrationDirectory Constants.migrationDir)
  case migrationRes of
    MigrationError err ->
      putStrLn $ "\nError performing migration: " ++ show err
    MigrationSuccess -> putStrLn "\nMigration completed successfully"
