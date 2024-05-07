{-# LANGUAGE OverloadedStrings #-}

module Migrations
  ( runMig
  ) where

import qualified Constants
import           Database.PostgreSQL.Simple           (connectPostgreSQL)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationResult (..),
                                                       defaultOptions,
                                                       runMigration)
import           Database.PostgreSQL.Simple.Util      (existsTable)

runMig :: IO ()
runMig = do
  con <- connectPostgreSQL Constants.connectionString
  isTableCreated <- existsTable con "schema_migrations"
  
  if not isTableCreated
    then do
      initDb con
      runMigr con
    else runMigr con

  where
    initDb con = do
      initializeDbRes <- runMigration con defaultOptions MigrationInitialization
      handleMigrationRes initializeDbRes

    runMigr con = do
      migrationResult <-
        runMigration
          con
          defaultOptions
          (MigrationDirectory Constants.migrationDir)
      handleMigrationRes migrationResult

    handleMigrationRes mr =
      case mr of
        MigrationError err ->
          putStrLn $ "\nError performing migration: " ++ show err
        MigrationSuccess -> putStrLn "\nMigration step completed successfully"
