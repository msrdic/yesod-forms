module Main where

import          Yesod
import          Yesod.Form.Jquery
import          Yesod.Default.Util

import          Foundation
import          Dispatch

import          Database.Persist.Sqlite

connectionCount :: Int
connectionCount = 5

main :: IO ()
main = do
    connectionPool <- createSqlitePool ":memory:" connectionCount
    runSqlPersistMPool (runMigration migrateAll) connectionPool
    warpEnv $ FormApp connectionPool