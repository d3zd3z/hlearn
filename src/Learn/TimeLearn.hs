-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |A model for spaced repetition system learning

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- This module is intended to be imported qualified.

module Learn.TimeLearn (
   TimeLearn, kind,
   Problem(..),
   TimeLearnException(..),
   create,
   open,
   close,
   Populator,
   populate,
   addProblem
) where

import Control.Exception (Exception, throwIO)
import Data.Convertible (Convertible)
import Data.Foldable (for_)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

data TimeLearn = TimeLearn {
   tlConn :: ConnWrapper,
   kind :: String }

-- A single problem.
data Problem = Problem {
   pId :: Int,
   pQuestion :: String,
   pAnswer :: String,
   pNext :: POSIXTime,
   pInterval :: NominalDiffTime }

-- When populating the database, the `Populator` restricts the update
-- operation to something within a single transaction.
newtype Populator = Populator TimeLearn

data TimeLearnException
   = MalformedResult
   | InvalidSchemaVersion String String
   deriving Show
instance Exception TimeLearnException

create :: String -> String -> IO TimeLearn
create path kind = do
   conn <- connectSqlite3 path
   for_ schema $ \query -> do
      runRaw conn query
   _ <- run conn "INSERT INTO config VALUES ('kind', ?)" [toSql kind]
   _ <- run conn "INSERT INTO schema_version VALUES (?)" [toSql schemaVersion]
   commit conn
   return $ TimeLearn { tlConn = ConnWrapper conn, kind = kind }

open :: String -> IO TimeLearn
open path = do
   conn <- connectSqlite3 path
   vers <- query1 conn "SELECT version FROM schema_version" []
   if vers == schemaVersion
      then return ()
      else throwIO $ InvalidSchemaVersion schemaVersion vers

   kind <- query1 conn "SELECT value FROM config WHERE key = 'kind'" []

   return $ TimeLearn { tlConn = ConnWrapper conn, kind = kind }

-- Run the action given the populator, which can be used to add items.
populate :: TimeLearn -> (Populator -> IO ()) -> IO ()
populate tl@TimeLearn{..} act = do
   withTransaction tlConn (\_ -> act $ Populator tl)

addProblem :: Populator -> String -> String -> IO ()
addProblem (Populator TimeLearn{..}) question answer = do
   _ <- run tlConn "INSERT INTO probs (question, answer) VALUES (?, ?)"
      [toSql question, toSql answer]
   return ()

-- Perform a query that is expected to return a single value of in a
-- single row.
query1 :: (Convertible SqlValue a, IConnection c) => c -> String -> [SqlValue] -> IO a
query1 conn query parms = do
   st <- prepare conn query
   _ <- execute st parms
   res <- fetchAllRows' st
   case res of
      [[v]] -> return $ fromSql v
      _ -> throwIO MalformedResult

close :: TimeLearn -> IO ()
close TimeLearn{..} = do
   disconnect tlConn

schema :: [String]
schema = [
   "CREATE TABLE probs (id INTEGER PRIMARY KEY, " ++
      "question TEXT UNIQUE, " ++
      "answer TEXT NOT NULL)",
   "CREATE TABLE learning (probid INTEGER PRIMARY KEY REFERENCES probs (id), " ++
      "next REAL NOT NULL, " ++
      "interval REAL NOT NULL)",
   "CREATE TABLE config (key TEXT PRIMARY KEY, value TEXT NOT NULL)",
   "CREATE INDEX learning_next ON learning (next)",
   "CREATE TABLE schema_version (version TEXT NOT NULL)",
   "CREATE TABLE log (stamp REAL NOT NULL, " ++
      "score INTEGER NOT NULL, " ++
      "probid INTEGER REFERENCES probs (id) NOT NULL)" ]

schemaVersion :: String
schemaVersion = "20170709A"
