-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |A model for spaced repetition system learning.  Internal
-- implementation.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- This module is intended to be imported qualified.

module Learn.TimeLearn.Internal (
   TimeLearn(..),
   Problem(..),
   TimeLearnException(..),
   create,
   open,
   close,
   Populator,
   populate,
   addProblem,

   getNexts,
   getNew,
   update,

   -- Internal use
   getAll,
   query1
) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString.Lazy as L
import Data.Convertible (Convertible)
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import System.Random (randomR, newStdGen)

data TimeLearn = TimeLearn {
   tlConn :: ConnWrapper,
   kind :: String }

-- A single problem.
data Problem = Problem {
   pId :: Int,
   pQuestion :: String,
   pAnswer :: L.ByteString,
   pNext :: POSIXTime,
   pInterval :: NominalDiffTime }
   deriving Show

-- When populating the database, the `Populator` restricts the update
-- operation to something within a single transaction.
newtype Populator = Populator TimeLearn

data TimeLearnException
   = MalformedResult
   | InvalidSchemaVersion String String
   | InvalidFactor Int
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

-- |Query for 'n' upcoming problems that have expired.  This will
-- return list of problems, with element 0 being the next problem that
-- should be asked.
getNexts :: TimeLearn -> Int -> IO [Problem]
getNexts tl@TimeLearn{..} count = do
   now <- getPOSIXTime
   st <- prepare tlConn $ "SELECT id, question, answer, next, interval " ++
      "FROM probs JOIN learning " ++
      "WHERE probs.id = learning.probid " ++
      "  AND next <= ? " ++
      "ORDER BY next " ++
      "LIMIT ?"
   _ <- execute st [toSql (realToFrac now :: Double), toSql count]
   rows <- fetchAllRows' st
   case map toProblem rows of
      [] -> (maybe [] (:[])) <$> getNew tl
      nexts -> return nexts
   where
      toProblem [pId, qn, ans, pNext, interval] = Problem {
         pId = fromSql pId,
         pQuestion = fromSql qn,
         pAnswer = fromSql ans,
         pNext = fromRational $ fromSql pNext,
         pInterval = fromRational $ fromSql interval }
      toProblem _ = error "Unexpected SQL result"

-- |Get a problem that hasn't started being learned.  The interval and
-- next will be set appropriately for a new problem.
-- Will also append " (NEW)" to the question text.
getNew :: TimeLearn -> IO (Maybe Problem)
getNew TimeLearn{..} = do
   now <- getPOSIXTime
   st <- prepare tlConn $ "SELECT id, question, answer " ++
      "FROM probs " ++
      "WHERE ID NOT IN (SELECT probid FROM learning) " ++
      "ORDER BY id " ++
      "LIMIT 1"
   _ <- execute st []
   rows <- fetchAllRows' st
   return $ listToMaybe $ map (toProblem now) rows
   where
      toProblem now [pId, pQuestion, pAnswer] = Problem {
         pId = fromSql pId,
         pQuestion = fromSql pQuestion ++ " (NEW)",
         pAnswer = fromSql pAnswer,
         pNext = now,
         pInterval = 5.0 }
      toProblem _ _ = error "Unexpected SQL result"

-- |Retrieve all problems from the database.
getAll :: TimeLearn -> IO [(Int, String, L.ByteString)]
getAll TimeLearn{..} = do
   st <- prepare tlConn $ "SELECT id, question, answer " ++
      "FROM probs " ++
      "ORDER BY id"
   _ <- execute st []
   rows <- fetchAllRows' st
   return $ map toProblem rows
   where
      toProblem [pId, qn, ans] = (fromSql pId, fromSql qn, fromSql ans)
      toProblem _ = error "Unexpected SQL result"

-- |Update a problem, based on a learning factor.  The scale is 1..4,
-- with 1 being totally incorrect, and 4 being totally correct.
update :: TimeLearn -> Problem -> Int -> IO ()
update TimeLearn{..} prob factor = do
   adjust <- case factor of
      1 -> return 0.25
      2 -> return 0.9
      3 -> return 1.2
      4 -> return 2.2
      _ -> throwIO $ InvalidFactor factor
   now <- getPOSIXTime
   rng <- newStdGen
   let fudge = realToFrac $ fst $ randomR (0.75 :: Double, 1.25) rng
   let interval = ((pInterval prob) * adjust * fudge) `max` 5.0
   let next = now + interval

   _ <- run tlConn "INSERT OR REPLACE INTO learning VALUES (?, ?, ?)"
      [toSql $ pId prob, toSql (realToFrac next :: Double),
         toSql (realToFrac interval :: Double)]
   _ <- run tlConn "INSERT INTO log VALUES (?, ?, ?)"
      [toSql (realToFrac now :: Double),
         toSql $ pId prob,
         toSql factor]
   commit tlConn

-- Run the action given the populator, which can be used to add items.
populate :: TimeLearn -> (Populator -> IO ()) -> IO ()
populate tl@TimeLearn{..} act = do
   withTransaction tlConn (\_ -> act $ Populator tl)

addProblem :: Populator -> String -> L.ByteString -> IO ()
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
