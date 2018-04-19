-- |Learning statistics

{-# LANGUAGE RecordWildCards #-}

module Learn.TimeLearn.Stats (
   Stats(..),
   getStats,
   prettyStats
) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.HDBC
import Learn.TimeLearn.Internal
import Text.Printf (printf)

-- |The stats about the learn database.
data Stats = Stats {
   sActive :: Int,
   -- ^The number of "active" problems.  Something is considered
   -- active if it is due for learning.

   sLater :: Int,
   -- ^The number of problems that are being learned, but aren't ready
   -- to be asked again.

   sUnlearned :: Int,
   -- ^The number of problems the use has never been shown

   sBuckets :: [(String, Int)]
   -- ^Counts all of the problems, grouped into histogram buckets
   -- based on the learning interval of the problem.  Each pair is a
   -- name of a bucket, and a count of items in that bucket.
   } deriving Show

-- |Collect some statistics from the learning database.
getStats :: TimeLearn -> IO Stats
getStats TimeLearn{..} = do
   now <- realToFrac <$> getPOSIXTime :: IO Double
   unlearned <- query1 tlConn
      ("SELECT COUNT(*) " ++
      "FROM probs " ++
      "WHERE id NOT IN (SELECT probid FROM learning)") [] :: IO Int
   active <- query1 tlConn
      ("SELECT COUNT (*) " ++
      "FROM probs JOIN learning " ++
      "WHERE probs.id = learning.probid " ++
      "  AND next <= ?")
      [toSql now] :: IO Int
   later <- query1 tlConn
      ("SELECT COUNT (*) " ++
      "FROM probs JOIN learning " ++
      "WHERE probs.id = learning.probid " ++
      "  AND next > ?")
      [toSql now] :: IO Int
   buckets <- bucketQuery tlConn countBuckets 1.0 0.0
   return $ Stats {
      sActive = active,
      sLater = later,
      sUnlearned = unlearned,
      sBuckets = buckets }

-- |Provide a pretty formatted textual representation of the statics.
-- The interval passed in is included in the stats.
prettyStats :: Stats -> Double -> String
prettyStats Stats{..} interval =
   "Active: " ++ show sActive ++
      ", Later: " ++ show sLater ++
      ", Unlearned: " ++ show sUnlearned ++
      ", Interval: " ++ humanize interval ++
      "\n" ++ concatMap (prettyBucket total) sBuckets
   where
      total = sum $ map snd sBuckets

prettyBucket :: Int -> (String, Int) -> String
prettyBucket total (name, count) =
   printf "  %-4s: %6d %s\n" name count (stars count total)

stars :: Int -> Int -> String
stars this total =
   let starCount = if total > 0 then (65 * this) `div` total else 0 in
   replicate starCount '*'

-- For statistics we match up short bucket names with time intervals.
countBuckets :: [(String, Double)]
countBuckets = [
   ("sec", 60.0),
   ("min", 60.0),
   ("hr", 24.0),
   ("day", 30.0),
   ("mon", 1.0e30) ]

-- Query for a the buckets.
bucketQuery :: IConnection c => c -> [(String, Double)] -> Double -> Double -> IO [(String, Int)]
bucketQuery _ [] _ _ = return []
bucketQuery conn ((name, limit):xs) interval prior = do
   let interval' = interval * limit
   count <- query1 conn (
      "SELECT COUNT (*) " ++
      "FROM probs JOIN learning " ++
      "WHERE probs.id = learning.probid " ++
      "  AND interval <= ? and interval > ?")
      [toSql interval', toSql prior]
   more <- bucketQuery conn xs interval' interval
   return $ (name, count) : more

timeUnits :: [(String, Double)]
timeUnits = [
   ("seconds", 60.0),
   ("minutes", 60.0),
   ("hours", 24.0),
   ("days", 30.0),
   ("months", 12.0),
   ("years", 1.0e9) ]

-- |Produce a friendly human readable representation of a time in
-- seconds.
humanize :: Double -> String
humanize = rep 1.0 timeUnits
   where
      rep soFar ((name, duration):xs) time
         | time < soFar * duration = printf "%.1f %s" time name
         | otherwise               = rep (soFar * duration) xs (time / duration)
      rep _ [] _ = "INF"
