-- |Timed learning.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Learn.Learn where

import Control.Exception (Exception, throwIO)
import Data.Aeson (decode)
import Data.Char (chr)
import Data.Foldable (for_)
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Metrics (levenshtein)
import Learn.Gen.Midi
import Learn.TimeLearn
import Learn.TimeLearn.Stats
import Sound.Midi.Exercise
import System.IO (hFlush, stdout)

-- TODO: Move beyond something specific to the Midi setup.

-- |A given problem space has the following associated with it.  The
-- type 'ctx' is passed from the setup into the asker.
data ProblemAsker = forall ctx. ProblemAsker {
   withAsker :: forall a. (IO ctx -> IO a) -> IO a,
   -- ^Setup a context for this operation, passing it into the given
   -- function.
   askProblem :: IO ctx -> Problem -> IO AskResult
   -- ^Ask a problem, returning the result to the user.
   }

data LearnException =
   UnknownKind String
   deriving Show

instance Exception LearnException

getAsker :: TimeLearn -> IO ProblemAsker
getAsker tl = do
   case kind tl of
      "midi" -> return midiAsker
      k -> throwIO $ UnknownKind k

data AskResult =
   StopAsking
   | Answered Int
   deriving Show

learn :: TimeLearn -> IO ()
learn tl = do
   ProblemAsker{..} <- getAsker tl
   withAsker $ \askState -> do
      nexts <- getNexts tl 2
      case nexts of
         [] -> putStrLn "No more problems to learn"
         (prob:probs) -> do
            counts <- getStats tl
            let pretty = prettyStats counts (realToFrac $ pInterval prob)
            putStrLn $ "\n" ++ pretty
            putStrLn $ "   Q: " ++ (pQuestion prob)
            for_ probs $ \p2 -> do
               putStrLn $ "   n: " ++ (pQuestion p2)

            user <- askProblem askState prob
            putStrLn $ "You played: " ++ show user

-- Midi based problems
midiAsker :: ProblemAsker
midiAsker = ProblemAsker {
   withAsker = withMidi,
   askProblem = askMidi }

askMidi :: IO [(Int, Int)] -> Problem -> IO AskResult
askMidi getMidi prob = do
   putStr $ "\nPlay exercise: "
   hFlush stdout
   notes <- recordExercise getMidi
   putStrLn $ "\nNotes: " ++ show notes
   case notes of
      [[_]] -> return StopAsking
      _ -> checkMidi getMidi prob notes

checkMidi :: IO [(Int, Int)] -> Problem -> [[Int]] -> IO AskResult
checkMidi getMidi prob notes = do
   let correct = case fromJust $ decode (pAnswer prob) of
         Voicing{..} -> vChords
         _ -> error "Invalid answer in problem"
   let diffs = chordsCompare correct notes
   putStrLn $ "There were " ++ show diffs ++ " differences"
   if diffs == 0
      then return $ Answered 4
      else mustCorrect getMidi correct

mustCorrect :: IO [(Int, Int)] -> [[Int]] -> IO AskResult
mustCorrect getMidi correct = do
   putStr $ "\nPlease play correctly: "
   hFlush stdout
   notes <- recordExercise getMidi
   case notes of
      [[_]] -> return StopAsking
      _ -> do
         let diffs = chordsCompare correct notes
         putStrLn $ "There were " ++ show diffs ++ " differences"
         if diffs == 0
            then return $ Answered 4
            else mustCorrect getMidi correct

-- To compare the user's notes and the input notes, we want to compute
-- the Levenshtein distance.  The 'text-metrics' package has a nice
-- utility for this, but it only works on Unicode strings.  To make
-- this work, we want to convert Midi note values to (meaningless)
-- Unicode strings.  Each midi note is converted to a single Unicode
-- character (biased by 256 so that every character is valid), and the
-- chords are separated by commas.
--
-- In addition, we don't require the user to play in the same octave
-- as the exercise, so we return the minimum distance with the input
-- adjusted by up to four octaves.

chordsCompare :: [[Int]] -> [[Int]] -> Int
chordsCompare a b =
   foldl1 min $ map (\oct -> chordsCompareSingle a (map (map (+ oct)) b)) (manyOctaves 5)

chordsCompareSingle :: [[Int]] -> [[Int]] -> Int
chordsCompareSingle a b = levenshtein (chordsToText a) (chordsToText b)

manyOctaves :: Int -> [Int]
manyOctaves oct = map (* 12) $ 0 : interleave [1 .. oct] [-1, -2 .. -oct]

-- |Interleave to lists
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (transpose [xs, ys])

chordsToText :: [[Int]] -> Text
chordsToText = T.pack . intercalate "," .  map (map $ chr . (+ 256))
