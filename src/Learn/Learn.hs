-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |Timed learning.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Learn.Learn (
   learn
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Reader
import Data.Aeson (decode)
import Data.Char (chr)
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

-- |Give the result of asking a question, as either an Int 1-4 (4
-- being perfect), or Nothing to abort the process.
type AskResult = Maybe Int

learn :: TimeLearn -> IO ()
learn tl = do
   ProblemAsker{..} <- getAsker tl
   withAsker $ \askState -> do
      let loop = do
            nexts <- getNexts tl 2
            case nexts of
               [] -> putStrLn "No more problems to learn"
               allProbs@(prob:_) -> do
                  prompt <- genPrompt tl allProbs
                  putStrLn prompt

                  user <- askProblem askState prob
                  case user of
                     Nothing -> return ()
                     Just x -> do
                        update tl prob x
                        loop
      loop

-- Given a problem, possible more problems, and the current state,
-- generate a textual message to prompt for the question.
genPrompt :: TimeLearn -> [Problem] -> IO String
genPrompt tl (prob:probs) = do
   counts <- getStats tl
   let pretty = prettyStats counts (realToFrac $ pInterval prob)
   return $ "\n" ++ pretty ++
      "\n   Q: " ++ (pQuestion prob) ++ "\n" ++
      concatMap (\p2 -> "   n: " ++ (pQuestion p2) ++ "\n") probs
genPrompt _ [] = undefined

-- Midi based problems
midiAsker :: ProblemAsker
midiAsker = ProblemAsker {
   withAsker = withMidi,
   askProblem = askMidi }

-- State used to ask a single question
data AskState = AskState {
   sGetMidi :: IO [(Int, Int)],
   sProblem :: Problem,
   sCorrect :: [[Int]] -> Int }

runAsker :: IO [(Int, Int)] -> Problem -> AskIO a -> IO a
runAsker getMidi prob asker =
   let correct = case fromJust $ decode (pAnswer prob) of
         Voicing{..} -> vChords
         _ -> error "Invalid answer in problem" in
   runReaderT asker AskState {
      sGetMidi = getMidi,
      sProblem = prob,
      sCorrect = chordsCompare correct }

type AskIO a = ReaderT AskState IO a

type Getter = IO [(Int, Int)]

askMidi :: Getter -> Problem -> IO AskResult
askMidi getMidi prob =
   runAsker getMidi prob drill

drill :: AskIO AskResult
drill = do
   res <- getUser "Play exercise: "
   case res of
      Just x
         | x == 4 -> return res
         | otherwise -> require res
      Nothing -> return Nothing

-- Ask the problem repeatedly, until the user plays it correctly.
require :: AskResult -> AskIO AskResult
require res = do
   r2 <- getUser "Play correctly: "
   case r2 of
      Nothing -> return Nothing
      Just x
         | x == 4 -> return res
         | otherwise -> require res

-- Emit a given prompt, record a session, and return its correctness.
getUser :: String -> AskIO AskResult
getUser prompt = do
   lift $ putStr prompt
   lift $ hFlush stdout
   getMidi <- asks sGetMidi
   notes <- lift $ recordExercise getMidi
   case notes of
      [[_]] -> do
         lift $ putStrLn "Exiting"
         return Nothing
      _ -> do
         correct <- asks sCorrect
         let diffs = correct notes
         lift $ putStrLn $ "There were " ++ show diffs ++ " differences"
         if diffs == 0
            then return $ Just 4
            else return $ Just 1

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

-- |Compare two chord sequences, returning the "distance" between
-- them, with 0 being a perfect match.  The match is tried across
-- multiple octaves and the match with the shortest distance is used.
chordsCompare :: [[Int]] -> [[Int]] -> Int
chordsCompare a b =
   foldl1 min $ map (\oct -> chordsCompareSingle a (map (map (+ oct)) b)) (manyOctaves 5)

-- |Compare two chord sequences within a single octave.
chordsCompareSingle :: [[Int]] -> [[Int]] -> Int
chordsCompareSingle a b = levenshtein (chordsToText a) (chordsToText b)

-- |Generate a list of octave maps by octave: [0, 12, -12, 24, -24,
-- ...] for the given number of octaves.
manyOctaves :: Int -> [Int]
manyOctaves oct = map (* 12) $ 0 : interleave [1 .. oct] [-1, -2 .. -oct]

-- |Interleave two lists
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (transpose [xs, ys])

-- |Convert a chord list (nested list of midi note values) into a
-- unicode sequence so that we can use a textual comparison function.
chordsToText :: [[Int]] -> Text
chordsToText = T.pack . intercalate "," .  map (map $ chr . (+ 256))
