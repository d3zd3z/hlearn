-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |A midi exercise.

{-# LANGUAGE RecordWildCards #-}


module Sound.Midi.Exercise (
   withMidi,
   recordExercise
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_, Exception, throwIO)
import Data.Bits
import Data.List (groupBy, sort)
import Data.Maybe (mapMaybe)
import Sound.PortMidi

-- Convert midi errors to an exception
data MidiError = MidiError PMError
   | NoInputDevice
   deriving Show

instance Exception MidiError

check :: IO PMError -> IO ()
check act = do
   err <- act
   case err of
      NoError -> return ()
      _ -> throwIO $ MidiError err

-- The PortMidi library uses Either backward, so note that.  This
-- considers any error return to be exceptional, see checkMaybe that
-- will return Nothing if a NoError error is returned).
checkGet :: IO (Either a PMError) -> IO a
checkGet act = do
   res <- act
   case res of
      Left a -> return a
      Right err -> throwIO $ MidiError err

checkMaybe :: IO (Either a PMError) -> IO (Maybe a)
checkMaybe act = do
   res <- act
   case res of
      Left a -> return $ Just a
      Right NoError -> return Nothing
      Right err -> throwIO $ MidiError err

-- |Invoke action within an initialized portmidi.
withPortMidi :: IO a -> IO a
withPortMidi action = bracket_ (check initialize) (check terminate) action

-- |Inside of withPortMidi, invoke action, passing in the PMStream of
-- the default input device.
withDefaultInput :: (PMStream -> IO b) -> IO b
withDefaultInput action = do
   tmpInput <- getDefaultInputDeviceID
   defInput <- maybe (throwIO NoInputDevice) return tmpInput
   bracket (checkGet $ openInput defInput) (\st -> check $ close st) action

-- |Run the input system.  Opens the default input system, and invokes
-- action, giving it a reader that will return midi events.  The event
-- poll waits a short delay (250ms) and returns any available events,
-- with an empty list being valid.
withMidi :: (IO [(Int, Int)] -> IO a) -> IO a
withMidi action = do
   withPortMidi $ do
      withDefaultInput $ \input -> do
         drain $ reader input 250
         action $ reader input 250000
   where
      reader input delay = do
         threadDelay delay
         evts <- checkMaybe $ readEvents input
         return $ mapMaybe getDown $ maybe [] id evts

-- |The useful action (inside withMidi), reads a single exercise
-- (delimited by a small pause without playing), returning a list of
-- the chords played.
recordExercise :: (IO [(Int, Int)]) -> IO [[Int]]
recordExercise getMidi = do
   evts <- readSeq getMidi 6
   return $ groupChords evts

-- Invoke the reader repeatedly until it returns an empty list.
drain :: IO [a] -> IO ()
drain reader = do
   items <- reader
   if null items then return ()
      else drain reader

-- |Read a sequence.  Invokes read until it turns a non-empty list,
-- and collects the results until read returns `limit` consecutive
-- empty lists.
readSeq :: IO [a] -> Int -> IO [a]
readSeq reader limit = initial
   where
      initial = do
         xs <- reader
         if null xs then initial
            else normal xs limit
      normal xs 0 = return xs
      normal xs count = do
         ys <- reader
         if null ys then normal xs (count-1)
            else normal (xs ++ ys) limit

-- Convert a Midi event into a note-down event (if that is what it
-- is).  Returns a possible pair of (timestamp, note).
getDown :: PMEvent -> Maybe (Int, Int)
getDown PMEvent{..} =
   let PMMsg{..} = decodeMsg message in
   if (status .&. 0xf0) == 0x90
      then Just (fromIntegral timestamp, fromIntegral data1)
      else Nothing

-- |Group all of the notes together that we can think of as a single
-- chord.  The 80ms is mostly chosen by trial and error, and seems to
-- work fairly well.
groupChords :: Ord a => [(Int, a)] -> [[a]]
groupChords =
   map (sort . map snd) .
      groupBy (\(a, _) (b, _) -> b - a < 80)
