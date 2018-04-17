{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Learn.Gen.Scales (
   scales
) where

import qualified Data.Aeson as A
import Data.Array.Unboxed
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust)
import Control.Monad (liftM2)
import Learn.Gen.Midi (Exercise(..))

scales :: [(String, L.ByteString)]
scales = concatMap genScales practice

-- Generate a deterministic list of scales for practice.  For this to
-- work meaningfully, this should always be created with the same
-- primary key.

practice :: [Practice]
practice = [
   Practice {
      name = "major",
      intervals = "WWHWWWH",
      hands = bothHandsProgressive,
      style = styleUpDown },
   Practice {
      name = "minor (dorian)",
      intervals = "WHWWWHW",
      hands = bothHands,
      style = styleUpDown },
   Practice {
      name = "dominant (mixolydian)",
      intervals = "WWHWWHW",
      hands = bothHands,
      style = styleUpDown },
   Practice {
      name = "half diminished (locrian)",
      intervals = "HWWHWWW",
      hands = bothHands,
      style = styleUpDown },
   Practice {
      name = "diminished (whole-half)",
      intervals = "WHWHWHWH",
      hands = bothHandsProgressive,
      style = styleUpDown },
   Practice {
      name = "sym-dom (half-whole)",
      intervals = "HWHWHWHW",
      hands = bothHands,
      style = styleUpDown },
   Practice {
      name = "major 3rds",
      intervals = "WWHWWWH",
      hands = bothHandsProgressive,
      style = style3Up },
   Practice {
      name = "major 3rds rev",
      intervals = "WWHWWWH",
      hands = bothHandsProgressive,
      style = style3Upr }
      ]

-- Convert a given Practice into a series of question and answer pairs
genScales :: Practice -> [(String, L.ByteString)]
genScales Practice{..} = do
   hand <- hands
   key <- bases
   let getNote = genIntervals (fromJust $ parseNote key) intervals
   let scale = mkScale (length intervals) (octaves style) (extra style)
   let notes = map ((:[]) . getNote) (liftM2 (+) scale (pattern style))
   let notes' = notes ++ [[getNote 0]]
   let qn = textHand hand ++ "-scale " ++ key ++ " " ++ name
   return $ (qn, encodeChords $ adjustHands hand notes')

-- Adjust the notes according to the hands in the exercise.
adjustHands :: Hands -> [[Int]] -> [[Int]]
adjustHands H2 notes = map (double) notes
   where
      double [a] = [a, a + 12]
      double _ = error "Invalid number of notes in scale"
adjustHands _ notes = notes

bases :: [String]
bases = [ "C", "G", "D", "A", "E", "B", "F♯", "G♭", "D♭", "A♭", "E♭", "B♭", "F" ]

-- TODO: Convert numeric note back to name (and octave)

bothHandsProgressive :: [Hands]
bothHandsProgressive = [RH, LH, H2]

bothHands :: [Hands]
bothHands = [H2]

-- A style
data Style = Style {
   name :: String,
   pattern :: [Int],
   octaves :: Int,
   extra :: Int }
   deriving Show

-- The styles available
styleUpDown :: Style
style3Up :: Style
style3Upr :: Style

styleUpDown = Style {
   name = "updown",
   pattern = [0],
   octaves = 2,
   extra = 0 }

style3Up = Style {
   name = "3up",
   pattern = [0, 2],
   octaves = 1,
   extra = 2 }

style3Upr = Style {
   name = "3up",
   pattern = [2, 0],
   octaves = 1,
   extra = 2 }

-- |The data associated with a practice.  This gets build into the
-- description.
data Practice = Practice {
   name :: String, -- major, minor, etc
   intervals :: String,  -- The intervals
   hands :: [Hands], -- Which hand(s) to use
   style :: Style }
   deriving Show

-- Encode chord notes as an exercise.
encodeChords :: [[Int]] -> L.ByteString
encodeChords ch = A.encode $ Voicing { vChords = ch }

-- Generate a practice pattern for 'n' notes in a scale, climbing to
-- the top, and back down, using 'oct' octaves.
mkScale :: Int -> Int -> Int -> [Int]
mkScale notes oct extra =
   [0 .. notes * oct - 1] ++ [notes * oct, notes * oct - 1 .. 1-extra]

-- Given an interval string, generate the given number of octaves of
-- notes with index '0' being at base.
genIntervals :: Int -> String -> (Int -> Int)
genIntervals base text = lookupFunc
   where
      lookupFunc elt =
         let (idx, base') = rangeFix elt 0 in
         (ary ! idx) + base'

      ary :: UArray Int Int
      ary = if base + 12 == ending then listArray (0, high - 1) $ reverse notes
         else error $ "Interval not an octave: " ++ show text

      -- Move the range up or down so that it is within the domain of
      -- the build array.  Keep with that a base that can be added to
      -- the particular note to get the value within the right octave.
      rangeFix :: Int -> Int -> (Int, Int)
      rangeFix n scaleBase
         | n < 0     = rangeFix (n + high) (scaleBase - 12)
         | n >= high = rangeFix (n - high) (scaleBase + 12)
         | otherwise = (n, scaleBase)

      high = length notes
      (ending, notes) = foldl sumNotes (base, []) text
      sumNotes (n, elts) ch =
         let note = intervalName ch in
         (n + note, n:elts)

-- Map from the human readable intervals to the number of notes
-- represented by that interval.
intervalName :: Char -> Int
intervalName 'H' = 1
intervalName 'W' = 2
intervalName 'm' = 3
intervalName 'M' = 4
intervalName '5' = 5
intervalName ch = error $ "Unsupported interval: " ++ show ch

data Hands = RH | LH | H2
   deriving Show

textHand :: Hands -> String
textHand RH = "RH"
textHand LH = "LH"
textHand H2 = "2H"

-- |Attempt to decode a note as a Midi note.  There isn't a worry
-- about the particular octave, just a pitch value within range.
-- the allowed format is a letter optionally followed by an
-- accidental.
parseNote :: String -> Maybe Int
parseNote (x:xs) = do
   nv <- noteValue x
   av <- parseAccidental xs
   return $ nv + av
parseNote _ = Nothing

parseAccidental :: String -> Maybe Int
parseAccidental "" = Just 0
parseAccidental [x] = accidentalValue x
parseAccidental _ = Nothing

noteValue :: Char -> Maybe Int
noteValue 'C' = Just 60
noteValue 'D' = Just 62
noteValue 'E' = Just 64
noteValue 'F' = Just 65
noteValue 'G' = Just 67
noteValue 'A' = Just 69
noteValue 'B' = Just 71
noteValue _ = Nothing

accidentalValue :: Char -> Maybe Int
accidentalValue '#' = Just 1
accidentalValue '♯' = Just 1
accidentalValue 'b' = Just $ -1
accidentalValue '♭' = Just $ -1
accidentalValue _ = Nothing
