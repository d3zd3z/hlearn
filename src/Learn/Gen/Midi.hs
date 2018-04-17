{-# LANGUAGE TemplateHaskell #-}

module Learn.Gen.Midi where

import Data.Aeson.TH
import Data.Char (toLower)

-- |A voicing exercise.
data Exercise = Voicing {
   vChords :: [[Int]] }
   -- ^Midi note values for each chord in the voicing.  Can also be
   -- individual notes for a scale.  Notes within a chord should be in
   -- ascending order.
   | Invalid
   deriving (Show, Eq)

deriveJSON defaultOptions {
   sumEncoding = TaggedObject {
      tagFieldName = "type",
      contentsFieldName = "contents"
   },
   fieldLabelModifier = map toLower . drop 1,
   constructorTagModifier = map toLower }
   ''Exercise
