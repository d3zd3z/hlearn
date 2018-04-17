-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |Sanity checking the database

module Learn.TimeLearn.SanityCheck (
   sanityCheck
) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as L
import Learn.Gen.Scales
import Learn.Gen.Midi
import qualified Learn.TimeLearn.Internal as TL

-- |Perform a sanity check on the existing database.  We want to make
-- sure it is safe to add new lessons without messing up the ones that
-- are there.  This will query the given database, retrieving all of
-- the problems, and compare these problems with the new set.
-- Everything should match, with the only difference being that the
-- new problem set is longer.
sanityCheck :: TL.TimeLearn -> IO (Either String ())
sanityCheck tl = do
   problems <- TL.getAll tl
   putStrLn $ "There are " ++ (show $ length problems) ++ " problems"
   let scales' = map fixTuple $ zip [1..] scales
   compProbs scales' problems
   return $ Right ()

-- Compare the expected (left) with the actual in DB value (right).
-- It is Ok if there are more expected than exist, but any other
-- mismatches result in failure.
compProbs :: [(Int, String, L.ByteString)] -> [(Int, String, L.ByteString)] -> IO ()
compProbs [] [] = return ()
compProbs _ [] = putStrLn $ "More problems to be added, all OK"
compProbs [] _ = putStrLn $ "Extra problems in Database, stopping"
compProbs ((a, aqn, aans):ar) ((b, bqn, bans):br)
   | a /= b = putStrLn $ "ID mismatch " ++ show a ++ " and " ++ show b
   | aqn /= bqn =
      putStrLn $ show a ++ ": question mismatch Exp:" ++ show aqn ++ " db:" ++ show bqn
   | not $ sameJson aans bans =
      putStrLn $ show a ++ ": answer mismatch (" ++ aqn ++ ")\n" ++ jshow aans ++ " and\n" ++ jshow bans
   | otherwise = compProbs ar br

sameJson :: L.ByteString -> L.ByteString -> Bool
sameJson a b = case (decode a :: Maybe Exercise, decode b) of
   (Nothing, _) -> error "Invalid json"
   (_, Nothing) -> error "Invalid json"
   (Just aval, Just bval) -> aval == bval

jshow :: L.ByteString -> String
jshow item = show $ (decode item :: Maybe Exercise)

-- When zipping problems with integers, fixup the tuples.
fixTuple :: (a, (b, c)) -> (a, b, c)
fixTuple (a, (b, c)) = (a, b, c)
