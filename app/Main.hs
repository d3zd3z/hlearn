-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |A model for spaced repetition system learning

module Main where

import Control.Exception (bracket)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Foldable (for_)
import qualified Learn.TimeLearn as TL
import Learn.TimeLearn.SanityCheck
import Learn.TimeLearn.Stats
import System.Environment (getArgs)

main :: IO ()
main = do
   args <- getArgs
   doCommand args

doCommand :: [String] -> IO ()
doCommand ["check", dbFile] = doCheck dbFile
doCommand ["update", dbFile] = doUpdate dbFile
doCommand ["stats", dbFile] = doStats dbFile
doCommand _ = putStrLn $ "Unknown usage"

doCheck :: String -> IO ()
doCheck dbFile = do
   bracket (TL.open dbFile) TL.close $ \tl -> do
      res <- sanityCheck tl
      either (\msg -> putStrLn $ "Error in database: " ++ msg)
         (\probs -> putStrLn $ (show $ length probs) ++ " problems to add")
         res

doUpdate :: String -> IO ()
doUpdate dbFile = do
   bracket (TL.open dbFile) TL.close $ \tl -> do
      res <- sanityCheck tl
      either (\msg -> putStrLn $ "Error in database: " ++ msg)
         (\probs -> do
            TL.populate tl $ \pop -> do
               for_ probs $ \(question, answer) -> do
                  TL.addProblem pop question answer)
         res

doStats :: String -> IO ()
doStats dbFile = do
   bracket (TL.open dbFile) TL.close $ \tl -> do
      counts <- getStats tl
      putStrLn $ prettyStats counts 65.2

bmain :: IO ()
bmain = do
   tl <- TL.create "sample.db" "raw"
   TL.close tl
   putStrLn $ "Created"
   t2 <- TL.open "sample.db"
   putStrLn $ "Kind: " ++ TL.kind t2
   TL.populate t2 $ \pop -> do
      for_ [1 :: Int .. 100] $ \num -> do
         TL.addProblem pop ("Question: " ++ show num)
            (LC.pack $ "Answer: " ++ show num)
   nx <- TL.getNexts t2 2
   putStrLn $ "Nexts: " ++ show nx
   case nx of
      (p : _) -> TL.update t2 p 4
      _ -> return ()
   TL.close t2
