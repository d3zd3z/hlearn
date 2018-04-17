-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |A model for spaced repetition system learning

module Main where

import Control.Exception (bracket)
import Data.Foldable (for_)
import qualified Learn.TimeLearn as TL
import Learn.TimeLearn.SanityCheck
import System.Environment (getArgs)

main :: IO ()
main = do
   args <- getArgs
   doCommand args

doCommand :: [String] -> IO ()
doCommand ["check", dbFile] = doCheck dbFile
doCommand _ = putStrLn $ "Unknown usage"

doCheck :: String -> IO ()
doCheck dbFile = do
   bracket (TL.open dbFile) TL.close $ \tl -> do
      res <- sanityCheck tl
      either (\msg -> putStrLn $ "Error in database: " ++ msg)
         (\() -> return ())
         res

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
            ("Answer: " ++ show num)
   nx <- TL.getNexts t2 2
   putStrLn $ "Nexts: " ++ show nx
   case nx of
      (p : _) -> TL.update t2 p 4
      _ -> return ()
   TL.close t2
