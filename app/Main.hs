-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |A model for spaced repetition system learning

module Main where

import Data.Foldable (for_)
import qualified Learn.TimeLearn as TL

main :: IO ()
main = do
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
