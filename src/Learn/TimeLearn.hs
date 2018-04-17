-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |A model for spaced repetition system learning

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- This module is intended to be imported qualified.

module Learn.TimeLearn (
   TimeLearn, kind,
   Problem(..),
   TimeLearnException(..),
   create,
   open,
   close,
   Populator,
   populate,
   addProblem,

   getNexts,
   getNew,
   update,

   sanityCheck
) where

import Learn.TimeLearn.Internal
import Learn.TimeLearn.SanityCheck
