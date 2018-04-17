-- Copyright (C) 2018 David Brown
--
-- SPDX-License-Identifier: Apache-2.0
--
-- |Sanity checking the database

module Learn.TimeLearn.SanityCheck (
   sanityCheck
) where

import Learn.TimeLearn.Internal

-- |Perform a sanity check on the existing database.  We want to make
-- sure it is safe to add new lessons without messing up the ones that
-- are there.  This will query the given database, retrieving all of
-- the problems, and compare these problems with the new set.
-- Everything should match, with the only difference being that the
-- new problem set is longer.
sanityCheck :: TimeLearn -> Either String ()
sanityCheck = undefined
