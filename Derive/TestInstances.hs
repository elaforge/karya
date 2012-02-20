{-# LANGUAGE  CPP, StandaloneDeriving  #-}
-- | Instances that are not safe but very useful for testing.
module Derive.TestInstances where

-- I should be able to just trust this to not be linked in, but it turns out
-- the main app actually does import testing modules for some util functions.
-- Eventually that should stop, but meanwhile...
#ifdef TESTING

import qualified Derive.TrackLang as TrackLang

-- Normally Vals aren't comparable for equality because of the pesky VPitch,
-- but it's too convenient for testing to lose.
deriving instance Eq TrackLang.Val
deriving instance Eq TrackLang.Call
deriving instance Eq TrackLang.Term

#endif
