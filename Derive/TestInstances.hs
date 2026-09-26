-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP, StandaloneDeriving #-}
-- | Instances that are not safe but very useful for testing.
module Derive.TestInstances where

#ifndef TESTING
#error "don't import testing modules from non-test code"
#endif

#ifdef TESTING

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr -- needed for standalone deriving
import qualified Derive.PSignal as PSignal

-- Normally Vals aren't comparable for equality because of the pesky VPSignal,
-- but it's too convenient for testing to lose.
deriving instance {-# OVERLAPPING #-} Eq DeriveT.Val
deriving instance {-# OVERLAPPING #-} Eq DeriveT.Call
deriving instance {-# OVERLAPPING #-} Eq DeriveT.Term
deriving instance Eq DeriveT.Quoted

instance Eq PSignal.PSignal where
    sig1 == sig2 = PSignal.toPairs sig1 == PSignal.toPairs sig2
instance Eq (PSignal.RawPitch a) where
    p1 == p2 = PSignal.pitchNn (PSignal.coerce p1)
        == PSignal.pitchNn (PSignal.coerce p2)

instance Eq DeriveT.CFunction where
    a == b = DeriveT.cfName a == DeriveT.cfName b
instance Eq DeriveT.PFunction where
    a == b = DeriveT.pfName a == DeriveT.pfName b

#endif
