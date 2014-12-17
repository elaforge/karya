-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | 'Flags' constants, analogous to "Derive.Attrs".
module Derive.Flags where
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Derive.ShowVal as ShowVal
import Global


-- | Flags are like 'Derive.Environ.attributes', but are used for internal
-- communication between calls, while attributes are used for to configure
-- a note for the instrument.  Keeping them separate avoids confusing the
-- performer with various internal attributes that have nothing to do with
-- instrument.
type Flags = Set.Set Flag
type Flag = Text.Text

instance ShowVal.ShowVal Flags where
    show_val flags = "{" <> Text.intercalate ", " (Set.toList flags) <> "}"

flag :: Text.Text -> Flags
flag = Set.singleton

-- | Does the first argument contain the second argument?
has :: Flags -> Flags -> Bool
has = flip Set.isSubsetOf

-- | This note needs to wait until postproc to figure out its duration.  This
-- is used to implement arrival notes, see "Derive.Call.Post.ArrivalNote" for
-- details.
infer_duration :: Flags
infer_duration = flag "infer-duration"

-- | This is set on notes that occur at TrackTime 0.  This is a hack to support
-- 'infer_duration': a note with inferred duration will replace any following
-- note with +track-time-0.
can_cancel :: Flags
can_cancel = flag "can-cancel"

-- | Cancel coincident notes on the same track.  This is like forcing the next
-- event to have 'can_cancel'.
cancel_next :: Flags
cancel_next = flag "cancel-next"
