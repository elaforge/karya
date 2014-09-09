-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | 'Flags' constants, analogous to "Derive.Attrs".
module Derive.Flags where
import qualified Data.Set as Set
import Data.Text (Text)


-- | Flags are like 'Derive.Environ.attributes', but are used for internal
-- communication between calls, while attributes are used for to configure
-- a note for the instrument.  Keeping them separate avoids confusing the
-- performer with various internal attributes that have nothing to do with
-- instrument.
type Flags = Set.Set Flag
type Flag = Text

flag :: Text -> Flags
flag = Set.singleton

-- | This note needs to wait until postproc to figure out its duration.  This
-- is used to implement arrival notes, see "Derive.Call.Post.ArrivalNote" for
-- details.
infer_duration :: Flags
infer_duration = flag "infer-duration"

-- | This is set on notes that occur at TrackTime 0.  This is a hack to support
-- 'infer_duration': a note with inferred duration will replace any following
-- note with +track-time-0.
track_time_0 :: Flags
track_time_0 = flag "track-time-0"
