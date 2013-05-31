-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This re-exports especially popular types, since they are frequently
-- mentioned in type signatures.  Unlike most of the other modules, it's meant
-- to be imported unqualified.
module Types (
    TrackNum, ScoreTime, TrackTime, RealTime
    , BlockId, ViewId, TrackId, RulerId
) where

import Ui.ScoreTime
import Ui.Types
-- Ok ok so it's not all from Ui any more, but RealTime is still used along
-- with ScoreTime just about everywhere in Derive.
import Perform.RealTime
