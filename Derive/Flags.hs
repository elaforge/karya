-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | 'Flags' constants, analogous to "Derive.Attrs".
module Derive.Flags where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import Global


-- | Flags are like 'Derive.Environ.attributes', but are used for internal
-- communication between calls, while attributes are used for to configure
-- a note for the instrument.  Keeping them separate avoids confusing the
-- performer with various internal attributes that have nothing to do with
-- instrument.
type Flags = Set Flag
newtype Flag = Flag Text
    deriving (Eq, Ord, Show, DeepSeq.NFData)

instance Pretty.Pretty Flag where pretty (Flag t) = t

flag :: Text -> Flags
flag = Set.singleton . Flag

-- | Does the first argument contain the second argument?
has :: Flags -> Flags -> Bool
has = flip Set.isSubsetOf

-- | This note needs to wait until postproc to figure out its duration.  This
-- is used to implement final notes, where a zero duration note at the end
-- of a block can replace the first note of the next block.
infer_duration :: Flags
infer_duration = flag "infer-duration"

-- | This indicates that a note can be cancelled by a coincident note.  Among
-- other things, it supports 'infer_duration': a note with inferred duration
-- will replace any following note with 'weak'.
weak :: Flags
weak = flag "weak"

-- | Cancel coincident notes on the same track.  This is like forcing
-- concurrent events to have 'weak'.
strong :: Flags
strong = flag "strong"
