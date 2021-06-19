-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Split some types out of Audio, to reduce dependencies.
module Util.Audio.AudioT where
import qualified Util.Serialize as Serialize

import           Global


-- | Should be >=0.
newtype Frames = Frames Int
    deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Serialize.Serialize)

instance Pretty Frames where
    pretty (Frames n) = pretty n <> "f"

type Rate = Int
type Seconds = Double

framesToSeconds :: Rate -> Frames -> Seconds
framesToSeconds rate (Frames frames) = fromIntegral frames / fromIntegral rate
