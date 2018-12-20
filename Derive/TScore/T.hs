-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Shared types for TScore.
module Derive.TScore.T where
import qualified Ui.Id as Id

import           Global


-- * Score

newtype Score = Score [Toplevel] deriving (Eq, Show)

data Toplevel = ToplevelDirective !Directive | BlockDefinition !Block
    deriving (Eq, Show)

data Block = Block {
    block_id :: !Id.BlockId
    , block_directives :: ![Directive]
    , block_title :: !Text
    , block_tracks :: !Tracks
    } deriving (Eq, Show)

newtype Tracks = Tracks [Track]
    deriving (Eq, Show)

data Track = Track {
    track_title :: !Text
    , track_tokens :: ![Token]
    } deriving (Eq, Show)

data Directive = Directive !Text !(Maybe Text)
    deriving (Eq, Show)

data Token =
    -- | Higher count for larger divisions, e.g. anga vs. avartanam.
    TBarline !Barline
    | TNote !Note
    | TRest !Rest
    deriving (Eq, Show)

type Rank = Int

newtype Barline = Barline Rank
    deriving (Eq, Show)

data Note = Note {
    note_call :: !Call
    , note_pitch :: !Pitch
    , note_duration :: !Duration
    } deriving (Eq, Show)

newtype Call = Call Text
    deriving (Eq, Show)

newtype Rest = Rest Duration
    deriving (Eq, Show)

data Pitch = Pitch {
    pitch_octave :: !Octave
    , pitch_call :: !Text
    } deriving (Eq, Show)

data Octave = Absolute !Int | Relative !Int
    deriving (Eq, Show)

data Duration = Duration {
    dur_duration :: !(Maybe Int)
    , dur_dots :: !Int
    , dur_tie :: !Bool
    } deriving (Eq, Show)
