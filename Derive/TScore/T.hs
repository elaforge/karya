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
    , track_tokens :: ![Token Pitch Duration]
    } deriving (Eq, Show)

data Directive = Directive !Text !(Maybe Text)
    deriving (Eq, Show)

data Token pitch dur =
    -- | Higher count for larger divisions, e.g. anga vs. avartanam.
    TBarline !Barline
    | TNote !(Note pitch dur)
    | TRest !(Rest dur)
    deriving (Eq, Show)

token_name :: Token pitch dur -> Text
token_name = \case
    TBarline {} -> "barline"
    TNote {} -> "note"
    TRest {} -> "rest"

map_pitch :: Monad m => (pitch1 -> m pitch2) -> Token pitch1 dur
    -> m (Token pitch2 dur)
map_pitch f = \case
    TNote note -> do
        pitch <- f (note_pitch note)
        return $ TNote $ note { note_pitch = pitch }
    TBarline a -> return $ TBarline a
    TRest a -> return $ TRest a

map_duration :: Monad m => (dur1 -> m dur2) -> Token pitch dur1
    -> m (Token pitch dur2)
map_duration f = \case
    TNote note -> do
        dur <- f (note_duration note)
        return $ TNote $ note { note_duration = dur }
    TBarline a -> return $ TBarline a
    TRest (Rest dur) -> TRest . Rest <$> f dur

map_note :: Monad m => (Note pitch1 dur -> m (Note pitch2 dur))
    -> Token pitch1 dur -> m (Token pitch2 dur)
map_note f = \case
    TNote note -> TNote <$> f note
    TBarline a -> return $ TBarline a
    TRest a -> return $ TRest a

-- | Opposide from Ruler.Rank, higher numbers mean larger divisions.
type Rank = Int

newtype Barline = Barline Rank
    deriving (Eq, Show)

data Note pitch dur = Note {
    note_call :: !Call
    , note_pitch :: !pitch
    , note_duration :: !dur
    } deriving (Eq, Show)

newtype Call = Call Text
    deriving (Eq, Show)

newtype Rest dur = Rest dur
    deriving (Eq, Show)

data Pitch = Pitch {
    pitch_octave :: !Octave
    , pitch_call :: !Text
    } deriving (Eq, Show)

data Octave = Absolute !Int | Relative !Int
    deriving (Eq, Show)

data Duration = Duration {
    dur_int :: !(Maybe Int)
    , dur_dots :: !Int
    , dur_tie :: !Bool
    } deriving (Eq, Show)
