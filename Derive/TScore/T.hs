-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- deriving (Real) for Time emits this warning.
{-# OPTIONS_GHC -fno-warn-identities #-}
-- | Shared types for TScore.
module Derive.TScore.T where
import qualified Data.Ratio as Ratio
import qualified Data.String as String

import qualified Ui.Id as Id

import           Global


-- | This is the default "beat".
newtype Time = Time Ratio.Rational
    deriving (Ord, Eq, Num, Enum, Real, Fractional, RealFrac, Pretty)

instance Show Time where
    show (Time t) = prettys t

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
    , track_tokens :: ![Token Pitch NDuration Duration]
    } deriving (Eq, Show)

data Directive = Directive !Text !(Maybe Text)
    deriving (Eq, Show)

data Token pitch ndur rdur =
    -- | Higher count for larger divisions, e.g. anga vs. avartanam.
    TBarline !Barline
    | TNote !(Note pitch ndur)
    | TRest !(Rest rdur)
    deriving (Eq, Show)

token_name :: Token pitch ndur rdur -> Text
token_name = \case
    TBarline {} -> "barline"
    TNote {} -> "note"
    TRest {} -> "rest"

map_pitch :: Applicative m => (pitch1 -> m pitch2) -> Token pitch1 ndur rdur
    -> m (Token pitch2 ndur rdur)
map_pitch f = \case
    TNote note -> TNote . (\a -> note { note_pitch = a}) <$> f (note_pitch note)
    TBarline a -> pure $ TBarline a
    TRest a -> pure $ TRest a

map_note_duration :: Applicative m => (dur1 -> m dur2) -> Token pitch dur1 rdur
    -> m (Token pitch dur2 rdur)
map_note_duration f = \case
    TNote note -> TNote . (\a -> note { note_duration = a }) <$>
        f (note_duration note)
    TBarline a -> pure $ TBarline a
    TRest (Rest dur) -> pure $ TRest $ Rest dur

map_note :: Applicative m => (Note pitch1 ndur -> m (Note pitch2 ndur))
    -> Token pitch1 ndur rdur -> m (Token pitch2 ndur rdur)
map_note f = \case
    TNote note -> TNote <$> f note
    TBarline a -> pure $ TBarline a
    TRest a -> pure $ TRest a

-- | Opposide from Ruler.Rank, higher numbers mean larger divisions.
type Rank = Int

newtype Barline = Barline Rank
    deriving (Eq, Show)

data Note pitch dur = Note {
    note_call :: !Call
    , note_pitch :: !pitch
    , note_duration :: !dur
    } deriving (Eq, Show)

instance (Pretty pitch, Pretty dur) => Pretty (Note pitch dur) where
    pretty (Note call pitch dur) = pretty (call, pitch, dur)

newtype Call = Call Text
    deriving (Eq, Show, Pretty, String.IsString)

newtype Rest dur = Rest dur
    deriving (Eq, Show)

data Pitch = Pitch {
    pitch_octave :: !Octave
    , pitch_call :: !Text
    } deriving (Eq, Show)

data Octave = Absolute !Int | Relative !Int
    deriving (Eq, Show)

data NDuration = NDuration !Duration | CallDuration
    deriving (Eq, Show)

data Duration = Duration {
    dur_int :: !(Maybe Int)
    , dur_dots :: !Int
    , dur_tie :: !Bool
    } deriving (Eq, Show)
