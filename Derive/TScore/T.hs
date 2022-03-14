-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- deriving (Real) for Time emits this warning.
{-# OPTIONS_GHC -fno-warn-identities #-}
-- | Shared types for TScore.
module Derive.TScore.T where
import qualified Data.Ratio as Ratio
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Util.Texts as Texts
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Ui.Id as Id

import           Global


-- | This is the default "beat".
newtype Time = Time Ratio.Rational
    deriving (Ord, Eq, Num, Enum, Real, Fractional, RealFrac, Pretty)

instance Show Time where
    show (Time t) = prettys t

-- * Score

newtype Score = Score [(Pos, Toplevel)] deriving (Eq, Show)

data Toplevel =
    ToplevelDirective !Directive
    | BlockDefinition !(Block WrappedTracks)
    deriving (Eq, Show)

-- | call is a parameter, because 'SubBlock' will later be resolved to
-- 'CallText'.
data Block tracks = Block {
    block_id :: !Id.BlockId
    , block_directives :: ![Directive]
    , block_title :: !Text
    , block_tracks :: !tracks
    } deriving (Eq, Show)

data WrappedTracks = WrappedTracks !Pos ![Tracks Call]
    deriving (Eq, Show)

newtype Tracks call = Tracks [Track call]
    deriving (Eq, Show)

untracks :: Tracks call -> [Track call]
untracks (Tracks tracks) = tracks

data Track call = Track {
    -- | Some arbitrary symbols.  This has no meaning except to make the track
    -- with its title unique on this block.  This is so that tracks have an
    -- identity, and I can detect track moves, adds, and deletes.
    track_key :: !Text
    -- | The track title will include a > if the original syntax did, or be ""
    -- if the track has no title at all, which is only possible for the first
    -- one, e.g. [a] or [a > b].  This way unparse can emit the same
    -- abbreviated syntax.
    , track_title :: !Text
    , track_directives :: ![Directive]
    , track_tokens :: ![Token call (NPitch Pitch) NDuration Duration]
    , track_pos :: !Pos
    } deriving (Eq, Show)

data Directive = Directive !Pos !Text !(Maybe Text)
    deriving (Eq, Show)

-- | This is a simplified subset of 'Ui.UiConfig.Allocation'.
data Allocation = Allocation {
    alloc_name :: !Instrument
    , alloc_qualified :: !InstT.Qualified
    , alloc_config :: !Config
    , alloc_backend :: !Backend
    } deriving (Eq, Show)

-- | Subset of 'Instrument.Common.Config'.
data Config = Config {
    config_mute :: !Bool
    , config_solo :: !Bool
    } deriving (Eq, Show)

empty_config :: Config
empty_config = Config False False

-- | This is ScoreT.Instrument, but I don't want to import ScoreT here.
type Instrument = Text
data Backend = Midi Midi.WriteDevice [Midi.Channel] | ImSc
    deriving (Eq, Show)

data Token call pitch ndur rdur =
    -- | Higher count for larger divisions, e.g. anga vs. avartanam.
    TBarline !Pos !Barline
    | TNote !Pos !(Note call pitch ndur)
    | TRest !Pos !(Rest rdur)
    deriving (Eq, Show)

token_pos :: Token call pitch ndur rdur -> Pos
token_pos = \case
    TBarline pos _ -> pos
    TNote pos _ -> pos
    TRest pos _ -> pos

token_name :: Token call pitch ndur rdur -> Text
token_name = \case
    TBarline {} -> "barline"
    TNote {} -> "note"
    TRest {} -> "rest"

map_call :: (call1 -> call2)
    -> Token call1 pitch ndur rdur -> Token call2 pitch ndur rdur
map_call f = \case
    TBarline pos a -> TBarline pos a
    TNote pos note -> TNote pos (note { note_call = f (note_call note) })
    TRest pos a -> TRest pos a

map_pitch :: Applicative m => (pitch1 -> m pitch2)
    -> Token call pitch1 ndur rdur -> m (Token call pitch2 ndur rdur)
map_pitch f = \case
    TBarline pos a -> pure $ TBarline pos a
    TNote pos note ->
        TNote pos . (\a -> note { note_pitch = a}) <$> f (note_pitch note)
    TRest pos a -> pure $ TRest pos a

map_note_duration :: Applicative m => (dur1 -> m dur2)
    -> Token call pitch dur1 rdur -> m (Token call pitch dur2 rdur)
map_note_duration f = \case
    TBarline pos a -> pure $ TBarline pos a
    TNote pos note -> TNote pos . (\a -> note { note_duration = a }) <$>
        f (note_duration note)
    TRest pos (Rest dur) -> pure $ TRest pos (Rest dur)

map_note :: Applicative m
    => (Note call1 pitch1 ndur -> m (Note call2 pitch2 ndur))
    -> Token call1 pitch1 ndur rdur -> m (Token call2 pitch2 ndur rdur)
map_note f = \case
    TBarline pos a -> pure $ TBarline pos a
    TNote pos note -> TNote pos <$> f note
    TRest pos a -> pure $ TRest pos a

-- | Opposite from Ruler.Rank, higher numbers mean larger divisions.
type Rank = Int

data Barline = Barline Rank | AssertCoincident
    deriving (Eq, Show)

data Note call pitch dur = Note {
    note_call :: !call
    , note_pitch :: !pitch
    -- | The generated event should have 0 duration.
    , note_zero_duration :: !Bool
    , note_duration :: !dur
    -- | This is redundant with 'TNote's Pos, but convenient, since Check will
    -- later strip away 'Token's.
    , note_pos :: !Pos
    } deriving (Eq, Show)

data Call = Call !CallText
    -- | A call can take multiple 'Tracks' arguments, each one of which is a
    -- sub-block.
    | SubBlock !CallText ![Tracks Call]
    deriving (Eq, Show)

instance String.IsString Call where
    fromString = Call . txt

-- | Tracklang expression.  This goes into the event text.
type CallText = Text

newtype Rest dur = Rest dur
    deriving (Eq, Show)

data NPitch pitch = CopyFrom | NPitch !pitch
    deriving (Eq, Show)

instance Pretty pitch => Pretty (NPitch pitch) where
    pretty (NPitch pitch) = pretty pitch
    pretty CopyFrom = "CopyFrom"

data Pitch = Pitch {
    pitch_octave :: !Octave
    , pitch_call :: !Text
    } deriving (Eq, Show)

type PitchText = Text

data Octave = Absolute !Int | Relative !Int
    deriving (Eq, Show)

data NDuration = NDuration !Duration | CallDuration
    deriving (Eq, Show)

data Duration = Duration {
    -- | Durations are specified as two optional integers: int1, or int1:int2.
    -- The interpretation is up to the dur Directive.
    dur_int1 :: !(Maybe Int)
    , dur_int2 :: !(Maybe Int)
    , dur_dots :: !Int
    , dur_tie :: !Bool
    } deriving (Eq, Show)

-- * error

-- | Character position in the input.
newtype Pos = Pos Int deriving (Eq, Show, Pretty)

data Error = Error !Pos !Text
    deriving (Eq, Show)

instance Pretty Error where
    pretty (Error (Pos pos) msg) = pretty pos <> ": " <> msg

show_error :: Text -> Error -> Text
show_error source (Error pos msg) = Texts.unlines2 msg $ fromMaybe "" $ do
    (line_num, char_num, line) <- find_pos source pos
    return $ Text.unlines
        [ Text.justifyRight 3 ' ' (showt line_num) <> " | " <> line
        , Text.replicate (3 + 3 + char_num) " " <> "^"
        ]

-- | Find the line and position on that line.
find_pos :: Text -> Pos -> Maybe (Int, Int, Text)
find_pos source (Pos pos) =
    case Seq.drop_before (fst . snd) pos (zip [1..] lines) of
        (line_num, (start, line)) : _ -> Just (line_num, pos - start, line)
        _ -> Nothing
    where
    lines = Seq.scanl_on (+) ((+1) . Text.length) 0 $ Text.lines source
