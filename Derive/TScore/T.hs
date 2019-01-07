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

import qualified Util.TextUtil as TextUtil
import qualified Ui.Id as Id

import           Global


-- | This is the default "beat".
newtype Time = Time Ratio.Rational
    deriving (Ord, Eq, Num, Enum, Real, Fractional, RealFrac, Pretty)

instance Show Time where
    show (Time t) = prettys t

-- * Score

newtype Score = Score [(Pos, Toplevel)] deriving (Eq, Show)

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
    TBarline !Pos !Barline
    | TNote !Pos !(Note pitch ndur)
    | TRest !Pos !(Rest rdur)
    deriving (Eq, Show)

token_pos :: Token pitch ndur rdur -> Pos
token_pos = \case
    TBarline pos _ -> pos
    TNote pos _ -> pos
    TRest pos _ -> pos

token_name :: Token pitch ndur rdur -> Text
token_name = \case
    TBarline {} -> "barline"
    TNote {} -> "note"
    TRest {} -> "rest"

map_pitch :: Applicative m => (pitch1 -> m pitch2) -> Token pitch1 ndur rdur
    -> m (Token pitch2 ndur rdur)
map_pitch f = \case
    TNote pos note ->
        TNote pos . (\a -> note { note_pitch = a}) <$> f (note_pitch note)
    TBarline pos a -> pure $ TBarline pos a
    TRest pos a -> pure $ TRest pos a

map_note_duration :: Applicative m => (dur1 -> m dur2) -> Token pitch dur1 rdur
    -> m (Token pitch dur2 rdur)
map_note_duration f = \case
    TNote pos note -> TNote pos . (\a -> note { note_duration = a }) <$>
        f (note_duration note)
    TBarline pos a -> pure $ TBarline pos a
    TRest pos (Rest dur) -> pure $ TRest pos (Rest dur)

map_note :: Applicative m => (Note pitch1 ndur -> m (Note pitch2 ndur))
    -> Token pitch1 ndur rdur -> m (Token pitch2 ndur rdur)
map_note f = \case
    TNote pos note -> TNote pos <$> f note
    TBarline pos a -> pure $ TBarline pos a
    TRest pos a -> pure $ TRest pos a

-- | Opposide from Ruler.Rank, higher numbers mean larger divisions.
type Rank = Int

newtype Barline = Barline Rank
    deriving (Eq, Show)

data Note pitch dur = Note {
    note_call :: !Call
    , note_pitch :: !pitch
    , note_duration :: !dur
    , note_pos :: !Pos
    } deriving (Eq, Show)

instance (Pretty pitch, Pretty dur) => Pretty (Note pitch dur) where
    pretty (Note call pitch dur _pos) = pretty (call, pitch, dur)

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


-- * error

-- | Character position in the input.
newtype Pos = Pos Int deriving (Eq, Show)

data Error = Error !Pos !Text
    deriving (Eq, Show)

instance Pretty Error where
    pretty (Error (Pos pos) msg) = pretty pos <> ": " <> msg

show_error :: Text -> Error -> Text
show_error source (Error pos msg) =
    TextUtil.joinWith "\n" msg $ fromMaybe "" $ do
        (line_num, char_num, line) <- find_pos source pos
        return $ Text.unlines
            [ Text.justifyRight 3 ' ' (showt line_num) <> " | " <> line
            , Text.replicate (3 + 3 + char_num) " " <> "^"
            ]

-- | Find the line and position on that line.
find_pos :: Text -> Pos -> Maybe (Int, Int, Text)
find_pos source (Pos pos) =
    case drop_before (fst . snd) pos (zip [1..] lines) of
        (line_num, (start, line)) : _ -> Just (line_num, pos - start, line)
        _ -> Nothing
    where
    lines = scanl_on (+) ((+1) . Text.length) 0 $ Text.lines source

-- * seq

scanl_on :: (accum -> key -> accum) -> (a -> key) -> accum -> [a]
    -> [(accum, a)]
scanl_on f key z xs = zip (scanl (\t -> f t . key) z xs) xs

-- | Drop until the last element before or equal to the given element.
drop_before :: Ord key => (a -> key) -> key -> [a] -> [a]
drop_before _ _ [] = []
drop_before key p (x:xs)
    | p < key x = x : xs
    | otherwise = case xs of
        next : _ | p >= key next -> drop_before key p xs
        _ -> x : xs
