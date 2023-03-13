-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls to highlight events.
module Derive.C.Prelude.Highlight (
    library
    , c_highlight_strings_note
    , out_of_range, open_strings, warn_non_open
) where
import qualified Data.Maybe as Maybe

import qualified Util.Test.ApproxEq as ApproxEq
import qualified Derive.Args as Args
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Ui.Color as Color

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("highlight", c_highlight)
    , ("highlight-strings", c_highlight_strings)
    , ("highlight-out-of-range", c_highlight_out_of_range)
    ]

c_highlight :: Derive.Transformer Derive.Note
c_highlight = Derive.transformer Module.prelude "highlight" mempty
    "Add a highlight color."
    $ Sig.callt (Sig.required "highlight" "Highlight code.")
    $ \highlight _ deriver -> Post.emap1_ (add_highlight highlight) <$> deriver

instance ShowVal.ShowVal Color.Highlight
instance Typecheck.Typecheck Color.Highlight


-- * open strings

-- | This is a generator that goes on null call, not a transformer over
-- everything.
c_highlight_strings_note :: Derive.Generator Derive.Note
c_highlight_strings_note = Note.transformed_note_name "note-highlight"
    ("Highlight any notes whose initial pitch isn't in "
        <> ShowVal.doc EnvKey.open_strings <> ".")
    mempty $ \args deriver -> do
        pos <- Args.real_start args
        out_of_range (Args.start args) $ open_strings pos warn_non_open deriver

c_highlight_strings :: Derive.Transformer Derive.Note
c_highlight_strings = Derive.transformer Module.prelude "highlight-strings"
    mempty ("Highlight any notes whose initial pitch either is or isn't in "
        <> ShowVal.doc EnvKey.open_strings <> ".")
    $ Sig.callt
    ( Sig.environ "open" Sig.Prefixed False
        "If true, put Info on open strings, else put Warning on non-open ones."
    ) $ \highlight_open args deriver -> do
        pos <- Args.real_start args
        open_strings pos (if highlight_open then notice_open else warn_non_open)
            deriver

warn_non_open :: Bool -> Maybe Color.Highlight
warn_non_open open = if open then Nothing else Just Color.Warning

notice_open :: Bool -> Maybe Color.Highlight
notice_open open = if open then Just Color.Notice else Nothing

open_strings :: RealTime -> (Bool -> Maybe Color.Highlight)
    -- ^ True if this note is on an open string.
    -> Derive.NoteDeriver -> Derive.NoteDeriver
open_strings pos highlight deriver = do
    maybe_pitches <- Derive.lookup_val EnvKey.open_strings
    maybe_pitches <- case maybe_pitches of
        Just pitches -> Just <$> mapM (Derive.resolve_pitch pos) pitches
        Nothing -> return Nothing
    maybe id (\pitches -> fmap (Post.emap1_ (apply pitches))) maybe_pitches
        deriver
    where
    apply :: [PSignal.Transposed] -> Score.Event -> Score.Event
    apply pitches event = case Score.initial_nn event of
        Just nn -> case highlight (any (same_pitch nn) pitches) of
            Nothing -> event
            Just highlight -> add_highlight highlight event
        _ -> event
    same_pitch nn pitch = case PSignal.pitch_nn pitch of
        Right string_nn -> ApproxEq.eq 0.05 nn string_nn
        _ -> False


-- * out of range

c_highlight_out_of_range :: Derive.Transformer Derive.Note
c_highlight_out_of_range = Derive.transformer Module.prelude
    "highlight-out-of-range" mempty
    ("Error on notes whose initial pitch is below "
        <> ShowVal.doc EnvKey.instrument_bottom <> " or above "
        <> ShowVal.doc EnvKey.instrument_top <> ". The range must be \
        \ in NNs.")
    $ Sig.call0t $ \args deriver -> out_of_range (Args.start args) deriver

-- | Highlight with 'Color.Warning' if there is 'EnvKey.instrument_top' or
-- 'EnvKey.instrument_bottom' and the pitch is above or below it,
-- respectively.
out_of_range :: ScoreTime -> Derive.NoteDeriver -> Derive.NoteDeriver
out_of_range pos deriver = do
    (maybe_top, maybe_bottom) <- get_instrument_range pos
    if all Maybe.isNothing [maybe_top, maybe_bottom] then deriver
        else Post.emap1_ (apply maybe_top maybe_bottom) <$> deriver
    where
    apply maybe_top maybe_bottom event
        | is_oor = add_highlight Color.Error event
        | otherwise = event
        where
        is_oor = fromMaybe False $ do
            nn <- Score.initial_nn event
            return $ maybe False (<nn) maybe_top
                || maybe False (>nn) maybe_bottom

get_instrument_range :: ScoreTime
    -> Derive.Deriver (Maybe Pitch.NoteNumber, Maybe Pitch.NoteNumber)
get_instrument_range pos = do
    (_parse_p, show_p, _) <- Call.get_pitch_functions
    maybe_top <- get show_p EnvKey.instrument_top
    maybe_bottom <- get show_p EnvKey.instrument_bottom
    return (maybe_top, maybe_bottom)
    where
    get show_p key =
        maybe (return Nothing)
            (fmap Just . either (resolve_pitch show_p pos) return)
        =<< Derive.lookup_val key

resolve_pitch :: (Pitch.Pitch -> Maybe Pitch.Note) -> ScoreTime -> Pitch.Pitch
    -> Derive.Deriver Pitch.NoteNumber
resolve_pitch show_p pos = Pitches.pitch_nn <=< Call.eval_pitch show_p pos

_initial_pitch :: Scale.Scale -> Score.Event -> Maybe Pitch.Pitch
_initial_pitch scale event = do
    note <- Score.initial_note event
    either (const Nothing) Just $ Scale.scale_read scale mempty note


-- * util

add_highlight :: Color.Highlight -> Score.Event -> Score.Event
add_highlight highlight event = event
    { Score.event_highlight = highlight <> Score.event_highlight event }
