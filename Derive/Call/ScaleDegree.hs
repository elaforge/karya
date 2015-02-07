-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Create val calls for scale degrees.  These are the calls that a scale
-- brings into scope, so they should be referenced from
-- 'Derive.scale_note_to_call' implementations.
module Derive.Call.ScaleDegree (
    -- * equal tempered
    scale_degree, pitch_expr
    -- * just
    , NamedIntervals
    , scale_degree_just, scale_degree_interval
) where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Parse as Parse
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import Global


-- * equal tempered

-- | Create a pitch val call for the given scale degree.  This is intended to
-- be used by scales to generate their calls, but of course each scale may
-- define calls in its own way.
scale_degree :: PitchSignal.Scale -> Scale.PitchNn -> Scale.PitchNote
    -> Derive.ValCall
scale_degree scale pitch_nn pitch_note = Derive.val_call Module.scale
    "pitch" mempty "Emit the pitch of a scale degree." $
    Sig.call (Sig.defaulted "frac" 0
        "Add this many hundredths of a scale degree to the output.")
    $ \frac _args -> do
        env <- Internal.get_environ
        let config = PitchSignal.PitchConfig env controls
            controls = if frac == 0 then mempty
                else Map.singleton Controls.chromatic (frac / 100)
        return $! PitchSignal.pitch scale
            (\config -> add_absolute_transposers config <$> pitch_nn config)
            pitch_note config

add_absolute_transposers :: PitchSignal.PitchConfig -> Pitch.NoteNumber
    -> Pitch.NoteNumber
add_absolute_transposers config nn =
    Pitch.add_hz (Map.findWithDefault 0 Controls.hz controls)
        (nn + Pitch.nn (Map.findWithDefault 0 Controls.nn controls))
    where controls = PitchSignal.pitch_controls config

-- | Convert a note and @frac@ arg into a tracklang expression representing
-- that note.
pitch_expr :: Double -> Pitch.Note -> Pitch.Note
pitch_expr frac note
    | frac == 0 = note
    | otherwise = Pitch.Note $
        Pitch.note_text note <> " " <> showt (floor (frac * 100))

-- * just

-- | Map from named intervals to the interval's ratio.
type NamedIntervals = Map.Map Text Ratio.Rational

-- | A fancier version of 'scale_degree' that takes interval arguments.
scale_degree_just :: PitchSignal.Scale -> NamedIntervals
    -> Pitch.Hz -- ^ add an arbitrary extra interval to the output
    -> Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall
scale_degree_just scale named_intervals extra_interval pitch_nn pitch_note =
    Derive.val_call Module.scale "pitch" mempty
    "Emit the pitch of a scale degree."
    $ Sig.call (intervals_arg named_intervals) $ \intervals _ -> do
        interval <- resolve_intervals named_intervals intervals
        env <- Internal.get_environ
        return $! PitchSignal.pitch scale
            (\config -> modify (extra_interval*interval) config <$>
                pitch_nn config)
            pitch_note (PitchSignal.PitchConfig env mempty)
    where
    modify interval config = add_absolute_transposers config
        . Pitch.modify_hz (*interval)

scale_degree_interval :: PitchSignal.Scale -> NamedIntervals -> Pitch.Note
    -> Maybe Derive.ValCall
scale_degree_interval scale named_intervals note =
    case parse_relative_interval named_intervals note of
        Nothing -> Nothing
        Just interval ->
            Just $ relative_scale_degree scale named_intervals interval

intervals_arg :: NamedIntervals -> Sig.Parser [Either Pitch.Hz Text]
intervals_arg named_intervals = Sig.many "interval" $
    "Multiply this interval with the note's frequency. Negative numbers\
    \ divide, so while `3/2` goes up a fifth, `-3/2` goes down a fifth.\
    \ Can be either a ratio or a symbol drawn from: "
    <> Text.intercalate ", " (Map.keys named_intervals)

parse_relative_interval :: NamedIntervals -> Pitch.Note -> Maybe Pitch.Hz
parse_relative_interval named_intervals note =
    unsign <$> (resolve_interval named_intervals (Pitch.note_text note)
        `mplus` parse_num)
    where
    parse_num = case Parse.parse_val (Pitch.note_text note) of
        Right (TrackLang.VNum (Score.Typed Score.Untyped num)) -> Just num
        _ -> Nothing
    unsign val = if val < 0 then recip (abs val) else val

relative_scale_degree :: PitchSignal.Scale -> NamedIntervals -> Pitch.Hz
    -> Derive.ValCall
relative_scale_degree scale named_intervals initial_interval =
    Derive.val_call Module.scale "pitch" mempty
    "Emit a pitch that is a relative interval from the previous pitch." $
    Sig.call (intervals_arg named_intervals) $ \intervals args -> do
        interval <- (initial_interval*) <$>
            resolve_intervals named_intervals intervals
        start <- Args.real_start args
        Derive.require "relative interval requires a previous pitch" $ do
            Derive.TagPitch prev <- Args.prev_val args
            modify interval <$> PitchSignal.at start prev
    where
    modify interval pitch = PitchSignal.pitch scale (pitch_nn interval pitch)
        (PitchSignal.pitch_eval_note pitch) (PitchSignal.pitch_config pitch)
    pitch_nn interval pitch = \config -> do
        nn <- PitchSignal.pitch_nn $ PitchSignal.coerce $
            PitchSignal.config config pitch
        return $ Pitch.modify_hz (*interval) nn

resolve_intervals :: NamedIntervals -> [Either Pitch.Hz Text]
    -> Derive.Deriver Pitch.Hz
resolve_intervals named_intervals intervals =
    product . map unsign <$> mapM (either return resolve) intervals
    where
    resolve text = Derive.require ("unknown named interval: " <> showt text) $
        resolve_interval named_intervals text
    unsign val = if val < 0 then recip (abs val) else val

resolve_interval :: NamedIntervals -> Text -> Maybe Pitch.Hz
resolve_interval named_intervals text = case Text.uncons text of
    Just ('-', text) -> negate <$> lookup text
    _ -> lookup text
    where lookup text = realToFrac <$> Map.lookup text named_intervals
