-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Create val calls for scale degrees.  These are the calls that a scale
-- brings into scope, so they should be referenced from
-- 'Derive.scale_note_to_call' implementations.
module Derive.Call.ScaleDegree (
    -- * equal tempered
    scale_degree
    , pitch_expr
    -- * just
    , NamedIntervals
    , scale_degree_just, scale_degree_interval
) where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.ParseBs as ParseBs
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- * equal tempered

-- | Create a pitch val call for the given scale degree.  This is intended to
-- be used by scales to generate their calls, but of course each scale may
-- define calls in its own way.
scale_degree :: Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall
scale_degree pitch_nn pitch_note = Derive.val_call
    "pitch" Tags.scale "Emit the pitch of a scale degree." $ Sig.call ((,)
    <$> defaulted "frac" 0
        "Add this many hundredths of a scale degree to the output."
    <*> defaulted "hz" 0 "Add an absolute hz value to the output."
    ) $ \(frac, hz) _ -> do
        environ <- Internal.get_dynamic Derive.state_environ
        return $! TrackLang.VPitch $ PitchSignal.pitch
            (call frac hz environ) (pitch_note environ)
    where
    call frac hz environ controls =
        Pitch.add_hz (hz + get_hz controls) <$> pitch_nn environ
            (if frac == 0 then controls
                else Map.insertWith' (+) Controls.chromatic (frac / 100)
                    controls)

get_hz :: Score.ControlValMap -> Pitch.Hz
get_hz = Map.findWithDefault 0 Controls.hz

-- | Convert a note and @frac@ arg into a tracklang expression representing
-- that note.
pitch_expr :: Pitch.Note -> Double -> Text
pitch_expr (Pitch.Note note) frac
    | frac == 0 = note
    | otherwise = note <> " " <> showt (floor (frac * 100))

-- * just

-- | Map from named intervals to the interal's ratio.
type NamedIntervals = Map.Map Text Ratio.Rational

-- | A fancier version of 'scale_degree' that takes interval arguments.
scale_degree_just :: NamedIntervals
    -> Pitch.Hz -- ^ add an arbitrary extra interval to the output
    -> Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall
scale_degree_just named_intervals extra_interval pitch_nn pitch_note =
    Derive.val_call "pitch" Tags.scale
    "Emit the pitch of a scale degree." $
    Sig.call (intervals_arg named_intervals) $ \intervals _ -> do
        interval <- resolve_intervals named_intervals intervals
        environ <- Internal.get_dynamic Derive.state_environ
        return $! TrackLang.VPitch $ PitchSignal.pitch
            (call (extra_interval * interval) environ) (pitch_note environ)
    where
    call interval environ controls = modify interval (get_hz controls) <$>
        pitch_nn environ controls
    modify interval hz
        | interval == 1 && hz == 0 = id
        | otherwise = Pitch.modify_hz ((+hz) . (*interval))

scale_degree_interval :: NamedIntervals -> Pitch.Note -> Maybe Derive.ValCall
scale_degree_interval named_intervals note =
    case parse_relative_interval named_intervals note of
        Nothing -> Nothing
        Just interval -> Just $ relative_scale_degree named_intervals interval

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
    parse_num = case ParseBs.parse_val (Pitch.note_text note) of
        Right (TrackLang.VNum (Score.Typed Score.Untyped num)) -> Just num
        _ -> Nothing
    unsign val = if val < 0 then recip (abs val) else val

relative_scale_degree :: NamedIntervals -> Pitch.Hz -> Derive.ValCall
relative_scale_degree named_intervals initial_interval =
    Derive.val_call "pitch" Tags.scale "doc doc" $
    Sig.call (intervals_arg named_intervals) $ \intervals args -> do
        interval <- (initial_interval*) <$>
            resolve_intervals named_intervals intervals
        Args.prev_val args >>= \x -> case x of
            Just (_, Derive.TagPitch prev) ->
                return $ TrackLang.VPitch (modify interval prev)
            _ -> Derive.throw "relative interval requires a previous pitch"
    where
    modify interval pitch =
        PitchSignal.pitch (pitch_nn interval pitch)
            (PitchSignal.eval_note pitch)
    pitch_nn interval pitch controls = do
        nn <- PitchSignal.eval_pitch pitch controls
        return $ Pitch.modify_hz (*interval) nn

resolve_intervals :: NamedIntervals -> [Either Pitch.Hz Text]
    -> Derive.Deriver Pitch.Hz
resolve_intervals named_intervals intervals =
    product . map unsign <$> mapM (either return resolve) intervals
    where
    resolve text = Derive.require ("unknown named interval: " <> show text) $
        resolve_interval named_intervals text
    unsign val = if val < 0 then recip (abs val) else val

resolve_interval :: NamedIntervals -> Text -> Maybe Pitch.Hz
resolve_interval named_intervals text = case Text.uncons text of
    Just ('-', text) -> negate <$> lookup text
    _ -> lookup text
    where lookup text = realToFrac <$> Map.lookup text named_intervals
