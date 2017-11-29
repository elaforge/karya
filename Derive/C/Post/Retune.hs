-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls to do with intonation and tuning.
module Derive.C.Post.Retune (library) where
import qualified Util.Num as Num
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


library :: Derive.Library
library = Library.transformers
    [ ("retune", c_retune)
    , ("realize-retune", c_realize_retune)
    ]

module_ :: Module.Module
module_ = "retune"

-- | (time, dist)
type RetuneArg = (RealTime, Pitch.NoteNumber)

retune_arg :: Text
retune_arg = "retune"

c_retune :: Derive.Transformer Derive.Note
c_retune = Derive.transformer module_ "retune" Tags.delayed
    "Notes start with some detuning, and adjust to the desired pitch.  This\
    \ effect is scaled by the time and pitch distance from the previous note,\
    \ as documeneted in 'Derive.Call.Post.Retune.pitch_scale'."
    $ Sig.callt ((,)
    <$> Sig.defaulted "time" (Sig.control "retune-time" 0.15)
        "RealTime to get to the intended pitch."
    <*> Sig.defaulted "dist" (Sig.control "retune-dist" 0.15)
        "Out of tune distance, in NNs. Presumably this should be set to a\
        \ control function with a bimodal distribution."
    ) $ \(time, dist) args deriver -> do
        start <- Args.real_start args
        -- TODO typecheck
        time <- RealTime.seconds <$> Call.control_at time start
        dist <- Pitch.nn <$> Call.control_at dist start
        Post.emap1_ (put time dist) <$> deriver
    where
    put time dist = Score.put_arg retune_arg ((time, dist) :: RetuneArg)

c_realize_retune :: Derive.Transformer Derive.Note
c_realize_retune = Derive.transformer module_ "retune-realize"
    Tags.realize_delayed "Perform annotations added by `retune`."
    $ Sig.call0t $ \_args deriver -> do
        srate <- Call.get_srate
        Post.emap_m_ snd (realize srate) . Post.prev_by Post.hand_key
            =<< deriver
    where
    realize srate (prev, event) = do
        (event, maybe_arg) <- Derive.require_right id $
            Score.take_arg retune_arg event
        return $ case maybe_arg of
            Nothing -> [event]
            Just arg -> [realize_retune srate arg prev event]

realize_retune :: ControlUtil.SRate -> RetuneArg -> Maybe Score.Event
    -> Score.Event -> Score.Event
realize_retune srate (time, max_dist) prev event
    | dist == 0 = event
    | otherwise = add_nn_transpose curve event
    where
    dist = calculate_retune (pitch_distance prev event) max_dist
    curve = retune_curve srate time dist (Score.event_start event)
        (Score.event_end event)

-- | Transpose an event by adding to its nn transpose control.
add_nn_transpose :: Signal.Control -> Score.Event -> Score.Event
add_nn_transpose curve = Score.modify_control Controls.nn (Signal.sig_add curve)

retune_curve :: ControlUtil.SRate -> RealTime -> Pitch.NoteNumber -> RealTime
     -> RealTime -> Signal.Control
retune_curve srate time dist start end =
    ControlUtil.segment srate True True curve
        start (Pitch.nn_to_double dist) (min end (start + time)) 0
    where
    -- Adjust quickly at first, then slow down.
    curve = ControlUtil.expon (-4)

pitch_distance :: Maybe Score.Event -> Score.Event
    -> (RealTime, Pitch.NoteNumber)
pitch_distance prev cur = fromMaybe (0, 0) $
    distance <$> (get_last x2 =<< prev) <*> Score.initial_nn cur
    where
    x2 = Score.event_start cur
    distance (x1, y1) y2 = (x2 - x1, y2 - y1)
    get_last x event = do
        (pos, pitch) <- Score.pitch_sample_at x event
        nn <- either (const Nothing) Just $ PSignal.pitch_nn $
            Score.apply_controls event x pitch
        Just (pos, nn)

calculate_retune :: (RealTime, Pitch.NoteNumber)
    -> Pitch.NoteNumber -> Pitch.NoteNumber
calculate_retune (time_diff, nn_diff) max_dist =
    max_dist * pitch_scale time_diff nn_diff

-- | Scale the nn distance from 0 to 1 based on the pitch and time distance to
-- the last pitch sample of the previous note.
--
-- An octave is the most inaccurate.  Unison is totally accurate.  0 time is
-- the most inaccurate, while 1 second is still a little bit inaccurate.
pitch_scale :: RealTime -> Pitch.NoteNumber -> Pitch.NoteNumber
pitch_scale time_diff nn_diff =
    Pitch.nn (RealTime.to_seconds time_scale) * nn_scale
    where
    nn_scale = Num.clamp 0 12 (abs nn_diff) / 12
    time_scale = Num.scale 0.15 1 $ 1 - Num.clamp 0 1 (abs time_diff)
