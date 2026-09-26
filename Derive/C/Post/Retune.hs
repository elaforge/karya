-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls to do with intonation and tuning.
module Derive.C.Post.Retune (library) where
import qualified Util.Num as Num
import qualified Util.Segment as Segment
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

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("retune", cRetune)
    , ("realize-retune", cRealizeRetune)
    ]

module_ :: Module.Module
module_ = "retune"

-- | (time, dist)
type RetuneArg = (RealTime, Pitch.NoteNumber)

retuneArg :: Text
retuneArg = "retune"

cRetune :: Derive.Transformer Derive.Note
cRetune = Derive.transformer module_ "retune" Tags.delayed
    "Notes start with some detuning, and adjust to the desired pitch.  This\
    \ effect is scaled by the time and pitch distance from the previous note,\
    \ as documeneted in 'Derive.Call.Post.Retune.pitch_scale'."
    $ Sig.callt ((,)
    <$> Sig.defaulted "time" (0.15 :: Double)
        "RealTime to get to the intended pitch."
    <*> Sig.defaulted "dist" (0.15 :: Double)
        "Out of tune distance, in NNs. Presumably this should be set to a\
        \ control function with a bimodal distribution."
    ) $ \(time, dist) _args -> fmap (Post.emap1_ (put time dist))
    where put time dist = Score.putArg retuneArg ((time, dist) :: RetuneArg)

cRealizeRetune :: Derive.Transformer Derive.Note
cRealizeRetune = Derive.transformer module_ "retune-realize"
    Tags.realizeDelayed "Perform annotations added by `retune`."
    $ Sig.call0t $ \_args deriver -> do
        srate <- Call.getSrate
        Post.emapM_ snd (realize srate) . Post.prevBy Post.handKey
            =<< deriver
    where
    realize srate (prev, event) = do
        (event, maybeArg) <- Derive.requireRight id $
            Score.takeArg retuneArg event
        pure $ case maybeArg of
            Nothing -> [event]
            Just arg -> [realizeRetune srate arg prev event]

realizeRetune :: ControlUtil.SRate -> RetuneArg -> Maybe Score.Event
    -> Score.Event -> Score.Event
realizeRetune srate (time, maxDist) prev event
    | dist == 0 = event
    | otherwise = addNnTranspose curve event
    where
    dist = calculateRetune (pitchDistance prev event) maxDist
    curve = retuneCurve srate time dist (Score.eventStart event)
        (Score.eventEnd event)

-- | Transpose an event by adding to its nn transpose control.
addNnTranspose :: Signal.Control -> Score.Event -> Score.Event
addNnTranspose curve = Score.modifySignal Controls.nn (Signal.sigAdd curve)

retuneCurve :: ControlUtil.SRate -> RealTime -> Pitch.NoteNumber -> RealTime
     -> RealTime -> Signal.Control
retuneCurve srate time dist start end =
    ControlUtil.segment srate curve
        start (Pitch.nnToDouble dist) (min end (start + time)) 0
    where
    -- Adjust quickly at first, then slow down.
    curve = ControlUtil.Function $ ControlUtil.expon (-4)

-- | Get the pitch and the time ago it was established.
-- TODO this gets the start of the segment, so if it's sloped, this isn't the
-- current pitch.  What should happen with notes that already have some pitch
-- variation is dependent on how I want this to work, but for now start of the
-- segment still seems reasonable.
pitchDistance :: Maybe Score.Event -> Score.Event
    -> (RealTime, Pitch.NoteNumber)
pitchDistance prev cur = fromMaybe (0, 0) $
    distance <$> (getLast x2 =<< prev) <*> Score.initialNn cur
    where
    x2 = Score.eventStart cur
    distance (x1, y1) y2 = (x2 - x1, y2 - y1)
    getLast x event = do
        segment <- PSignal.segmentAt (Score.eventPitch event) x
        nn <- either (const Nothing) Just $ PSignal.pitchNn $
            Score.applyControls event x $ Segment._y1 segment
        Just (Segment._x1 segment, nn)

calculateRetune :: (RealTime, Pitch.NoteNumber)
    -> Pitch.NoteNumber -> Pitch.NoteNumber
calculateRetune (timeDiff, nnDiff) maxDist =
    maxDist * pitchScale timeDiff nnDiff

-- | Scale the nn distance from 0 to 1 based on the pitch and time distance to
-- the last pitch sample of the previous note.
--
-- An octave is the most inaccurate.  Unison is totally accurate.  0 time is
-- the most inaccurate, while 1 second is still a little bit inaccurate.
pitchScale :: RealTime -> Pitch.NoteNumber -> Pitch.NoteNumber
pitchScale timeDiff nnDiff =
    Pitch.nn (RealTime.toSeconds timeScale) * nnScale
    where
    nnScale = Num.clamp 0 12 (abs nnDiff) / 12
    timeScale = Num.scale 0.15 1 $ 1 - Num.clamp 0 1 (abs timeDiff)
