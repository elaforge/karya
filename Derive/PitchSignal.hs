-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Derive.PitchSignal (
    Signal, sig_scale_id, sig_scale, Scale(Scale)
    -- * construct and convert
    , constant, signal, unsignal, to_nn
    -- * apply controls
    , apply_controls, apply_control, controls_at
    -- * signal functions
    , null, at, shift, last
    , take, drop_after, drop_before
    -- * Pitch
    , Pitch, PitchError(..), ControlValMap
    , pitch, apply, add_control, eval_pitch, eval_note, pitch_nn, pitch_note
) where
import Prelude hiding (take, last, null)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Vector as V

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TimeVector as TimeVector

import qualified Derive.BaseTypes as Score
import Derive.BaseTypes (Pitch(..), PitchCall, ControlValMap, PitchError(..))
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


-- | A pitch signal is similar to a 'Signal.Control', except that its values
-- are 'Pitch'es instead of plain floating point values.
data Signal = Signal {
    -- | The set of transposer signals for this scale, as documented in
    -- 'Derive.Scale.scale_transposers'.
    --
    -- They are stored here because they're needed by 'to_nn'.  I could
    -- store them separately, e.g. in the 'Score.Event' alongside the
    -- event_pitch, but the scale at event creation time is not guaranteed to
    -- be the same, so the safest thing to do is keep it with the signal
    -- itself.
    sig_transposers :: !(Set.Set Score.Control)

    -- | This is not technically needed, but it seems useful to be able to
    -- see the scale of a signal.  If signals of different scales are
    -- combined it will be just one of them.  TODO if multi-scale signals is
    -- ever something I do a lot it might behoove me to make this a Set.
    , sig_scale_id :: !Pitch.ScaleId
    , sig_vec :: !(TimeVector.Boxed Pitch)
    } deriving (Read, Show)

sig_scale :: Signal -> Scale
sig_scale sig = Scale (sig_scale_id sig) (sig_transposers sig)

modify_vector :: (TimeVector.Boxed Pitch -> TimeVector.Boxed Pitch)
    -> Signal -> Signal
modify_vector f sig = sig { sig_vec = f (sig_vec sig) }

-- | Signal can't take a Scale because that would be a circular import.
-- Fortunately it only needs a few fields.  However, because of the
-- circularity, the Scale.Scale -> PitchSignal.Scale constructor is in
-- "Derive.Derive".
data Scale = Scale Pitch.ScaleId (Set.Set Score.Control) deriving (Show)

instance Monoid.Monoid Signal where
    mempty = Signal mempty Pitch.empty_scale mempty
    mappend s1 s2 = Monoid.mconcat [s1, s2]
    mconcat [] = mempty
    mconcat sigs = Signal (mconcat (map sig_transposers sigs))
        (fromMaybe Pitch.empty_scale
            (List.find (/=Pitch.empty_scale) (map sig_scale_id sigs)))
        (TimeVector.merge (map sig_vec sigs))

instance DeepSeq.NFData Signal where
    rnf (Signal _ _ v) = v `seq` ()

instance Pretty.Pretty Signal where
    format (Signal _ scale_id vec) = Pretty.fsep
        [Pretty.text "Pitch", Pretty.format scale_id, Pretty.format vec]

constant :: Scale -> Pitch -> Signal
constant (Scale scale_id transposers) =
    Signal transposers scale_id . TimeVector.constant

signal :: Scale -> [(RealTime, Pitch)] -> Signal
signal (Scale scale_id transposers) =
    Signal transposers scale_id . TimeVector.signal

unsignal :: Signal -> [(RealTime, Pitch)]
unsignal = TimeVector.unsignal . sig_vec

-- | Flatten a signal to a non-transposeable Signal.NoteNumber.
to_nn :: Signal -> (Signal.NoteNumber, [PitchError])
to_nn sig = (Signal.signal nns, Set.toList errs)
    where
    (errs, nns) = split (unsignal sig)
    split [] = (Set.empty, [])
    split ((x, pitch) : rest) = case pitch_nn pitch of
            Left err -> (Set.insert err errs, nns)
            Right (Pitch.NoteNumber nn) -> (errs, (x, nn) : nns)
        where (errs, nns) = split rest

type ControlMap = Map.Map Score.Control Score.TypedControl

-- | Resample the signal according to the 'sig_transposers' and apply the
-- given controls to the signal.
--
-- Controls are /added/ so if this is not correct for a given control then
-- this will do the wrong thing.  Transpose signals are probably mostly
-- additive so it'll be ok as long as you only apply transposing signals
-- and only apply the complete ControlMap once at the end (i.e.
-- "Perform.Midi.Convert").
apply_controls :: ControlMap -> Signal -> Signal
apply_controls controls sig
    | V.null (sig_vec sig) = sig
    | otherwise = sig { sig_vec = resampled }
    where
    resampled = TimeVector.sig_op2 initial_controls initial_pitch
        apply
        (sample_controls controls (sig_transposers sig))
        (sig_vec sig)
    TimeVector.Sample start initial_pitch = V.unsafeHead (sig_vec sig)
    initial_controls = controls_at start controls

-- | Sample the ControlMap on the sample points of the given set of controls.
sample_controls :: ControlMap -> Set.Set Score.Control
    -> TimeVector.Boxed ControlValMap
sample_controls controls transposers =
    TimeVector.signal $ zip xs (map (flip controls_at controls) xs)
    where
    xs = Seq.drop_dups id $ Seq.merge_asc_lists id (map xs_of sigs)
    sigs = mapMaybe (\c -> Map.lookup c controls)
        (Set.toList transposers)
    xs_of = map fst . Signal.unsignal . Score.typed_val
    -- If the tsigs are dense, then it's wasteful to keep looking up all
    -- the values instead of stepping along in order, but if the tsigs are
    -- sparse then it's probably more efficient to sample.  I expect in many
    -- cases there will be 0 or 1 transposition values.

-- | 'apply_controls' specialized for a single control.
apply_control :: Score.Control -> Score.TypedControl -> Signal -> Signal
apply_control cont sig = apply_controls (Map.singleton cont sig)

controls_at :: RealTime -> ControlMap -> ControlValMap
controls_at t = Map.map (Signal.at t . Score.typed_val)

-- * signal functions

null :: Signal -> Bool
null = TimeVector.null . sig_vec

at :: RealTime -> Signal -> Maybe Pitch
at x = TimeVector.at x . sig_vec

shift :: RealTime -> Signal -> Signal
shift x = modify_vector (TimeVector.shift x)

last :: Signal -> Maybe (RealTime, Pitch)
last sig
    | V.null (sig_vec sig) = Nothing
    | otherwise = case V.unsafeLast (sig_vec sig) of
        TimeVector.Sample x pitch -> Just (x, pitch)

take :: Int -> Signal -> Signal
take = modify_vector . TimeVector.take

drop_after :: RealTime -> Signal -> Signal
drop_after x = modify_vector (TimeVector.drop_after x)

drop_before :: RealTime -> Signal -> Signal
drop_before x = modify_vector (TimeVector.drop_before x)

-- * Pitch

-- newtype Pitch = Pitch PitchCall
-- type PitchCall = ControlValMap -> Either PitchError Pitch.NoteNumber
-- type ControlValMap = Map.Map Score.Control Signal.Y

pitch :: PitchCall Pitch.NoteNumber -> PitchCall Pitch.Note -> Pitch
pitch = Pitch

-- | Apply controls to a pitch.
apply :: ControlValMap -> Pitch -> Pitch
apply controls (Pitch nn note) = Pitch
    (\controls2 -> nn $! Map.unionWith (+) controls2 controls)
    (\controls2 -> note $! Map.unionWith (+) controls2 controls)

add_control :: Score.Control -> Double -> Pitch -> Pitch
add_control control val (Pitch nn note) = Pitch
    (\controls -> nn $! Map.insertWith (+) control val controls)
    (\controls -> note $! Map.insertWith (+) control val controls)

eval_pitch :: Pitch -> PitchCall Pitch.NoteNumber
eval_pitch (Pitch p _) = p

eval_note :: Pitch -> PitchCall Pitch.Note
eval_note (Pitch _ n) = n

pitch_nn :: Pitch -> Either PitchError Pitch.NoteNumber
pitch_nn p = eval_pitch p Map.empty

pitch_note :: Pitch -> Either PitchError Pitch.Note
pitch_note p = eval_note p Map.empty
