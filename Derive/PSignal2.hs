-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.PSignal2 (
    PSignal, sig_scale_id
    , Scale(Scale), no_scale

    -- * construct / destruct
    , from_pairs, from_segments
    , to_pairs, to_segments
    , constant
    , to_nn
    , unfoldr

    -- * query
    , null, at
    -- , sample_at, before, head, last
    -- , take, drop, drop_while, drop_after, drop_at_after
    -- , drop_before, drop_before_strict, drop_before_at, within

    -- * transform
    , before, after
    , shift
    , apply_controls, apply_control, apply_environ
    , map_y
    -- , prepend

    -- * Pitch
    , Transposed, Pitch
    , RawPitch, PitchConfig(..)
    , symbolic_pitch
    , pitch_scale_id, pitch_transposers
    , pitch_scale, pitch_eval_nn, pitch_eval_note, pitch_config, pitch_controls
    , PitchError(..)
    , pitch, coerce
    , apply_config, apply, add_control, pitch_nn, pitch_note
    -- ** create
    , constant_pitch, nn_pitch
) where
import Prelude hiding (head, take, drop, last, null)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map

import qualified Util.Num as Num
import qualified Util.Segment as Segment
import Util.Segment (Sample(..))
import qualified Util.Seq as Seq

import qualified Derive.BaseTypes as BaseTypes
import Derive.BaseTypes
       (Transposed, Pitch, pitch, coerce, pitch_nn, pitch_note, RawPitch(..),
        Scale(..), PitchConfig(..), PitchError(..))
import qualified Derive.ScoreTypes as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal2 as Signal

import Global
import Types


-- *** TODO move to base types

-- | A pitch signal is similar to a 'Signal.Control', except that its values
-- are 'Pitch'es instead of plain floating point values.
newtype PSignal = PSignal (Segment.Boxed Pitch)
    deriving (Show, Pretty)

_signal :: PSignal -> Segment.Boxed Pitch
_signal (PSignal sig) = sig

instance Monoid PSignal where
    mempty = PSignal Segment.empty
    mappend s1 s2
        | Segment.null (_signal s1) = s2
        | Segment.null (_signal s2) = s1
        | otherwise = mconcat [s1, s2]
    mconcat [] = mempty
    mconcat sigs = PSignal $ Segment.concat (map _signal sigs)

instance DeepSeq.NFData PSignal where
    rnf (PSignal vec) = vec `seq` ()

-- *** end TODO move to base types

-- Signal imported from BaseTypes.

-- | Set of transposers for the signal.  Transposers are documented in
-- 'pscale_transposers'.
--
-- A Signal can contain pitches from multiple scales, though I don't think this
-- should ever happen.  But if it does, the first pitch wins.
sig_transposers :: PSignal -> Set Score.Control
sig_transposers = pscale_transposers . sig_scale

-- | Get the scale id of the signal.
--
-- A PSignal can contain pitches from multiple scales, though I don't think this
-- should ever happen.  But if it does, the first pitch wins.
sig_scale_id :: PSignal -> Pitch.ScaleId
sig_scale_id = pscale_scale_id . sig_scale

sig_scale :: PSignal -> Scale
sig_scale = maybe no_scale (pitch_scale . sy) . Seq.head . Segment.to_samples
    . _signal

modify :: (Segment.Boxed Pitch -> Segment.Boxed Pitch) -> PSignal -> PSignal
modify f = PSignal . f . _signal

no_scale :: Scale
no_scale = Scale "no-scale" mempty

-- * construct / destruct

from_pairs :: [(RealTime, Pitch)] -> PSignal
from_pairs = PSignal . Segment.from_pairs

from_segments :: [Segment.Segment Pitch] -> PSignal
from_segments = PSignal . Segment.from_segments

to_pairs :: PSignal -> [(RealTime, Pitch)]
to_pairs = Segment.to_pairs . _signal

to_segments :: PSignal -> [Segment.Segment Pitch]
to_segments = Segment.to_segments . _signal

constant :: Pitch -> PSignal
constant  = PSignal . Segment.constant

-- | Flatten a signal to a non-transposeable Signal.NoteNumber.
-- TODO I could probably avoid the intermediate list
to_nn :: PSignal -> (Signal.NoteNumber, [PitchError])
to_nn = extract . Either.partitionEithers . map eval . to_pairs
    where
    extract (errs, nns) = (Signal.from_pairs nns, Seq.unique_sort errs)
    eval (x, pitch) = case pitch_nn (coerce pitch) of
        Left err -> Left err
        Right (Pitch.NoteNumber nn) -> Right (x, nn)

unfoldr :: (state -> Maybe ((RealTime, Pitch), state)) -> state -> PSignal
unfoldr gen state = PSignal $ Segment.unfoldr gen state

-- * query

null :: PSignal -> Bool
null = Segment.null . _signal

at :: RealTime -> PSignal -> Maybe Pitch
at x = Segment.at_interpolate interpolate x . _signal

-- | A pitch interpolated a certain distance between two other pitches.
interpolate :: RealTime -> Pitch -> RealTime -> Pitch -> RealTime -> Pitch
interpolate x1 p1 x2 p2 x = Pitch
    { pitch_eval_nn = nn
    , pitch_eval_note = note
    , pitch_scale = pitch_scale p1
    , pitch_config = mempty
    }
    where
    nn config = do
        p1_nn <- pitch_nn $ coerce $ apply_config config p1
        p2_nn <- pitch_nn $ coerce $ apply_config config p2
        return $ Num.scale p1_nn p2_nn $
            Pitch.NoteNumber $ RealTime.to_seconds $ Num.normalize x1 x2 x
    note = pitch_eval_note $ coerce $ if x < x2 then p1 else p2

{-
sample_at :: RealTime -> PSignal -> Maybe (RealTime, Pitch)
sample_at x = Segment.sample_at x . _signal

-- | Find the last pitch before the point.
before :: RealTime -> PSignal -> Maybe (RealTime, Pitch)
before x = fmap Segment.to_pair . Segment.before x . _signal

head :: PSignal -> Maybe (RealTime, Pitch)
head = fmap Segment.to_pair . Segment.head . _signal

last :: PSignal -> Maybe (RealTime, Pitch)
last = fmap Segment.to_pair . Segment.last . _signal

take :: Int -> PSignal -> PSignal
take = modify . Segment.take

drop :: Int -> PSignal -> PSignal
drop = modify . Segment.drop

drop_while :: (Sample Pitch -> Bool) -> PSignal -> PSignal
drop_while f = modify (V.dropWhile f)

drop_after :: RealTime -> PSignal -> PSignal
drop_after = modify . Segment.drop_after

drop_at_after :: RealTime -> PSignal -> PSignal
drop_at_after = modify . Segment.drop_at_after

drop_before :: RealTime -> PSignal -> PSignal
drop_before = modify . Segment.drop_before

drop_before_strict :: RealTime -> PSignal -> PSignal
drop_before_strict = modify . Segment.drop_before_strict

drop_before_at :: RealTime -> PSignal -> PSignal
drop_before_at = modify . Segment.drop_before_at

within :: RealTime -> RealTime -> PSignal -> PSignal
within start end = modify $ Segment.within start end
-}

-- * transform

before, after :: RealTime -> PSignal -> PSignal
before x = modify $ Segment.before x
after x = modify $ Segment.after x

shift :: RealTime -> PSignal -> PSignal
shift x = modify (Segment.shift x)

type ControlMap = Map Score.Control Score.TypedControl

-- | Resample the signal according to the 'sig_transposers' and apply the
-- given controls to the signal.
--
-- Controls are /added/ so if this is not correct for a given control then
-- this will do the wrong thing.  Transpose signals are probably mostly
-- additive so it'll be ok as long as you only apply transposing signals
-- and only apply the complete ControlMap once at the end (i.e.
-- "Perform.Midi.Convert").
apply_controls :: ControlMap -> PSignal -> PSignal
apply_controls = undefined

{-
apply_controls :: ControlMap -> PSignal -> PSignal
apply_controls controls sig
    | Just (x, _) <- head sig = sig { _signal = resample x }
    | otherwise = sig
    where
    resample x = Segment.sig_op_poly initial_controls initial_pitch
        (\vmap -> coerce . apply vmap)
        (sample_controls x (trim x controls) (sig_transposers sig))
        (_signal sig)
    trim = fmap . fmap . Signal.drop_before
    Sample start initial_pitch = V.unsafeHead (_signal sig)
    initial_controls = controls_at start controls

-- | Sample the ControlMap on the sample points of the given set of controls.
sample_controls :: RealTime -> ControlMap -> Set Score.Control
    -> Segment.Boxed Score.ControlValMap
sample_controls start controls transposers =
    Segment.signal $ zip xs (map (flip controls_at controls) xs)
    where
    xs = Seq.drop_dups id $ Seq.merge_lists id (map xs_of sigs)
    sigs = mapMaybe (\c -> Map.lookup c controls) (Set.toList transposers)
    -- dropWhile (<start) because the xs may start before the start time to
    -- get initial values, but I don't want to extend the pitch signal to
    -- before where it originally started.  This would cause a problem when
    -- flattening a PSignal for the track signal, where a transpose signal
    -- could cause every pitch signal fragment to start at 0.
    xs_of = dropWhile (<start) . map fst . Signal.unsignal . Score.typed_val
    -- If the tsigs are dense, then it's wasteful to keep looking up all
    -- the values instead of stepping along in order, but if the tsigs are
    -- sparse then it's probably more efficient to sample.  I expect in many
    -- cases there will be 0 or 1 transposition values.
-}

-- | 'apply_controls' specialized for a single control.
apply_control :: Score.Control -> Score.TypedControl -> PSignal -> PSignal
apply_control cont sig = apply_controls (Map.singleton cont sig)

-- | Apply an environ to all the pitches in the signal.  Unlike
-- 'apply_controls', this doesn't have to resample the signal.
apply_environ :: BaseTypes.Environ -> PSignal -> PSignal
apply_environ env =
    modify $ Segment.map_y $ apply_config (PitchConfig env mempty)

-- -- | Not exported, use the one in Derive.Score instead.
-- controls_at :: RealTime -> ControlMap -> Score.ControlValMap
-- controls_at t = Map.map (Signal.at t . Score.typed_val)

map_y :: (Pitch -> Pitch) -> PSignal -> PSignal
map_y = modify . Segment.map_y

-- prepend :: PSignal -> PSignal -> PSignal
-- prepend s1 s2 = PSignal $ Segment.prepend (_signal s1) (_signal s2)


-- * Pitch

-- | This is like pretty for pitch, but just shows the symbolic note name.
symbolic_pitch :: RawPitch a -> Text
symbolic_pitch = either showt Pitch.note_text . pitch_note . coerce

pitch_scale_id :: RawPitch a -> Pitch.ScaleId
pitch_scale_id = pscale_scale_id . pitch_scale

pitch_transposers :: Pitch -> Set Score.Control
pitch_transposers = pscale_transposers . pitch_scale

pitch_controls :: PitchConfig -> Score.ControlValMap
pitch_controls (PitchConfig _ controls) = controls

-- | Apply a config to a pitch.
apply_config :: PitchConfig -> RawPitch a -> RawPitch b
apply_config c pitch = pitch { pitch_config = c <> pitch_config pitch }

-- | Apply just the controls part of a config to a pitch.
apply :: Score.ControlValMap -> Pitch -> Transposed
apply controls = apply_config (PitchConfig mempty controls)

add_control :: Score.Control -> Double -> RawPitch a -> RawPitch a
add_control control val pitch =
    pitch { pitch_config = config <> pitch_config pitch }
    where config = PitchConfig mempty (Map.singleton control val)

-- ** create

-- | Create a Pitch that only emits the given NoteNumber, and doesn't respond
-- to transposition.
constant_pitch :: Pitch.ScaleId -> Pitch.Note -> Pitch.NoteNumber -> Pitch
constant_pitch scale_id note nn =
    pitch (Scale scale_id mempty) (const (Right nn)) (const (Right note)) mempty

-- | Like 'constant_pitch', but easier to use, but uses no_scale, which means
-- the result will be unparseable.
nn_pitch :: Pitch.NoteNumber -> Pitch
nn_pitch nn =
    pitch no_scale (const (Right nn)) (const (Right (Pitch.Note (pretty nn))))
        mempty
