-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.PSignal (
    PSignal, sig_scale_id
    , Scale(..), no_scale

    -- * construct / destruct
    , from_pairs, from_sample, from_segments
    , to_pairs, to_segments
    , constant
    , constant_val
    , pitch_val
    , prepend
    , ErrorText
    , to_nn
    , unfoldr

    -- * query
    , null
    , at, at_negative, segment_at
    , interpolate
    , head, last

    -- * transform
    , drop_after, clip_after
    , drop_before, clip_before
    , shift
    , apply_controls, apply_control, apply_environ
    , map_y_linear

    -- ** hacks
    , drop_discontinuity_at

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
import           Prelude hiding (head, last, null)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified Util.Segment as Segment
import           Util.Segment (Sample(..))

import qualified Derive.DeriveT as DeriveT
import           Derive.DeriveT
    (PSignal(..), Pitch, PitchConfig(..), PitchError(..), RawPitch(..),
     Scale(..), Transposed, _signal, coerce, interpolate, pitch, pitch_nn,
     pitch_note)
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global
import           Types


-- Signal imported from DeriveT.

-- | Set of transposers for the signal.  Transposers are documented in
-- 'pscale_transposers'.
--
-- A Signal can contain pitches from multiple scales, though I don't think this
-- should ever happen.  But if it does, the first pitch wins.
sig_transposers :: PSignal -> Set ScoreT.Control
sig_transposers = pscale_transposers . sig_scale

-- | Get the scale id of the signal.
--
-- A PSignal can contain pitches from multiple scales, though I don't think this
-- should ever happen.  But if it does, the first pitch wins.
sig_scale_id :: PSignal -> Pitch.ScaleId
sig_scale_id = pscale_scale_id . sig_scale

sig_scale :: PSignal -> Scale
sig_scale = maybe no_scale (pitch_scale . sy) . Lists.head . Segment.to_samples
    . _signal

modify ::
    (Segment.Signal Vector.Vector Pitch -> Segment.Signal Vector.Vector Pitch)
    -> PSignal -> PSignal
modify f = PSignal . f . _signal

no_scale :: Scale
no_scale = Scale "no-scale" mempty

-- * construct / destruct

from_pairs :: [(RealTime, Pitch)] -> PSignal
from_pairs = PSignal . Segment.from_pairs

from_sample :: RealTime -> Pitch -> PSignal
from_sample x y = from_pairs [(x, y)]

from_segments :: [Segment.Segment Pitch] -> PSignal
from_segments = PSignal . Segment.from_segments

to_pairs :: PSignal -> [(RealTime, Pitch)]
to_pairs = Segment.to_pairs . _signal

to_samples :: PSignal -> [Segment.Sample Pitch]
to_samples = Segment.to_samples . _signal

to_segments :: PSignal -> [Segment.Segment Pitch]
to_segments = Segment.to_segments . _signal

constant :: Pitch -> PSignal
constant = PSignal . Segment.constant

constant_val :: PSignal -> Maybe Pitch
constant_val = Segment.constant_val . _signal

pitch_val :: DeriveT.Val -> Maybe Pitch
pitch_val = \case
    DeriveT.VPSignal sig -> constant_val sig
    _ -> Nothing

prepend :: PSignal -> PSignal -> PSignal
prepend sig1 sig2 = PSignal $
    Segment.prepend Nothing interpolate (_signal sig1) (_signal sig2)

type ErrorText = Text

-- | Flatten a signal to a non-transposeable Signal.NoteNumber.
-- TODO I could probably avoid the intermediate list
to_nn :: PSignal -> (Signal.NoteNumber, [(RealTime, ErrorText)])
to_nn = extract . Either.partitionEithers . map eval . to_pairs
    where
    extract (errs, nns) = (Signal.from_pairs nns, Lists.uniqueSort errs)
    eval (x, pitch) = case pitch_nn (coerce pitch) of
        Left err -> Left (x, DeriveT.detailed_error pitch err)
        Right (Pitch.NoteNumber nn) -> Right (x, nn)

unfoldr :: (state -> Maybe ((RealTime, Pitch), state)) -> state -> PSignal
unfoldr gen state = PSignal $ Segment.unfoldr gen state

-- * query

null :: PSignal -> Bool
null = Segment.null . _signal

at :: PSignal -> RealTime -> Maybe Pitch
at = Segment.at interpolate . _signal

at_negative :: PSignal -> RealTime -> Maybe Pitch
at_negative = Segment.at_negative interpolate . _signal

segment_at :: PSignal -> RealTime -> Maybe (Segment.Segment Pitch)
segment_at = Segment.segment_at . _signal

head, last :: PSignal -> Maybe (RealTime, Pitch)
head = Segment.head . _signal
last = Segment.last . _signal


-- * transform

drop_after, drop_before :: RealTime -> PSignal -> PSignal
drop_after x = modify $ Segment.drop_after x
drop_before x = modify $ Segment.drop_before x

clip_after, clip_before :: RealTime -> PSignal -> PSignal
clip_after x = modify $ Segment.clip_after interpolate x
clip_before x = modify $ Segment.clip_before interpolate x

shift :: RealTime -> PSignal -> PSignal
shift x = modify (Segment.shift x)

type ControlMap = Map ScoreT.Control (ScoreT.Typed Signal.Control)

-- | Resample the signal according to the 'sig_transposers' and apply the given
-- controls to the signal.
--
-- Controls are /added/ so if this is not correct for a given control then
-- this will do the wrong thing.  Transpose signals should be additive so it'll
-- be ok as long as you only apply transposing signals and only apply the
-- complete ControlMap once at the end (i.e. "Perform.Midi.Convert").
{-# SCC apply_controls #-}
apply_controls :: ControlMap -> PSignal -> PSignal
apply_controls cmap psig = case Lists.head (to_pairs psig) of
    Nothing -> mempty
    Just (start, _) -> make1 start
    where
    make1 start = from_pairs $ drop1 $
        mapMaybe make $ zip3 xs pitch_resamples control_resamples
        where
        -- Discard transpose samples before the pitch starts.  The
        -- Signal.at_after below should ensure there is at most one of these,
        -- plus one for the transition from zero added by
        -- 'Segment.add_zero_transition'.
        make (_, Nothing, _) = Nothing
        make (x, Just pitch, controls) =
            Just $ (x,) $ coerce $ apply cmap2 pitch
            where
            cmap2 = Map.fromAscList (zip control_names controls)
                <> controls_at x non_transposers
        control_resamples
            | List.null control_samples = replicate (length xs) []
            | otherwise = Lists.rotate $
                map (Segment.resample_num xs) control_samples
        pitch_resamples =
            Segment.resample_maybe interpolate xs $ to_samples psig
        control_samples =
            map (Segment.add_zero_transition 0 . Signal.to_samples
                    . Signal.drop_before start)
                control_signals
        ((control_names, control_signals), non_transposers) =
            unzip_controls psig cmap

        xs = Segment.sample_xs (pitch_xs : control_xs)
        pitch_xs = map Segment.sx $ to_samples psig
        control_xs = map (map Signal.sx) control_samples
    -- If the control and pitch starts at the same place, I'll get an extra
    -- pre-transposed pitch.  It's just confusing clutter, especially if the
    -- transpose is invalid, at which point I'm just left with the original
    -- pitch.
    drop1 ((x1, _) : xs@((x2, _) : _)) | x1 == x2 = xs
    drop1 xs = xs

-- | Separate transposing from non-transposing controls.
--
-- This discards the ScoreT.Type, since 'apply' doesn't use that.  The
-- usual type distinctions like chromatic or diatonic instead get separate
-- controls.
unzip_controls :: PSignal -> ControlMap
    -> (([ScoreT.Control], [Signal.Control]), ControlMap)
unzip_controls psig cmap =
    ( second (map ScoreT.val_of) (unzip transposers)
    , Map.fromAscList non_transposers
    )
    where
    (transposers, non_transposers) =
        List.partition ((`Set.member` sig_transposers psig) . fst) $
        Map.toAscList cmap

-- | Not exported, use the one in Derive.Score instead.
controls_at :: RealTime -> ControlMap -> Map ScoreT.Control Signal.Y
controls_at t = Map.map ((`Signal.at` t) . ScoreT.val_of)

-- | 'apply_controls' specialized for a single control.
apply_control :: ScoreT.Control -> ScoreT.Typed Signal.Control
    -> PSignal -> PSignal
apply_control cont sig = apply_controls (Map.singleton cont sig)

-- | Apply an environ to all the pitches in the signal.  Unlike
-- 'apply_controls', this doesn't have to resample the signal.
{-# SCC apply_environ #-}
apply_environ :: DeriveT.Environ -> PSignal -> PSignal
apply_environ env =
    modify $ Segment.map_y_linear $ apply_config (PitchConfig env mempty)

map_y_linear :: (Pitch -> Pitch) -> PSignal -> PSignal
map_y_linear = modify . Segment.map_y_linear

-- ** hacks

drop_discontinuity_at :: RealTime -> PSignal -> PSignal
drop_discontinuity_at x = modify $ Segment.drop_discontinuity_at x


-- * Pitch

-- | This is like pretty for pitch, but just shows the symbolic note name.
symbolic_pitch :: RawPitch a -> Text
symbolic_pitch = either showt Pitch.note_text . pitch_note . coerce

pitch_scale_id :: RawPitch a -> Pitch.ScaleId
pitch_scale_id = pscale_scale_id . pitch_scale

pitch_transposers :: Pitch -> Set ScoreT.Control
pitch_transposers = pscale_transposers . pitch_scale

pitch_controls :: PitchConfig -> Map ScoreT.Control Signal.Y
pitch_controls (PitchConfig _ controls) = controls

-- | Apply a config to a pitch.
apply_config :: PitchConfig -> RawPitch a -> RawPitch b
apply_config c pitch = pitch { pitch_config = c <> pitch_config pitch }

-- | Apply just the controls part of a config to a pitch.
apply :: Map ScoreT.Control Signal.Y -> Pitch -> Transposed
apply controls
    | Map.null controls = coerce
    | otherwise = apply_config (PitchConfig mempty controls)

add_control :: ScoreT.Control -> Double -> RawPitch a -> RawPitch a
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
