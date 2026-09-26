-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.PSignal (
    PSignal, sigScaleId
    , Scale(..), noScale

    -- * construct / destruct
    , fromPairs, fromSample, fromSegments
    , toPairs, toSegments
    , constant
    , constantVal
    , pitchVal
    , prepend
    , ErrorText
    , toNn
    , unfoldr

    -- * query
    , null
    , at, atNegative, segmentAt
    , interpolate
    , head, last

    -- * transform
    , dropAfter, clipAfter
    , dropBefore, clipBefore
    , shift
    , applyControls, applyControl, applyEnviron
    , mapYLinear

    -- ** hacks
    , dropDiscontinuityAt

    -- * Pitch
    , Transposed, Pitch
    , RawPitch, PitchConfig(..)
    , symbolicPitch
    , pitchScaleId, pitchTransposers
    , pitchScale, pitchEvalNn, pitchEvalNote, pitchConfig, pitchControls
    , PitchError(..)
    , pitch, coerce
    , applyConfig, apply, addControl, pitchNn, pitchNote
    -- ** create
    , constantPitch, nnPitch
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
     Scale(..), Transposed, _signal, coerce, interpolate, pitch, pitchNn,
     pitchNote)
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
sigTransposers :: PSignal -> Set ScoreT.Control
sigTransposers = pscaleTransposers . sigScale

-- | Get the scale id of the signal.
--
-- A PSignal can contain pitches from multiple scales, though I don't think this
-- should ever happen.  But if it does, the first pitch wins.
sigScaleId :: PSignal -> Pitch.ScaleId
sigScaleId = pscaleScaleId . sigScale

sigScale :: PSignal -> Scale
sigScale = maybe noScale (pitchScale . sy) . Lists.head . Segment.toSamples
    . _signal

modify ::
    (Segment.Signal Vector.Vector Pitch -> Segment.Signal Vector.Vector Pitch)
    -> PSignal -> PSignal
modify f = PSignal . f . _signal

noScale :: Scale
noScale = Scale "no-scale" mempty

-- * construct / destruct

fromPairs :: [(RealTime, Pitch)] -> PSignal
fromPairs = PSignal . Segment.fromPairs

fromSample :: RealTime -> Pitch -> PSignal
fromSample x y = fromPairs [(x, y)]

fromSegments :: [Segment.Segment Pitch] -> PSignal
fromSegments = PSignal . Segment.fromSegments

toPairs :: PSignal -> [(RealTime, Pitch)]
toPairs = Segment.toPairs . _signal

toSamples :: PSignal -> [Segment.Sample Pitch]
toSamples = Segment.toSamples . _signal

toSegments :: PSignal -> [Segment.Segment Pitch]
toSegments = Segment.toSegments . _signal

constant :: Pitch -> PSignal
constant = PSignal . Segment.constant

constantVal :: PSignal -> Maybe Pitch
constantVal = Segment.constantVal . _signal

pitchVal :: DeriveT.Val -> Maybe Pitch
pitchVal = \case
    DeriveT.VPSignal sig -> constantVal sig
    _ -> Nothing

prepend :: PSignal -> PSignal -> PSignal
prepend sig1 sig2 = PSignal $
    Segment.prepend Nothing interpolate (_signal sig1) (_signal sig2)

type ErrorText = Text

-- | Flatten a signal to a non-transposeable Signal.NoteNumber.
-- TODO I could probably avoid the intermediate list
toNn :: PSignal -> (Signal.NoteNumber, [(RealTime, ErrorText)])
toNn = extract . Either.partitionEithers . map eval . toPairs
    where
    extract (errs, nns) = (Signal.fromPairs nns, Lists.uniqueSort errs)
    eval (x, pitch) = case pitchNn (coerce pitch) of
        Left err -> Left (x, DeriveT.detailedError pitch err)
        Right (Pitch.NoteNumber nn) -> Right (x, nn)

unfoldr :: (state -> Maybe ((RealTime, Pitch), state)) -> state -> PSignal
unfoldr gen state = PSignal $ Segment.unfoldr gen state

-- * query

null :: PSignal -> Bool
null = Segment.null . _signal

at :: PSignal -> RealTime -> Maybe Pitch
at = Segment.at interpolate . _signal

atNegative :: PSignal -> RealTime -> Maybe Pitch
atNegative = Segment.atNegative interpolate . _signal

segmentAt :: PSignal -> RealTime -> Maybe (Segment.Segment Pitch)
segmentAt = Segment.segmentAt . _signal

head, last :: PSignal -> Maybe (RealTime, Pitch)
head = Segment.head . _signal
last = Segment.last . _signal


-- * transform

dropAfter, dropBefore :: RealTime -> PSignal -> PSignal
dropAfter x = modify $ Segment.dropAfter x
dropBefore x = modify $ Segment.dropBefore x

clipAfter, clipBefore :: RealTime -> PSignal -> PSignal
clipAfter x = modify $ Segment.clipAfter interpolate x
clipBefore x = modify $ Segment.clipBefore interpolate x

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
{-# SCC applyControls #-}
applyControls :: ControlMap -> PSignal -> PSignal
applyControls cmap psig = case Lists.head (toPairs psig) of
    Nothing -> mempty
    Just (start, _) -> make1 start
    where
    make1 start = fromPairs $ drop1 $
        mapMaybe make $ zip3 xs pitchResamples controlResamples
        where
        -- Discard transpose samples before the pitch starts.  The
        -- Signal.at_after below should ensure there is at most one of these,
        -- plus one for the transition from zero added by
        -- 'Segment.add_zero_transition'.
        make (_, Nothing, _) = Nothing
        make (x, Just pitch, controls) =
            Just $ (x,) $ coerce $ apply cmap2 pitch
            where
            cmap2 = Map.fromAscList (zip controlNames controls)
                <> controlsAt x nonTransposers
        controlResamples
            | List.null controlSamples = replicate (length xs) []
            | otherwise = Lists.rotate $
                map (Segment.resampleNum xs) controlSamples
        pitchResamples =
            Segment.resampleMaybe interpolate xs $ toSamples psig
        controlSamples =
            map (Segment.addZeroTransition 0 . Signal.toSamples
                    . Signal.dropBefore start)
                controlSignals
        ((controlNames, controlSignals), nonTransposers) =
            unzipControls psig cmap

        xs = Segment.sampleXs (pitchXs : controlXs)
        pitchXs = map Segment.sx $ toSamples psig
        controlXs = map (map Signal.sx) controlSamples
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
unzipControls :: PSignal -> ControlMap
    -> (([ScoreT.Control], [Signal.Control]), ControlMap)
unzipControls psig cmap =
    ( second (map ScoreT.valOf) (unzip transposers)
    , Map.fromAscList nonTransposers
    )
    where
    (transposers, nonTransposers) =
        List.partition ((`Set.member` sigTransposers psig) . fst) $
        Map.toAscList cmap

-- | Not exported, use the one in Derive.Score instead.
controlsAt :: RealTime -> ControlMap -> Map ScoreT.Control Signal.Y
controlsAt t = Map.map ((`Signal.at` t) . ScoreT.valOf)

-- | 'apply_controls' specialized for a single control.
applyControl :: ScoreT.Control -> ScoreT.Typed Signal.Control
    -> PSignal -> PSignal
applyControl cont sig = applyControls (Map.singleton cont sig)

-- | Apply an environ to all the pitches in the signal.  Unlike
-- 'apply_controls', this doesn't have to resample the signal.
{-# SCC applyEnviron #-}
applyEnviron :: DeriveT.Environ -> PSignal -> PSignal
applyEnviron env =
    modify $ Segment.mapYLinear $ applyConfig (PitchConfig env mempty)

mapYLinear :: (Pitch -> Pitch) -> PSignal -> PSignal
mapYLinear = modify . Segment.mapYLinear

-- ** hacks

dropDiscontinuityAt :: RealTime -> PSignal -> PSignal
dropDiscontinuityAt x = modify $ Segment.dropDiscontinuityAt x


-- * Pitch

-- | This is like pretty for pitch, but just shows the symbolic note name.
symbolicPitch :: RawPitch a -> Text
symbolicPitch = either showt Pitch.noteText . pitchNote . coerce

pitchScaleId :: RawPitch a -> Pitch.ScaleId
pitchScaleId = pscaleScaleId . pitchScale

pitchTransposers :: Pitch -> Set ScoreT.Control
pitchTransposers = pscaleTransposers . pitchScale

pitchControls :: PitchConfig -> Map ScoreT.Control Signal.Y
pitchControls (PitchConfig _ controls) = controls

-- | Apply a config to a pitch.
applyConfig :: PitchConfig -> RawPitch a -> RawPitch b
applyConfig c pitch = pitch { pitchConfig = c <> pitchConfig pitch }

-- | Apply just the controls part of a config to a pitch.
apply :: Map ScoreT.Control Signal.Y -> Pitch -> Transposed
apply controls
    | Map.null controls = coerce
    | otherwise = applyConfig (PitchConfig mempty controls)

addControl :: ScoreT.Control -> Double -> RawPitch a -> RawPitch a
addControl control val pitch =
    pitch { pitchConfig = config <> pitchConfig pitch }
    where config = PitchConfig mempty (Map.singleton control val)

-- ** create

-- | Create a Pitch that only emits the given NoteNumber, and doesn't respond
-- to transposition.
constantPitch :: Pitch.ScaleId -> Pitch.Note -> Pitch.NoteNumber -> Pitch
constantPitch scaleId note nn =
    pitch (Scale scaleId mempty) (const (Right nn)) (const (Right note)) mempty

-- | Like 'constant_pitch', but easier to use, but uses no_scale, which means
-- the result will be unparseable.
nnPitch :: Pitch.NoteNumber -> Pitch
nnPitch nn =
    pitch noScale (const (Right nn)) (const (Right (Pitch.Note (pretty nn))))
        mempty
