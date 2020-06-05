-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is like the @sample@ patch, but with special support for breaking up
-- beats and naming them.
module Synth.Sampler.Patch.Break (patches) where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Cmd.PhysicalKey as PhysicalKey

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


patches :: [Patch.DbPatch]
patches = map Patch.DbPatch
    [ make "medeski" medeski
    ]
    where
    make name (perMeasure, beats) = (Patch.patch ("break-" <> name))
        { Patch._dir = dir
        , Patch._convert = convert sample
        , Patch._karyaPatch =
            -- (ImInst.common#Common.call_map #= make_call_map strokes)
            ImInst.code #= (call_code <> drum_code) $
            ImInst.make_patch $ Im.Patch.patch
                { Im.Patch.patch_controls = mconcat
                    [ Control.supportDyn
                    , Control.supportSampleStartOffset
                    , Control.support pitchOffset "Pitch offset, in nn."
                    ]
                }
        }
        where
        sample = untxt $ name <> ".wav"
        call_code = ImInst.note_generators $ ("n", break Nothing) :
            [ (Expr.Symbol stroke, break (Just frame))
            | (_, frame, stroke) <- beats
            ]
        drum_code = ImInst.cmd $ CUtil.insert_expr thru (Map.fromList strokes)
        -- TODO incremental should be based on perMeasure and overall range,
        -- or just hardcoded per break.
        strokes = makeStrokes (1/2) perMeasure beats
        thru = Util.imThruFunction dir (convert sample)
        beatMap = makeBeatMap perMeasure beats
        break = c_break beatMap perMeasure
    dir = "break"

physicalKeys :: [(Measure, Beat)] -> [((Measure, Beat), Char)]
physicalKeys beats = concat
    [ zip (map (measure,) beats) keys
    | (keys, (measure, beats)) <- zip byOctave byMeasure
    ]
    where
    byMeasure = Seq.group_fst beats
    byOctave = map (map fst) $ Seq.group_adjacent (Pitch.pitch_octave . snd) $
        filter ((>=0) . Pitch.pitch_accidentals . snd) $
        Seq.sort_on snd $ Map.toList $ PhysicalKey.pitch_map

-- | I can fit 8 per octave, and with black notes that's 16.
makeStrokes :: Beat -> Beat -> [((Measure, Beat), Frame, Stroke)]
    -> [(Char, DeriveT.Expr)]
makeStrokes increment perMeasure beats =
    [(key, toExpr mbeat) | (mbeat, key) <- physicalKeys allBeats]
    -- Since I want to interpret octaves, would it be easier to use
    -- InputNote then map pitches?
    -- CUtil.insert_expr works by completely replacing kbd entry, so it doesn't
    -- look at the octave control, which is actually usually right.
    -- If I want to reuse it, I'll have to add optional octave support.
    where
    toExpr mbeat = maybe (makeExpr mbeat) (Expr.generator0 . Expr.Symbol) $
        Map.lookup mbeat strokeMap
    inRange = maybe (const False) (\(m, _) -> (<=m)) $ Map.lookupMax strokeMap
    allBeats = takeWhile inRange
        [ (measure, beat)
        | measure <- [1..], beat <- Seq.range' 1 (perMeasure+1) increment
        ]
    strokeMap = Map.fromList [(beat, stroke) | (beat, _, stroke) <- beats]

makeExpr :: (Measure, Beat) -> DeriveT.Expr
makeExpr (measure, beat) = Expr.generator $ Expr.call "n" [DeriveT.num num]
    where num = fromIntegral measure + realToFrac (beat / 10)
    -- 1.25 -> 0.125

-- | Take a beat arg or named start time, and look up the corresponding start
-- offset.
c_break :: Map Beat Frame -> Beat -> Maybe Frame -> Derive.Generator Derive.Note
c_break beatMap perMeasure mbFrame =
    Derive.generator Module.instrument "break" mempty doc $
    Sig.call ((,,)
        <$> maybe (Left <$> beat_arg) (pure . Right) mbFrame
        <*> Sig.defaulted "pre" (Typecheck.score 0)
            "Move note start back by this much, along with the offset."
        <*> Sig.defaulted "pitch" 0 "Pitch offset."
    ) $ \(beatOrFrame, pre, pitch) -> Sub.inverting $ \args -> do
        frame <- either (lookupBeat beatMap) return beatOrFrame
        let (start, dur) = Args.extent args
        pre <- Call.score_duration start pre
        rpre <- Call.real_duration start pre
        Derive.with_constant_control
                (Controls.from_shared Control.sampleStartOffset)
                (fromIntegral frame - fromIntegral (AUtil.toFrames rpre)) $
            Derive.with_constant_control
                (Controls.from_shared pitchOffset) pitch $
            Derive.place (start - pre) (dur + pre) Call.note
    where
    beat_arg = Sig.required "beat" "Offset measure.beat. This abuses\
        \ decimal notation: 4 -> (1, 4), 4.1 -> (4, 1), 4.15 -> (4, 1.5)."
    doc = "Play a sample at a certain offset."

    lookupBeat beatMap beatFraction =
        Derive.require
            ("(measure, beat) out of range: " <> pretty (measure, beat)
                <> " " <> pretty b <> " " <> pretty (Map.keys beatMap)) $
            findFrame beatMap b
        where
        (measure, beat) = decodeBeat beatFraction
        b = max 0 (fromIntegral (measure-1)) * perMeasure + beat

-- |
-- 6.0 -> (1, 6)
-- 1.6 -> (1, 6)
-- 6.1 -> (6, 1)
-- 6.125 -> (6, 1.25)
decodeBeat :: Beat -> (Measure, Beat)
decodeBeat beatFraction
    | beat == 0 = (1, fromIntegral measure)
    | otherwise =
        ( if measure == 0 then 1 else measure
        , if beat == 0 then 1 else beat * 10
        )
    where
    (measure, beat) = properFraction beatFraction

pitchOffset :: Control.Control
pitchOffset = "pitch-offset"

convert :: FilePath -> Note.Note -> Patch.ConvertM Sample.Sample
convert filename note = return $ (Sample.make filename)
    { Sample.envelope = Util.dynEnvelope minDyn 0.15 note
    , Sample.offset = offset
    , Sample.ratios = Signal.constant $
        Sample.pitchToRatio 60 (60 + Pitch.nn pitch)
    }
    where
    offset = floor $ Note.initial0 Control.sampleStartOffset note
    pitch = Note.initial0 pitchOffset note

minDyn :: Signal.Y
minDyn = 0.5

findFrame :: Map Beat Frame -> Beat -> Maybe Frame
findFrame beats beat = case at of
    Just frame -> Just frame
    Nothing -> case (Map.lookupMax below, Map.lookupMin above) of
        (Just (beat0, frame0), Just (beat1, frame1)) -> Just $
            round $ Num.scale (Num.i2d frame0) (Num.i2d frame1) $
                Num.normalize (d beat0) (d beat1) (d beat)
        _ -> Nothing
    where
    (below, at, above) = Map.splitLookup beat beats
    d = realToFrac :: Beat -> Double


-- * implementation

type Measure = Int
type Beat = Ratio.Ratio Int
type Frame = Int
type Stroke = Text

addMeasures :: [(Beat, Frame, Stroke)] -> [((Measure, Beat), Frame, Stroke)]
addMeasures = concatMap add . zip [1..]
    . Seq.split_between (\(a, _, _) (b, _, _) -> b < a)
    where
    add (m, beats) =
        [((m, beat), frame, stroke) | (beat, frame, stroke) <- beats]

-- | Make Strokes unique by labelling them according to position in the
-- measure.
labelStrokes :: [((Measure, Beat), frame, Stroke)]
    -> [((Measure, Beat), frame, Stroke)]
labelStrokes = map $ \((measure, beat), frame, stroke) ->
    ((measure, beat), frame, stroke <> suffix measure beat)
    where
    suffix measure beat = showt measure
        <> if beat == 1 then "" else "-" <> encode beat
    encode = Text.dropWhileEnd (=='0') . Text.replace "." "" . showt
        . (realToFrac :: Beat -> Double)

makeBeatMap :: Beat -> [((Measure, Beat), Frame, stroke)] -> Map Beat Frame
makeBeatMap perMeasure beats = Map.fromList
    [ (fromIntegral (measure-1) * perMeasure + beat, frame)
    | ((measure, beat), frame, _) <- beats
    ]

-- 0.433202947845805 per beat, so 138.5032126359318 bpm, say 138.5
-- tempo 1 is 60bpm, 2 = 120bpm, so 60n = 138.5 = 138.5/60
medeski :: (Beat, [((Measure, Beat), Frame, Stroke)])
medeski = (8,) $ labelStrokes $ addMeasures
    [ (1,   0,      bd)
    , (2,   20296,  sn)
    , (3.5, 49024,  bd)
    , (4.5, 68024,  sn)
    , (6,   95982,  sn)
    , (7.5, 124482, bd)
    , (8,   133878, sn)
    , (8.5, 143584, bd)

    , (1,   152834, bd)
    , (2,   172572, sn)
    , (3.5, 201002, bd)
    , (4.5, 220438, sn)
    , (6,   248210, sn)
    , (7.5, 275986, bd)

    , (1,   303488, bd)
    , (2,   322988, sn)
    , (3.5, 351232, bd)
    , (4.5, 370402, sn)
    , (6,   398044, sn)
    , (7.5, 426150, bd)
    , (8,   435612, sn)
    , (8.5, 445314, bd)

    , (1,   454530, bd)
    , (2,   473836, sn)
    , (3.5, 502050, bd)
    , (4.5, 521034, sn)
    , (5,   529638, bd)

    , (1,   604380, bd)
    ]
    where
    bd = "bd"
    sn = "sn"
