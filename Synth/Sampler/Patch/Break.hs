-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
-- | This is like the @sample@ patch, but with special support for breaking up
-- beats and naming them.
module Synth.Sampler.Patch.Break (
    patches
#ifdef TESTING
    , module Synth.Sampler.Patch.Break
#endif
) where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num

import qualified Cmd.Cmd as Cmd
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
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


patches :: [Patch.Patch]
patches = map make allBreaks
    where
    make break = (Patch.patch ("break-" <> break.name))
        { Patch._dir = dir
        , Patch._convert = convert sample break.pitchAdjust
        , Patch._allFilenames = Set.fromList [sample]
        , Patch._effect = Just $ (Patch.effect "comb")
            { Patch._toEffectControl = Map.fromList
                [ ("pitch", "comb-pitch")
                , ("feedback", "comb-feedback")
                ]
            }
        , Patch._karyaPatch =
            ImInst.doc #= Doc.Doc
                ("BPM: " <> Num.showFloat 2 break.bpm) $
            ImInst.code #= (call_code <> drum_code) $
            ImInst.make_patch $ Im.Patch.patch
                { Im.Patch.patch_controls = mconcat
                    [ Control.supportDyn
                    , Control.supportSampleStartOffset
                    , Control.support pitchOffset "Pitch offset, in nn."
                    , Control.supportSampleTimeStretch
                    , Control.supportSamplePitchShift
                    ]
                }
        }
        where
        sample = untxt $ break.name <> ".wav"
        call_code = ImInst.note_generators $ ("n", breakCall Nothing) :
            [ (Expr.Symbol stroke, breakCall (Just frame))
            | (_, stroke, frame) <- break.beats
            ]
        drum_code = ImInst.cmd $ CUtil.insert_expr thru
            (lookupStroke break.increment break.perMeasure strokeMap)
            where
            strokeMap = Map.fromList
                [(beat, stroke) | (beat, stroke, _) <- break.beats]
        thru = Util.imThruFunction dir (convert sample break.pitchAdjust)
        beatMap = makeBeatMap break
        breakCall = c_break break.bpm beatMap break.perMeasure
    dir = "break"

-- * cmd

-- | I can fit 8 per octave, and with black notes that's 16.
lookupStroke :: Beat -> Beat -> Map (Measure, Beat) Text
    -> Cmd.NoteEntryMap DeriveT.Expr
lookupStroke increment perMeasure strokeMap
    | fst maxBeat > 2 = Cmd.WithOctave $ Map.fromList
        [ (oct, Map.mapMaybe (exprAtOctave oct) charToBeat)
        | oct <- [baseOctave - 1 .. baseOctave + fst maxBeat - 1]
        ]
    | otherwise = Cmd.WithoutOctave $ toExpr <$> charToBeat
    where
    -- Octave shifts the measure of the beat returned by charToBeat.
    exprAtOctave octave (measure, beat)
        | adjusted <= 0 = Nothing
        | (adjusted, beat) > maxBeat = Nothing
        | otherwise = Just $ toExpr (adjusted, beat)
        where
        -- I originally intended octave to move by 2 measures if 2 measures
        -- fit on a row, but I think I don't mind moving just 1 measure.
        adjusted = (octave - baseOctave) + measure
    maxBeat = maybe (1, 1) fst $ Map.lookupMax strokeMap
    charToBeat = Map.fromList $ physicalKeys steps allBeats
    steps = floor (perMeasure / increment)
    toExpr mbeat = maybe (makeExpr mbeat) (Expr.generator0 . Expr.Symbol) $
        Map.lookup mbeat strokeMap
    inRange = maybe (const False) (\(m, _) -> (<=m)) $ Map.lookupMax strokeMap
    allBeats = takeWhile inRange
        [ (measure, beat)
        | measure <- [1..], beat <- Lists.range' 1 (perMeasure+1) increment
        ]
    baseOctave = Cmd.state_kbd_entry_octave Cmd.initial_edit_state

makeExpr :: (Measure, Beat) -> DeriveT.Expr
makeExpr (measure, beat) = Expr.generator $ Expr.call "n" [DeriveT.num num]
    where num = fromIntegral measure + realToFrac (beat / 10)
    -- 1.25 -> 0.125

physicalKeys :: Int -> [(Measure, Beat)] -> [(Char, (Measure, Beat))]
physicalKeys steps beats = concat
    [ zip keys (map (measure,) beats)
    | (keys, (measure, beats)) <- zip (keysByMeasure steps) byMeasure
    ]
    where
    byMeasure = Lists.groupFst beats

-- | Get keys to map for each measure.  This fits an integral measure's worth
-- into the bottom and top rows.
keysByMeasure :: Int -> [[Char]]
keysByMeasure steps = concatMap (equalDivisions steps) $
    map (map fst) $ Lists.groupAdjacent (Pitch.pitch_octave . snd) $
    -- '1' is a special case with -1 accidental.
    filter ((>=0) . Pitch.pitch_accidentals . snd) $
    Lists.sortOn snd $ Map.toList PhysicalKey.pitch_map

equalDivisions :: Int -> [a] -> [[a]]
equalDivisions n xs
    | length pre == n = pre : equalDivisions n post
    | otherwise = []
    where (pre, post) = splitAt n xs

-- * call

data BpmMode = Pitch | Stretch
    deriving (Eq, Enum, Bounded, Show, ShowVal.ShowVal, Typecheck.Typecheck,
        Typecheck.ToVal)

-- | Take a beat arg or named start time, and look up the corresponding start
-- offset.
c_break :: BPM -> Map Beat Frame -> Beat -> Maybe Frame
    -> Derive.Generator Derive.Note
c_break naturalBpm beatMap perMeasure mbFrame =
    Derive.generator Module.instrument "break" mempty doc $
    Sig.call ((,,,,,)
        <$> maybe (Left <$> beatArg) (pure . Right) mbFrame
        <*> Sig.defaulted "pre" (Typecheck.score 0)
            "Move note start back by this much, along with the offset."
        <*> Sig.defaulted "pitch" (0 :: Double) "Pitch offset."
        <*> Sig.defaulted "pitch-shift" (0 :: Double)
            "Pitch offset, not affecting time."
        <*> Sig.defaulted "bpm" (Nothing :: Maybe BPM)
            "Set BPM. If Nothing, infer from the tempo."
        <*> Sig.defaulted "bpm-mode" Pitch
            "How to adjust bpm, by changing pitch, or stretching time."
    ) $ \(beatOrFrame, Typecheck.DefaultScore pre, pitch, pitchShift, mbBpm,
        bpmMode) ->
    Sub.inverting $ \args -> do
        frame <- either (lookupBeat beatMap) return beatOrFrame
        let (start, dur) = Args.extent args
        pre <- Call.score_duration start pre
        rpre <- Call.real_duration start pre
        bpm <- maybe ((*60) <$> Derive.get_tempo (Args.start args)) pure mbBpm
        let (bpmPitch, bpmStretch) = adjustBpm naturalBpm bpm bpmMode
        withControl Control.sampleStartOffset
                (fromIntegral frame - fromIntegral (AUtil.toFrames rpre)) $
            withControl pitchOffset (Pitch.nn_to_double (pitch + bpmPitch)) $
            bpmStretch $
            withControl Control.samplePitchShift pitchShift $
            Derive.place (start - pre) (dur + pre) Call.note
    where
    beatArg = Sig.required "beat" "Offset measure.beat. This abuses\
        \ decimal notation: 4 -> (1, 4), 4.1 -> (4, 1), 4.15 -> (4, 1.5)."
    doc = "Play a sample at a certain offset."
    lookupBeat beatMap beatFraction =
        Derive.require
            (pretty (measure, beat) <> " out of range: " <> "(1, 1) -- "
                <> pretty (maxBeat beatMap perMeasure, perMeasure)) $
            if beat >= perMeasure + 1 then Nothing else findFrame beatMap b
            -- beat > perMeasure would work, but it's confusing.
        where
        (measure, beat) = decodeBeat beatFraction
        b = max 0 (fromIntegral (measure-1)) * perMeasure + beat

withControl :: Control.Control -> Signal.Y -> Derive.Deriver a
    -> Derive.Deriver a
withControl control =
    Derive.with_constant_control (Controls.from_shared control)

adjustBpm :: BPM -> BPM -> BpmMode
    -> (Pitch.NoteNumber, Derive.Deriver a -> Derive.Deriver a)
adjustBpm naturalBpm bpm = \case
    Pitch -> (bpmPitchAdjust naturalBpm bpm, id)
    Stretch -> (0,) $ withControl Control.sampleTimeStretch (naturalBpm / bpm)

-- bpm to *2 means +12
-- bpm / naturalBpm = 2 => ratio
bpmPitchAdjust :: BPM -> BPM -> Pitch.NoteNumber
bpmPitchAdjust naturalBpm bpm = Sample.ratioToPitch $ bpm / naturalBpm

maxBeat :: Map Beat frame -> Beat -> Int
maxBeat beatMap perMeasure =
    maybe 1 (floor . (/ perMeasure) . fst) (Map.lookupMax beatMap)

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

convert :: FilePath -> Pitch.NoteNumber -> Note.Note
    -> Patch.ConvertM Sample.Sample
convert filename pitchAdjust note = return $ (Sample.make filename)
    { Sample.envelope = Util.dynEnvelope minDyn 0.15 note
    , Sample.offset = offset
    , Sample.ratios = Signal.constant $
        Sample.relativePitchToRatio (Pitch.nn pitch + pitchAdjust)
    , Sample.stretch = Sample.Stretch
        { stretchMode = Sample.StretchPercussive
        , timeRatio = fromMaybe 1 $
            Note.initial Control.sampleTimeStretch note
        , pitchRatio =
            maybe 1 (recip . Sample.relativePitchToRatio . Pitch.nn) $
            Note.initial Control.samplePitchShift note
        }
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
type BPM = Double
type Frame = Int
type Stroke = Text

addMeasures :: [(Beat, Stroke, Frame)] -> [((Measure, Beat), Stroke, Frame)]
addMeasures = concatMap add . zip [1..]
    . Lists.splitBetween (\(a, _, _) (b, _, _) -> b < a)
    where
    add (m, beats) =
        [((m, beat), stroke, frame) | (beat, stroke, frame) <- beats]

-- | Make Strokes unique by labelling them according to position in the
-- measure.
labelStrokes :: [((Measure, Beat), Stroke, frame)]
    -> [((Measure, Beat), Stroke, frame)]
labelStrokes = map $ \((measure, beat), stroke, frame) ->
    ((measure, beat), stroke <> suffix measure beat, frame)
    where
    suffix measure beat = showt measure
        <> if beat == 1 then "" else "-" <> encode beat
    encode = Text.dropWhileEnd (=='0') . Text.replace "." "" . showt
        . (realToFrac :: Beat -> Double)

makeBeatMap :: Break -> Map Beat Frame
makeBeatMap break = Map.fromList
    [ (fromIntegral (measure-1) * break.perMeasure + beat, frame)
    | ((measure, beat), _, frame) <- break.beats
    ]

-- * bpm

-- | For use from ghci.
_printBpms :: IO ()
_printBpms = forM_ allBreaks $ \break -> do
    Text.IO.putStrLn $ "---- " <> break.name
    Text.IO.putStrLn $ showBpms break

-- | Get inferred bpm from each stroke.
showBpms :: Break -> Text
showBpms break =
    Text.unlines . (++[last strokes, inferredBpm, bpm])
        . map fmt . zip strokes . breakBpms $ break
    where
    inferredBpm = "Inferred BPM: " <> Num.showFloat 2 (inferBpm break)
    bpm = "BPM: " <> Num.showFloat 2 break.bpm
    fmt (stroke, bpm) =
        Text.justifyLeft 8 ' ' stroke <> " - " <> Num.showFloat 2 bpm
    strokes = [stroke | (_, stroke, _) <- break.beats]

inferBpm :: Break -> BPM
inferBpm = centralMean . breakBpms

-- | Try to get a good mean by discarding outliers.
centralMean :: [Double] -> Double
centralMean bpms = mean $ filter (not . outlier) bpms
    where
    mean xs = Num.sum xs / fromIntegral (length xs)
    outlier bpm = bpm < low + quartile || bpm > high - quartile
    quartile = (high - low) / 4
    low = minimum bpms
    high = maximum bpms

beatToBpm :: Double -> BPM
beatToBpm beat = 1 / beat * 60

-- | Guess bpms based on the distance between each named position.
breakBpms :: Break -> [BPM]
breakBpms break = map guess . pairs . Map.toAscList . makeBeatMap $ break
    where
    guess ((beat0, frame0), (beat1, frame1)) = (60/) $
        (fromIntegral (frame1 - frame0) / realToFrac (beat1 - beat0))
            / fromIntegral Config.samplingRate
            * ratioAdjust
    ratioAdjust = Sample.relativePitchToRatio break.pitchAdjust
    pairs xs = zip xs (drop 1 xs)

data Break = Break {
    name :: Text
    , beats :: [((Measure, Beat), Stroke, Frame)]
    , perMeasure :: Beat -- ^ how many beats in one measure
    , bpm :: BPM
    , increment :: Beat
    -- | Relative pitch offset.  This changes the base speed.
    , pitchAdjust :: Pitch.NoteNumber
    } deriving (Show)

makeBreak :: Text -> Beat -> Pitch.NoteNumber -> Maybe BPM
    -> [(Beat, Stroke, Frame)] -> Break
makeBreak name perMeasure pitchAdjust mbBpm beats = addBpm $ Break
    { name
    , beats = labelStrokes (addMeasures beats)
    , perMeasure
    , bpm = fromMaybe 0 mbBpm
    , increment = 1/2
    , pitchAdjust
    }
    where
    addBpm break
        | break.bpm == 0 = break { bpm = inferBpm break }
        | otherwise = break

bd, sn, hh :: Stroke
bd = "bd"
sn = "sn"
hh = "hh"

allBreaks :: [Break]
allBreaks = [medeski, amen, massaker1, massaker2]

medeski :: Break
medeski = makeBreak "medeski" 8 0 (Just 138.53)
    [ (1,   bd, 0)
    , (2,   sn, 20296)
    , (3.5, bd, 49024)
    , (4.5, sn, 68024)
    , (6,   sn, 95982)
    , (7.5, bd, 124482)
    , (8,   sn, 133878)
    , (8.5, bd, 143584)

    , (1,   bd, 152834)
    , (2,   sn, 172572)
    , (3.5, bd, 201002)
    , (4.5, sn, 220438)
    , (6,   sn, 248210)
    , (7.5, bd, 275986)

    , (1,   bd, 303488)
    , (2,   sn, 322988)
    , (3.5, bd, 351232)
    , (4.5, sn, 370402)
    , (6,   sn, 398044)
    , (7.5, bd, 426150)
    , (8,   sn, 435612)
    , (8.5, bd, 445314)

    , (1,   bd, 454530)
    , (2,   sn, 473836)
    , (3.5, bd, 502050)
    , (4.5, sn, 521034)
    , (5,   bd, 529638)

    , (1,   bd, 604380)
    ]

amen :: Break
amen = makeBreak "amen" 4 0 Nothing
    [ (1,   bd, 0)
    , (2,   sn, 19594)
    , (3.5, bd, 48536)
    , (4,   sn, 57820)
    , (1,   bd, 77630)
    , (2,   sn, 97128)
    , (3.5, bd, 125932)
    , (4,   sn, 135294)
    , (1,   bd, 154792)
    , (2,   sn, 174050)
    , (3.5, bd, 202312)
    , (4.5, sn, 221422)
    , (2,   sn, 249952)
    , (3.5, bd, 278326)
    , (4.5, sn, 297470)
    ]

massaker1 :: Break
massaker1 = makeBreak "massaker1" 4 (-12) Nothing
    [ (1,   bd, 0)
    , (2,   sn, 7880)
    , (3.5, bd, 19790)
    , (4,   sn, 23704)
    , (1.5, bd, 35640)
    , (2,   sn, 39600)
    , (3.5, bd, 51480)
    , (4,   sn, 55348)
    ]

massaker2 :: Break
massaker2 = makeBreak "massaker2" 4 (-12) Nothing
    [ (1,   bd, 0)
    , (2,   bd, 7682)
    , (3,   sn, 15530)
    , (4.5, sn, 27062)
    , (1,   hh, 31070)
    , (2,   sn, 38532)
    , (3,   sn, 45738)
    , (4,   hh, 53672)
    , (4.5, sn, 57217)
    ]
