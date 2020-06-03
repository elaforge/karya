-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is like the @sample@ patch, but with special support for breaking up
-- beats and naming them.
module Synth.Sampler.Patch.Break where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Sig as Sig

import qualified Perform.Im.Patch as Im.Patch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note

import           Global


patches :: [Patch.DbPatch]
patches = map Patch.DbPatch
    [ make "medeski" medeski
    ]
    where
    make name beats = (Patch.patch ("break-" <> name))
        { Patch._dir = "break"
        , Patch._convert = convert (untxt (name <> ".wav"))
        , Patch._karyaPatch =
            ImInst.code #= code $
            ImInst.make_patch $ Im.Patch.patch
                { Im.Patch.patch_controls =
                    Control.supportDyn
                    <> Control.supportSampleStartOffset
                }
        }
        where
        code = ImInst.note_generators [("n", c_break beatMap strokeMap)]
        beatMap = Map.fromList [(beat, frame) | (beat, frame, _) <- beats]
        strokeMap = Map.fromList [(stroke, frame) | (_, frame, stroke) <- beats]

-- | Take a beat arg or named start time, and look up the corresponding start
-- offset.
c_break :: Map Beat Frame -> Map Stroke Frame -> Derive.Generator Derive.Note
c_break beatMap strokeMap =
    Derive.generator Module.instrument "break" mempty doc $
    Sig.call (
        Sig.required "beat" "Offset beat or named stroke."
    ) $ \beatOrStroke args -> do
        frame <- case beatOrStroke of
            Left beat -> Derive.require ("beat out of range: " <> pretty beat) $
                findFrame beatMap beat
            Right stroke ->
                Derive.require ("unknown stroke: " <> stroke <> ", valid: "
                    <> pretty (Map.keys strokeMap)) $
                Map.lookup stroke strokeMap
        Derive.with_constant_control
                (Controls.from_shared Control.sampleStartOffset)
                (fromIntegral frame) $
            Call.placed_note args
    where
    doc = "Play a sample at a certain offset.\n\nNamed beats: "
        <> Doc.Doc (Text.unwords (Map.keys strokeMap))

convert :: FilePath -> Note.Note -> Patch.ConvertM Sample.Sample
convert filename note = do
    let dynVal = Note.initial0 Control.dynamic note
    let offset = floor $ Note.initial0 Control.sampleStartOffset note
    return $ (Sample.make filename)
        { Sample.envelope = Util.asr dynVal 0.15 note
        , Sample.offset = offset
        }

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


-- | Make Strokes unique by labelling them according to position in the
-- measure.
labelStrokes :: [(Beat, frame, Stroke)] -> [(Beat, frame, Stroke)]
labelStrokes =
    concatMap label . zip [1..]
        . Seq.split_between (\(a, _, _) (b, _, _) -> b < a)
    where
    label (measure, beats) =
        [ (beat, frame, stroke <> suffix measure beat)
        | (beat, frame, stroke) <- beats
        ]
    suffix measure beat = showt measure
        <> if beat == 1 then "" else "-" <> encode beat
    encode = Text.dropWhileEnd (=='0') . Text.replace "." "" . showt
        . (realToFrac :: Beat -> Double)

type Frame = Int
type Beat = Ratio.Ratio Int
type Stroke = Text

unrollMeasures :: Beat -> [(Beat, frame, stroke)] -> [(Beat, frame, stroke)]
unrollMeasures perMeasure = go 0 0
    where
    go _ _ [] = []
    go prevMeasure prev ((beat, frame, stroke) : beats) =
        (beat + perMeasure * measure, frame, stroke) : go measure beat beats
        where
        measure = prevMeasure + if beat < prev then 1 else 0

-- 0.433202947845805 per beat, so 138.5032126359318 bpm, say 138.5
-- tempo 1 is 60bpm, 2 = 120bpm, so 60n = 138.5 = 138.5/60
medeski :: [(Beat, Frame, Stroke)]
medeski = unrollMeasures 8 $ labelStrokes
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
