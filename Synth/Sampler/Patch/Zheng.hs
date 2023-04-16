-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Synth.Sampler.Patch.Zheng where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified Text.Read as Read

import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.PPrint as PPrint
import qualified Util.Lists as Lists

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Highlight as Highlight
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Common as Common
import qualified Midi.Midi as Midi
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Patch.ZhengSamples as ZhengSamples
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


patches :: [Patch.Patch]
patches = (:[]) $ (Patch.patch "zheng")
    { Patch._dir = dir
    , Patch._convert = convert
    , Patch._allFilenames = Set.fromList $ concat $ concatMap Map.elems $
        concatMap Map.elems $ Map.elems $ ZhengSamples.samples
    , Patch._preprocess = inferDuration
    , Patch._karyaPatch = ImInst.code #= code $ ImInst.nn_range range $
        ImInst.make_patch $ Im.Patch.patch
            { Im.Patch.patch_controls = mconcat
                [ Control.supportPitch
                , Control.supportDyn
                , Control.supportVariation
                ] <> Map.fromList
                [ (c_damp, "Notes don't ring when this is 1.")
                , (c_damp_time, "Time in seconds to 0.")
                ]
            , Im.Patch.patch_attribute_map = const () <$> attributeMap
            }
    }
    where
    dir = "zheng"
    code = ImInst.note_calls
        [ ImInst.both "左" (Make.attributed_note Module.instrument Attrs.left)
        , ImInst.transformer "standard-strings" standard_strings
        ]
        <> ImInst.null_call Highlight.c_highlight_strings_note
        <> Util.thru dir convert
        <> ImInst.postproc
            (DUtil.element_from EnvKey.string show_string)
    -- copy paste from User.Elaforge.Instrument.Kontakt
    -- TODO put it in a shared module?
    -- This can't go in the automatic env because it uses DeriveT.Pitch, which
    -- is not serializable, hence not in REnv.
    standard_strings = DUtil.transformer0 "standard-strings"
        ("Set " <> ShowVal.doc EnvKey.open_strings
            <> " to standard pitches: " <> ShowVal.doc open_strings)
        $ \_ -> Derive.with_val EnvKey.open_strings
            (map Twelve.nn_pitch open_strings)
    open_strings = take (4*5 + 1) $ -- 4 octaves + 1, so D to D
        concatMap ((\nns oct -> map (oct+) nns) notes) octaves
        where
        notes = [NN.d2, NN.e2, NN.fs2, NN.a2, NN.b2]
        octaves = map fromIntegral [0, 12 ..]
    -- Let's say the top string can bend a minor third.
    range = (head open_strings, last open_strings + 3)

show_string :: PSignal.Pitch -> Either Log.Msg Text
show_string = bimap (Log.msg Log.Warn Nothing . pretty) Pitch.note_text
    . PSignal.pitch_note . PSignal.coerce

convert :: Note.Note -> Patch.ConvertM Sample.Sample
convert note = do
    art <- Util.articulation attributeMap (Note.attributes note)
    let dynVal = Note.initial0 Control.dynamic note
    let var = Note.initial0 Control.variation note
    noteNn <- Util.initialPitch note
    (key, sampleDyn, filename) <- tryJust "no sample" $
        toFilename noteNn art dynVal var
    -- The bottom of the scale should be enough to smooth out the volume
    -- differences between each velocity group.  It's surely highly variable,
    -- but this seems to sound ok in practice.
    let vol = Num.scale 0 1 (1 - (sampleDyn - dynVal))
    let dampTime = maybe defaultDampTime RealTime.seconds $
            Note.controlAt (Note.end note) c_damp_time note
    return $ (Sample.make filename)
        { Sample.envelope = Signal.from_pairs
            [ (Note.start note, vol), (Note.end note, vol)
            , (Note.end note + dampTime, 0)
            ]
        , Sample.ratios = Sample.pitchToRatioSignal (Midi.from_key key) note
        }

defaultDampTime :: RealTime
defaultDampTime = 0.75

toFilename :: Pitch.NoteNumber -> Articulation -> Signal.Y -> Signal.Y
    -> Maybe (Midi.Key, Signal.Y, FilePath)
toFilename nn art dyn var = do
    (key, velToFiles) <- Maps.lookupClosest (Midi.to_key (round nn))
        (samples art)
    -- TODO pick some from neighbors, since I lost variations due to combining
    -- them
    -- TODO also scale by difference from maxVel
    (sampleVel, filenames) <- Map.lookupGE
        (round (Num.clamp 0 127 (Num.scale 0 127 dyn)))
        velToFiles
    return
        ( key
        , Num.normalize 0 127 (fromIntegral sampleVel)
        , Util.pickVariation filenames var
        )

data Articulation = RightHand | LeftHand | Harmonic
    deriving (Eq, Ord, Show, Enum)

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (Attrs.harm, Harmonic)
    , (Attrs.left, LeftHand)
    , (mempty, RightHand)
    ]

samples :: Articulation -> Map Midi.Key (Map MaxVelocity [FilePath])
samples art = fromMaybe (error ("unknown articulation: " <> show art)) $
    Map.lookup (fromEnum art) ZhengSamples.samples

-- * preprocess

c_damp :: Control.Control
c_damp = "damp"

c_damp_time :: Control.Control
c_damp_time = "damp-time"

-- | Interpret the 'c_damp' control.  Each note extends until there's a note
-- with a c_damp with 1.
inferDuration :: [Note.Note] -> [Note.Note]
inferDuration = map infer . Util.nexts
    where
    -- It would be more efficient to find the next damp, then remember it until
    -- I pass it.  But the difference probably doesn't matter.
    infer (note, nexts) = note
        { Note.duration = maybe Sample.forever (\end -> end - Note.start note)
            (inferEnd note nexts)
        }

inferEnd :: Note.Note -> [Note.Note] -> Maybe RealTime
inferEnd note nexts = case mapMaybe dampedAt (note : nexts) of
    [] -> Nothing
    end : _ -> Just $ max end (Note.end note)
    where
    dampedAt = fmap fst . Signal.find (\_ y -> y >= 1)
        . fromMaybe mempty . Map.lookup c_damp . Note.controls

-- * make samples

data Sample = Sample {
    _key :: !Midi.Key
    , _articulation :: !Articulation
    , _variation :: !Util.Variation
    , _maxVelocity :: !MaxVelocity
    } deriving (Show)

type MaxVelocity = Int

-- | Call this to generate ZhengSamples, which is imported as 'samples'.
_writeSamplesModule :: IO ()
_writeSamplesModule = do
    fns <- Directory.listDirectory (Config.unsafeSamplerRoot </> "zheng")
    writeFile output $ samplesModuleHeader <> makeSamples fns
    where
    output = "Synth/Sampler/Patch/ZhengSamples.hs"

parseFilename :: FilePath -> Maybe Sample
parseFilename ('S':'C':'G':'Z':key1:key2:_:art:'-':rest) = do
    key <- Midi.Key <$> Read.readMaybe (key1:key2:"")
    art <- case art of
        'R' -> Just RightHand
        'L' -> Just LeftHand
        'H' -> Just Harmonic
        _ -> Nothing
    (var, maxVel) <- case art of
        Harmonic -> (0,) <$> Read.readMaybe (takeWhile Char.isDigit rest)
        _ -> case rest of
            n : rest -> (fromEnum n - fromEnum 'A',) <$>
                Read.readMaybe (takeWhile Char.isDigit rest)
            _ -> Nothing
    -- One sample is mislabeled.
    key <- return $ if key == 80 then 81 else key
    -- Harmonics are given an octave below their sounding pitch.
    key <- return $ if art == Harmonic then key + 12 else key
    return $ Sample key art var maxVel
parseFilename _ = Nothing

makeSampleMap :: [FilePath]
    -> Map Articulation (Map Midi.Key (Map MaxVelocity [FilePath]))
makeSampleMap =
    fmap (fmap (fmap (map snd)))
    . fmap (fmap (groupOn _maxVelocity))
    . fmap (groupOn _key)
    . groupOn _articulation
    . Lists.keyOnJust parseFilename
    where
    groupOn key = Map.fromList . Lists.keyedGroupSort (key . fst)

makeSamples :: [FilePath] -> String
makeSamples = PPrint.pshow . Map.mapKeys fromEnum . makeSampleMap

samplesModuleHeader :: String
samplesModuleHeader =
    "-- Copyright 2019 Evan Laforge\n\
    \-- This program is distributed under the terms of the GNU General Public\n\
    \-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt\n\
    \\n\
    \-- | This module was generated by Zheng._writeSamplesModule.\n\
    \module Synth.Sampler.Patch.ZhengSamples (samples) where\n\
    \import Data.Map (Map, fromList)\n\
    \import Midi.Midi (Key(Key))\n\
    \\n\
    \samples :: Map Int (Map Key (Map Int [FilePath]))\n\
    \samples = "
