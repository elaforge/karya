-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Faust.PatchDb (synth, warnings) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Doc as Doc
import qualified Util.Maps as Maps
import qualified Util.Seq as Seq

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.InstTypes as InstTypes
import qualified Perform.Im.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Faust.Code as Code
import qualified Synth.Faust.InstrumentC as InstrumentC
import qualified Synth.Faust.Preview as Preview
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Thru as Thru
import qualified Synth.Shared.Signal as Signal

import           Global


synth :: ImInst.Synth
synth = ImInst.synth Config.faustName "音 faust synthesizer" patches

patches :: [(InstTypes.Name, ImInst.Patch)]
warnings :: [Text]
(warnings, patches) = Unsafe.unsafePerformIO $ do
    imDir <- Config.imDir <$> Config.getConfig
    -- These are in IO, but should be safe, because they are just reading
    -- static data.  In fact the FFI functions could probably omit the IO.
    pmap <- InstrumentC.getPatches
    let errors =
            [ "faust/" <> name <> ": " <> err
            | (name, Left err) <- Map.toList pmap
            ] ++
            [ "faust: patch code with no patch: " <> name
            | (name, Seq.Second _) <- Maps.pairs pmap patchCode
            ]
    return $ (errors,)
        [ (name, makePatch imDir patch)
        | (name, Right patch) <- Map.toList pmap
        ]

patchCode :: Map Text ImInst.Code
patchCode = Map.fromList
    [ ("tambura8", tambura), ("tambura4", tambura)
    ]
    where
    tambura = ImInst.note_calls
        [ ImInst.transformer "terminate" (Code.note_terminate "decay" 0.01)
        ]

makePatch :: FilePath -> InstrumentC.Patch -> ImInst.Patch
makePatch imDir patch =
    ImInst.doc #= Doc.Doc (InstrumentC._doc patch) $
    code constantPitch constantControls $
    ImInst.make_patch $ Patch.patch
        { Patch.patch_controls = (pretty <$> controls) <> standardControls
        , Patch.patch_elements = Set.fromList $ filter (/="") $ map fst $
            Map.keys $ InstrumentC._controls patch
        }
    where
    constantControls = map fst $ filter (InstrumentC._constant . snd) $
        filter ((/=Control.pitch) . fst) $ Map.toList controls
    constantPitch = maybe False InstrumentC._constant $
        Map.lookup Control.pitch controls
    controls = InstrumentC.imControls patch
    code constantPitch constantControls = (ImInst.code #=) $ mconcat
        [ if constantPitch || not (null constantControls)
            then ImInst.null_call $
                DUtil.constant_controls constantPitch
                    (Set.fromList (map control constantControls))
            else mempty
        , thruCode pitchToSample
        , case InstrumentC._elementFrom patch of
            Nothing -> mempty
            Just elementFrom -> ImInst.postproc $
                DUtil.element_from_id elementFrom
        , Map.findWithDefault mempty (InstrumentC._name patch) patchCode
        ]
    pitchToSample = Preview.pitchToSample imDir (InstrumentC._name patch)

control :: Control.Control -> ScoreT.Control
control (Control.Control c) = ScoreT.Control c

standardControls :: Map Control.Control Text
standardControls = Map.fromList
    [ (Control.volume, "Low level volume, in dB.")
    -- , (Control.pan, "Pan, where -1 is left, and 1 is right.")
    ]
    -- All instruments put dyn in the gate signal.
    <> Control.supportDyn

-- * thru

thruCode :: Map Pitch.NoteNumber FilePath -> ImInst.Code
thruCode = ImInst.thru . thruFunction

-- This would be used for attribute-oriented instruments for a custom CUtil
-- call.
-- imThruFunction :: Map Pitch.NoteNumber FilePath -> CUtil.Thru
-- imThruFunction = CUtil.ImThru . thruFunction

thruFunction :: Map Pitch.NoteNumber FilePath -> Thru.ThruFunction
thruFunction pitchToSample = fmap Thru.Plays . mapM note
    where
    note (Thru.Note { _pitch = pitch }) =
        case Maps.lookup_closest pitch pitchToSample of
            Nothing -> Left "no samples"
            Just (sampleNn, sample) -> Right $ Thru.Play
                { _sample = sample
                , _offset = 0
                , _ratio = pitchToRatio sampleNn pitch
                -- I could use velocity, but I don't render at different
                -- dynamics so let's not give that impression.
                , _volume = 1
                }

-- | From Sampler.Sample
pitchToRatio :: Pitch.NoteNumber -> Pitch.NoteNumber -> Signal.Y
pitchToRatio sampleNn nn = Pitch.nn_to_hz sampleNn / Pitch.nn_to_hz nn
