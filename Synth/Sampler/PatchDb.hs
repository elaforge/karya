-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Sampler.PatchDb (db2, db, synth) where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Wayang as Wayang
import qualified Synth.Sampler.Patch2 as Patch2
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


db2 :: Patch2.Db
db2 = Patch2.db "../data/sampler" $ concat
    [ Wayang.patches
    , [testPatch]
    ]

testPatch :: Patch2.Patch
testPatch = Patch2.Patch
    { _name = "test"
    , _convert = \note -> do
        pitch <- tryJust "no pitch" $ Note.initialPitch note
        dyn <- tryJust "no dyn" $ Note.initial Control.dynamic note
        return $ Sample.Sample
            { filename = "open.flac"
            , offset = 0
            , envelope = Signal.constant dyn
            , ratio = Signal.constant $
                Sample.pitchToRatio (Pitch.nn_to_hz 60) pitch
            }
    , _karyaPatch = ImInst.make_patch $
        Im.Patch.Patch
            { patch_controls = Control.supportPitch <> Control.supportDyn
            , patch_attribute_map = Common.attribute_map []
            , patch_flags = mempty
            }
    }

-- * old

db :: Patch.Db
db = Patch.Db
    { _patches = Map.fromList
        [ ("test",) $ Patch.patch "test"
            [ ("cek.wav", attrs cek Patch.sample)
            , ("open.wav", attrs open $ Patch.pitchedSample 60)
            ]
        ]
    , _rootDir = "Synth/Sampler/instruments"
    }

cek, open :: Attrs.Attributes
cek = Attrs.attr "cek"
open = Attrs.attr "open"

attrs :: Attrs.Attributes -> Patch.Sample -> Patch.Sample
attrs attrs sample = sample { Patch.attributes = attrs }

-- | Declaration for "Local.Instrument".
synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = ImInst.synth Config.samplerName "音 sampler"
    [ (name, Patch2._karyaPatch patch)
    | (name, patch) <- Map.toList (Patch2._patches db2)
    ]
