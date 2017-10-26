-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Ness where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Derive.Attrs as Attrs
import qualified Derive.EnvKey as EnvKey

import qualified Perform.Im.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Ness.Guitar.Patch as Guitar.Patch


synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = Inst.SynthDecl Config.nessName
    "Write notes to a file, to submit to NESS by hand." $ concat
    [ map (,guitar) (Map.keys Guitar.Patch.patches)
    ]

guitar :: Inst.Inst Cmd.InstrumentCode
guitar = Inst.Inst (Inst.Im patch) (Common.common Cmd.empty_code)
    where
    patch = Patch.patch
        { Patch.patch_controls = Map.fromList
            [ (Control.pitch, "")
            , (Control.dynamic, "")
            , (Guitar.Patch.c_location, "")
            , (Guitar.Patch.c_finger, "")
            ]
        , Patch.patch_element_key = Just EnvKey.string
        , Patch.patch_attribute_map = Patch.attribute_map [Attrs.mute]
        }
