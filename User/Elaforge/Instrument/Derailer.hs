-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | PA3 Derailer.
module User.Elaforge.Instrument.Derailer where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.ScoreT as ScoreT
import Global


synth :: MidiInst.Synth
synth =
    MidiInst.synth "derailer" "PA3 Derailer." $
        MidiInst.synth_controls controls patches
    where
    controls = map (second ScoreT.Control) $ concat
        [ [ (cc, "drone" <> showt n <> "-mass")
          | (n, cc) <- zip [1..5] [21..25]
          ]
        , [(28, "strike-mass")]
        , [ (cc, "drone" <> showt n <> "-gain")
          | (n, cc) <- zip [1..5] [41..45]
          ]
        , [ (48, "strike-gain")
          , (31, "con-strength")
          , (32, "in-pos")
          , (33, "strike-rate")
          ]
        ]
    patches =
        [ patch "strike" []
        , patch "multi-strike" [(33, "strike-rate"), (1, "breath")]
        , MidiInst.pressure $ patch "bow" [(34, "bow-depth"), (1, "dyn")]
        ]
    patch = MidiInst.named_patch (-2, 2)
