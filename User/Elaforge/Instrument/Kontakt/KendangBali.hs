-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Kendang patches for "User.Elaforge.Instrument.Kontakt".
module User.Elaforge.Instrument.Kontakt.KendangBali where
import qualified Data.List as List

import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.KendangBali as K
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import           Derive.Attrs (soft)
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Ui.UiConfig as UiConfig
import qualified User.Elaforge.Instrument.Kontakt.Util as Util

import           Global


patches :: [MidiInst.Patch]
patches =
    [ MidiInst.code #= tunggal_code $ CUtil.pitched_drum_patch tunggal_strokes $
        patch "kendang-bali"
    , MidiInst.code #= tunggal_code $ CUtil.drum_patch old_tunggal_strokes $
        patch "kendang-bali-old"
    , MidiInst.code #= K.pasang_code $ MidiInst.triggered $
        patch "kendang-bali-pasang"
    ]
    where
    tunggal_code = CUtil.drum_code CUtil.MidiThru (Just "kendang-tune")
        (map fst tunggal_strokes)
    patch name = MidiInst.named_patch (-24, 24) name []

tunggal_strokes :: CUtil.PitchedStrokes
(tunggal_strokes, resolve_errors) =
    CUtil.resolve_strokes K.soft_dyn tunggal_keymap
        [ (char, K.to_call note, attrs, group)
        | (char, note@(K.Note _ attrs), group) <- K.tunggal_table
        ]

tunggal_keymap :: Map Attrs.Attributes CUtil.KeyswitchRange
tunggal_keymap = CUtil.make_keymap (Just Key2.e_2) Key2.c_1 12 Key.fs3
    [ [K.de <> Attrs.staccato, K.plak]
    , [K.de <> Attrs.thumb, K.dag <> Attrs.staccato]
    , [K.de, K.dag]
    , [K.de <> Attrs.closed, K.tek]
    , [K.tut]
    , [K.ka]
    , [K.pang]
    , [K.pak]
    , [K.de <> Attrs.left, K.tut <> Attrs.left]
    ]

-- | Mapping for the old kendang patches.
old_tunggal_strokes :: [(Drums.Stroke, Midi.Key)]
old_tunggal_strokes = map (first stroke)
    [ (K.plak, Key.g1)
    -- left
    , (K.pak, Key.c5)
    , (K.pang, Key.g4)
    , (K.pak <> soft, Key.c5)
    , (K.de <> Attrs.left, Key.d4)
    , (K.tut <> Attrs.left, Key.c4)
    -- right
    , (K.de, Key.c2)
    , (K.de <> soft, Key.c2)
    , (K.de <> Attrs.thumb, Key.f2)
    , (K.de <> Attrs.staccato, Key.c1)
    , (K.tut, Key.c3)
    , (K.ka <> soft, Key.g3)
    , (K.ka, Key.g3)
    , (K.dag, Key.c2)
    , (K.dag <> soft, Key.c2)
    , (K.tek <> soft, Key.c1)
    , (K.tek, Key.c1)
    ]
    where
    stroke attrs =
        Drums.stroke_dyn char (K.to_call (K.Note stroke attrs)) attrs
            (if Attrs.contain attrs soft then K.soft_dyn else 1)
        where
        Just (char, (K.Note stroke _), _) =
            List.find ((==attrs) . attrs_of) K.tunggal_table
    attrs_of (_, (K.Note _ a), _) = a

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("kendang-bali.ksp",
        Util.drum_mute_ksp "kendang bali" tunggal_strokes K.stops)
    ]

-- * config

-- | @LInst.merge $ KendangBali.allocations ...@
allocations :: Text -> Text -> UiConfig.Allocations
allocations name dev_ = MidiInst.allocations
    [ (inst $ name <> "w", "kontakt/kendang-bali", id, midi_channel 0)
    , (inst $ name <> "l", "kontakt/kendang-bali", id, midi_channel 1)
    , ( inst name, "kontakt/kendang-bali-pasang"
      , Common.add_cenviron "wadon" (inst $ name <> "w")
        . Common.add_cenviron "lanang" (inst $ name <> "l")
      , UiConfig.Dummy
      )
    ]
    where
    midi_channel = UiConfig.Midi . MidiInst.config1 dev
    dev = Midi.write_device dev_
    inst = ScoreT.Instrument
