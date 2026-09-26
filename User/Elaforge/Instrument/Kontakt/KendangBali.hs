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
import qualified Instrument.InstT as InstT
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Ui.UiConfig as UiConfig
import qualified User.Elaforge.Instrument.Kontakt.Util as Util

import           Global


patches :: [MidiInst.Patch]
patches =
    [ MidiInst.code #= tunggalCode $ CUtil.pitchedDrumPatch tunggalStrokes $
        patch "kendang-bali"
    , MidiInst.code #= tunggalCode $ CUtil.drumPatch oldTunggalStrokes $
        patch "kendang-bali-old"
    , MidiInst.dummy "requires realize-kendang" $
        MidiInst.code #= K.pasangCode $ MidiInst.triggered $
        patch "kendang-bali-pasang"
    ]
    where
    tunggalCode = CUtil.drumCode CUtil.MidiThru
        (map ((,config) . fst) tunggalStrokes)
    config = CUtil.callConfig { CUtil._tuningControl = Just "kendang-tune" }
    patch name = MidiInst.namedPatch (-24, 24) name []

tunggalStrokes :: CUtil.PitchedStrokes
(tunggalStrokes, resolveErrors) =
    CUtil.resolveStrokes K.softDyn tunggalKeymap
        [ (char, K.toCall note, attrs, group)
        | (char, note@(K.Note _ attrs), group) <- K.tunggalTable
        ]

tunggalKeymap :: Map Attrs.Attributes CUtil.KeyswitchRange
tunggalKeymap = CUtil.makeKeymap (Just Key2.e_2) Key2.c_1 12 Key.fs3
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
oldTunggalStrokes :: [(Drums.Stroke, Midi.Key)]
oldTunggalStrokes = map (first stroke)
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
        Drums.strokeDyn char (K.toCall (K.Note stroke attrs)) attrs
            (if Attrs.contain attrs soft then K.softDyn else 1)
        where
        Just (char, (K.Note stroke _), _) =
            List.find ((==attrs) . attrsOf) K.tunggalTable
    attrsOf (_, (K.Note _ a), _) = a

writeKsp :: IO ()
writeKsp = mapM_ (uncurry Util.write)
    [ ("kendang-bali.ksp",
        Util.drumMuteKsp "kendang bali" tunggalStrokes K.stops)
    ]

-- * config

-- | @LInst.merge $ KendangBali.allocations ...@
allocations :: Text -> Text -> UiConfig.Allocations
allocations name dev_ = MidiInst.allocations
    [ (inst $ name <> "w", qual "kendang-bali", id, midiChannel 0)
    , (inst $ name <> "l", qual "kendang-bali", id, midiChannel 1)
    , ( inst name, qual "kendang-bali-pasang"
      , Common.addCenviron "wadon" (inst $ name <> "w")
        . Common.addCenviron "lanang" (inst $ name <> "l")
      , UiConfig.Dummy "requires realize-kendang"
      )
    ]
    where
    qual = InstT.Qualified "kontakt"
    midiChannel = UiConfig.Midi . MidiInst.config1 dev
    dev = Midi.writeDevice dev_
    inst = ScoreT.Instrument
