-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to construct instrument allocations.  You can merge them with
-- 'LInst.replace' or 'LInst.merge'.
module Cmd.Repl.LAlloc where
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.C.Bali.Gangsa as Gangsa
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Synth.Sampler.Patch.KendangBali as Sampler.Patch.KendangBali
import qualified Ui.UiConfig as UiConfig
import qualified User.Elaforge.Instrument.Kontakt.KendangBali as KendangBali
import qualified User.Elaforge.Instrument.Kontakt.ScGamelan as ScGamelan

import           Global


-- * bali

rambat_im :: UiConfig.Allocations
rambat_im =
    pasang_im Short "ra" BaliScales.Umbang
        sampler "rambat" "rambat-umbang" "rambat-isep"

wayang_im :: Text -> Text -> UiConfig.Allocations
wayang_im pemade kantilan =
    pasang_im Short pemade BaliScales.Umbang
        sampler "wayang-pemade" "wayang-pemade-umbang" "wayang-pemade-isep"
    <> pasang_im Short kantilan BaliScales.Umbang
        sampler "wayang-kantilan"
        "wayang-kantilan-umbang" "wayang-kantilan-isep"

sampler :: Text -> InstT.Qualified
sampler = InstT.Qualified "sampler"

-- | Set up a gender wayang quartet.
--
-- There are two pasang instruments, which then rely on the kotekan calls to
-- split into inst-polos and inst-sangsih.  Polos has umbang.
wayang_midi :: Text -> Text -> Text -> UiConfig.Allocations
wayang_midi dev_ pemade kantilan =
    pasang_midi Short dev 0 pemade BaliScales.Umbang
        kontakt "wayang-pemade" "wayang-umbang" "wayang-isep"
    <> pasang_midi Short dev 2 kantilan BaliScales.Umbang
        kontakt "wayang-kantilan" "wayang-umbang" "wayang-isep"
    where
    dev = Midi.write_device dev_

kontakt :: Text -> InstT.Qualified
kontakt = InstT.Qualified "kontakt"

data Verbosity = Long | Short deriving (Eq, Show)

-- | Set up a umbang isep pair.
pasang_im :: Verbosity -> Text -> BaliScales.Tuning -> (Text -> InstT.Qualified)
    -> Text -> Text -> Text -> UiConfig.Allocations
pasang_im verbosity base polos_tuning
        qualify pasang_qual umbang_qual isep_qual =
    ImInst.allocations
        [ (inst base, qualify pasang_qual, pasang, UiConfig.Dummy "")
        , (umbang, qualify umbang_qual, id, UiConfig.Im)
        , (isep, qualify isep_qual, id, UiConfig.Im)
        ]
    where
    umbang = inst $ base <> case verbosity of
        Long -> "-umbang"
        Short -> "u"
    isep = inst $ base <> case verbosity of
        Long -> "-isep"
        Short -> "i"
    pasang = make_pasang polos_tuning umbang isep
    inst = ScoreT.Instrument

pasang_midi :: Verbosity -> Midi.WriteDevice -> Midi.Channel -> Text
    -> BaliScales.Tuning -> (Text -> InstT.Qualified) -> Text -> Text -> Text
    -> UiConfig.Allocations
pasang_midi verbosity dev chan base polos_tuning
        qualify pasang_qual umbang_qual isep_qual =
    MidiInst.allocations
        [ (inst base, qualify pasang_qual, pasang, UiConfig.Dummy "")
        , (umbang, qualify umbang_qual, id, midi_channel 0)
        , (isep, qualify isep_qual, id, midi_channel 1)
        ]
    where
    umbang = inst $ base <> case verbosity of
        Long -> "-umbang"
        Short -> "u"
    isep = inst $ base <> case verbosity of
        Long -> "-isep"
        Short -> "i"
    pasang = make_pasang polos_tuning umbang isep
    midi_channel relative_chan =
        UiConfig.Midi (MidiInst.config1 dev (chan + relative_chan))
    inst = ScoreT.Instrument

make_pasang :: BaliScales.Tuning -> ScoreT.Instrument -> ScoreT.Instrument
    -> Common.Config -> Common.Config
make_pasang polos_tuning umbang isep =
    Common.add_cenviron Gangsa.inst_polos polos
    . Common.add_cenviron Gangsa.inst_sangsih sangsih
    where
    (polos, sangsih) = case polos_tuning of
        BaliScales.Umbang -> (umbang, isep)
        BaliScales.Isep -> (isep, umbang)

type Device = Text

kebyar :: Device -> UiConfig.Allocations
kebyar = ScGamelan.kebyar_allocations

kendang_kontakt :: Device -> UiConfig.Allocations
kendang_kontakt = KendangBali.allocations "k"

kendang_im :: UiConfig.Allocations
kendang_im = Sampler.Patch.KendangBali.allocations "k"

legong_umbang :: Patch.Scale
legong_umbang = Legong.complete_instrument_scale Legong.laras_rambat
    BaliScales.Umbang

legong_isep :: Patch.Scale
legong_isep = Legong.complete_instrument_scale Legong.laras_rambat
    BaliScales.Isep
