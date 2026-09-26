-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to construct instrument allocations.
module Cmd.Repl.LAlloc where
import qualified Data.Text.IO as Text.IO

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.C.Bali.Gangsa as Gangsa
import qualified Derive.Parse.Instruments as Instruments
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


-- | Convert Allocations into ky and paste from here into instrument: section.
ky :: UiConfig.Allocations -> Cmd.CmdL Text
ky allocs = do
    lookupInst <- Cmd.getLookupInstEnv <$> Cmd.get
    either (pure . ("ERROR: "<>)) pure $
        Instruments.unInstruments lookupInst allocs

ky_ :: UiConfig.Allocations -> IO ()
ky_ allocs =
    either (Text.IO.putStrLn . ("ERROR: "<>)) Text.IO.putStr $
        Instruments.unInstruments (const Nothing) allocs

-- * bali

rambatIm :: UiConfig.Allocations
rambatIm =
    pasangIm Short "ra" BaliScales.Umbang
        sampler "rambat" "rambat-umbang" "rambat-isep"

wayangIm :: Text -> Text -> UiConfig.Allocations
wayangIm pemade kantilan =
    pasangIm Short pemade BaliScales.Umbang
        sampler "wayang-pemade" "wayang-pemade-umbang" "wayang-pemade-isep"
    <> pasangIm Short kantilan BaliScales.Umbang
        sampler "wayang-kantilan"
        "wayang-kantilan-umbang" "wayang-kantilan-isep"

sampler :: Text -> InstT.Qualified
sampler = InstT.Qualified "sampler"

-- | Set up a gender wayang quartet.
--
-- There are two pasang instruments, which then rely on the kotekan calls to
-- split into inst-polos and inst-sangsih.  Polos has umbang.
wayangMidi :: Text -> Text -> Text -> UiConfig.Allocations
wayangMidi dev_ pemade kantilan =
    pasangMidi Short dev 0 pemade BaliScales.Umbang
        kontakt "wayang-pemade" "wayang-umbang" "wayang-isep"
    <> pasangMidi Short dev 2 kantilan BaliScales.Umbang
        kontakt "wayang-kantilan" "wayang-umbang" "wayang-isep"
    where
    dev = Midi.writeDevice dev_

kontakt :: Text -> InstT.Qualified
kontakt = InstT.Qualified "kontakt"

data Verbosity = Long | Short deriving (Eq, Show)

-- | Set up a umbang isep pair.
pasangIm :: Verbosity -> Text -> BaliScales.Tuning -> (Text -> InstT.Qualified)
    -> Text -> Text -> Text -> UiConfig.Allocations
pasangIm verbosity base polosTuning
        qualify pasangQual umbangQual isepQual =
    ImInst.allocations
        [ (inst base, qualify pasangQual, pasang, UiConfig.Dummy "")
        , (umbang, qualify umbangQual, id, UiConfig.Im)
        , (isep, qualify isepQual, id, UiConfig.Im)
        ]
    where
    umbang = inst $ base <> case verbosity of
        Long -> "-umbang"
        Short -> "u"
    isep = inst $ base <> case verbosity of
        Long -> "-isep"
        Short -> "i"
    pasang = makePasang polosTuning umbang isep
    inst = ScoreT.Instrument

pasangMidi :: Verbosity -> Midi.WriteDevice -> Midi.Channel -> Text
    -> BaliScales.Tuning -> (Text -> InstT.Qualified) -> Text -> Text -> Text
    -> UiConfig.Allocations
pasangMidi verbosity dev chan base polosTuning
        qualify pasangQual umbangQual isepQual =
    MidiInst.allocations
        [ (inst base, qualify pasangQual, pasang, UiConfig.Dummy "")
        , (umbang, qualify umbangQual, id, midiChannel 0)
        , (isep, qualify isepQual, id, midiChannel 1)
        ]
    where
    umbang = inst $ base <> case verbosity of
        Long -> "-umbang"
        Short -> "u"
    isep = inst $ base <> case verbosity of
        Long -> "-isep"
        Short -> "i"
    pasang = makePasang polosTuning umbang isep
    midiChannel relativeChan =
        UiConfig.Midi (MidiInst.config1 dev (chan + relativeChan))
    inst = ScoreT.Instrument

makePasang :: BaliScales.Tuning -> ScoreT.Instrument -> ScoreT.Instrument
    -> Common.Config -> Common.Config
makePasang polosTuning umbang isep =
    Common.addCenviron Gangsa.instPolos polos
    . Common.addCenviron Gangsa.instSangsih sangsih
    where
    (polos, sangsih) = case polosTuning of
        BaliScales.Umbang -> (umbang, isep)
        BaliScales.Isep -> (isep, umbang)

type Device = Text

kebyar :: Device -> UiConfig.Allocations
kebyar = ScGamelan.kebyarAllocations

kendangKontakt :: Device -> UiConfig.Allocations
kendangKontakt = KendangBali.allocations "k"

kendangIm :: UiConfig.Allocations
kendangIm = Sampler.Patch.KendangBali.allocations "k"

legongUmbang :: Patch.Scale
legongUmbang = Legong.completeInstrumentScale Legong.larasRambat
    BaliScales.Umbang

legongIsep :: Patch.Scale
legongIsep = Legong.completeInstrumentScale Legong.larasRambat
    BaliScales.Isep
