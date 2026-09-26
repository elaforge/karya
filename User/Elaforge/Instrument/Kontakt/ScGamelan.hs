-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Sonic Couture's Gamelan Bali sample set.
module User.Elaforge.Instrument.Kontakt.ScGamelan where
import qualified Data.List as List

import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import qualified Derive.C.Bali.Gangsa as Gangsa
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.REnv as REnv
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Ui.UiConfig as UiConfig

import           Global


synthName :: InstT.SynthName
synthName = "kontakt"

patches :: [MidiInst.Patch]
patches = map addDoc $
    CUtil.simpleDrum CUtil.MidiThru Nothing gongStrokes (scPatch "gong")
    : CUtil.simpleDrum CUtil.MidiThru Nothing kempliKajarNotes
        (scPatch "kempli")
    : reyongKs (rangedPatch Legong.reyongRange "reyong")
    : rangedPatch Legong.trompongRange "trompong"
    : concat
    [ pasang True (rangeOf Legong.jegog) "jegog"
    , pasang True (rangeOf Legong.calung) "calung"
    , pasang True (rangeOf Legong.penyacah) "penyacah"
    , [tunggal False Legong.ugalRange "ugal"]
    , pasang False (rangeOf Legong.pemade) "pemade"
    , pasang False (rangeOf Legong.kantilan) "kantilan"
    ]
    where
    pasang wrapOctaves range name =
        [ tunggal wrapOctaves range name
        , MidiInst.dummy Bali.pasangMsg $ MidiInst.code #= code $
            rangedPatch range (name <> "-pasang")
        ]
        where
        wrap = if wrapOctaves then Just range else Nothing
        code = Bali.gangsaNote 1 wrap
            <> MidiInst.nullCall DUtil.constantPitch
            <> Bali.pasangCode
    tunggal wrapOctaves range name =
        MidiInst.code #= code $ gangsaKs $ rangedPatch range name
        where
        wrap = if wrapOctaves then Just range else Nothing
        code = Bali.gangsaNote 1 wrap
            <> MidiInst.nullCall DUtil.constantPitch

    rangeOf = BaliScales.instrumentRange
    rangedPatch range = MidiInst.range range . scPatch
    scPatch name =
        MidiInst.patch %= MidiInst.addFlags [Patch.UseFinalNoteOff] $
        MidiInst.namedPatch (-2, 2) ("sc-" <> name) []
    addDoc = MidiInst.doc
        %= ("Sonic Couture's Balinese gamelan sample set. " <>)
    gangsaKs = MidiInst.attributeMap #= Patch.singleKeyswitches
        [(Attrs.mute, Key2.cs1), (mempty, Key2.c1)]
    reyongKs = MidiInst.attributeMap #= Patch.singleKeyswitches
        [(Attrs.attr "cek", Key2.cs1), (mempty, Key2.c1)]
    gongStrokes =
        [ (n 'z' "O" (gong <> wadon),   Key2.b1)
        , (n 'x' "o" (gong <> lanang),  Key2.c2)
        , (n 'q' "p" kempur,            Key2.a2)
        , (n 'w' "m" kemong,            Key2.a3)
        ]
        where n = Drums.stroke
    kempliKajarNotes =
        [ (n 'z' "*"    kempli,                 Key2.d3)
        , (n 'a' "`O+`" (kempli <> open),       Key2.ds3)
        , (n 'x' "+1"   (kempli <> Attrs.v1),   Key2.f3)
        , (n 'c' "+2"   (kempli <> Attrs.v2),   Key2.g3)
        , (n 'v' "+3"   (kempli <> Attrs.v3),   Key2.a3)
        , (n 'b' "b"    bebende,                Key2.d4)
        , (n 'g' "B"    (bebende <> open),      Key2.ds4)
        , (n 'q' "o"    kajar,                  Key2.f4)
        , (n 'w' "+"    (kajar <> Attrs.rim <> open), Key2.fs4)
        -- The Sonic Couture kajar doesn't have this.
        , (n 'e' "P"    (kajar <> Attrs.rim),   Key2.g4)
        -- Soniccouture also has a low kajar variant.
        ]
        where n = Drums.stroke
    open = Attrs.open

gong = Attrs.attr "gong"
kemong = Attrs.attr "kemong"
kempur = Attrs.attr "kempur"
bebende = Attrs.attr "bebende"
wadon = Attrs.attr "wadon"
lanang = Attrs.attr "lanang"
kempli = Attrs.attr "kempli"
kajar = Attrs.attr "kajar"

kebyarAllocations :: Text -> UiConfig.Allocations
kebyarAllocations dev_ = makeConfig $ concat
    [ pasang "jegog"
    , pasang "calung"
    , pasang "penyacah"
    , pasang "pemade"
    , pasang "kantilan"
    , [ umbangPatch "ugal" "ugal"
      , isepPatch "reyong" "reyong"
      , umbangPatch "trompong" "trompong"
      , patch "gong"
      , patch "kempli"
      ]
    ]
    where
    -- (inst, qualified, gets_chan, environ, scale)
    makeConfig :: [(ScoreT.Instrument, InstT.Qualified, Bool,
            [(EnvKey.Key, REnv.Val)], Maybe Patch.Scale)]
        -> UiConfig.Allocations
    makeConfig = MidiInst.allocations . snd . List.mapAccumL allocate 0
        where
        allocate chan (inst, qualified, getsChan, environ, scale) =
            ( nextChan
            , (inst, qualified, setConfig, backend)
            )
            where
            nextChan = if getsChan then chan+1 else chan
            backend
                | getsChan = UiConfig.Midi $
                    Patch.settings#Patch.scale #= scale $
                    MidiInst.config1 dev chan
                -- Pasang instruments don't get an allocation.  Otherwise they
                -- don't have the right tuning.
                | otherwise = UiConfig.Dummy ""
            setConfig = Common.cenviron #= REnv.fromList environ
    dev = Midi.writeDevice dev_

    -- Actually pemade and kantilan have an umbang isep pair for both polos and
    -- sangsih, but since I don't have that many sample sets I have
    -- a mini-ensemble with only one pair of each gangsa.
    pasang name =
        [ ( ScoreT.Instrument name, scQualified (name <> "-pasang")
          , False, polosSangsih name, Nothing
          )
        , umbangPatch (ScoreT.Instrument $ name <> "-p") name
        , isepPatch (ScoreT.Instrument $ name <> "-s") name
        ]
    scQualified name = InstT.Qualified synthName ("sc-" <> name)
    polosSangsih name =
        [ (Gangsa.instPolos, REnv.toVal $ ScoreT.Instrument $ name <> "-p")
        , (Gangsa.instSangsih, REnv.toVal $ ScoreT.Instrument $ name <> "-s")
        ]
    umbangPatch name patch =
        ( name, scQualified patch, True
        , tuning BaliScales.Umbang
        , Just $ Legong.completeInstrumentScale
            Legong.larasRambat BaliScales.Umbang
        )
    isepPatch name patch =
        ( name, scQualified patch, True
        , tuning BaliScales.Isep
        , Just $ Legong.completeInstrumentScale
            Legong.larasRambat BaliScales.Isep
        )
    tuning val = [(EnvKey.tuning, REnv.toVal val)]
    patch name = (ScoreT.Instrument name, scQualified name, True, [], Nothing)
