-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.Util as CUtil
import qualified Cmd.Keymap as Keymap

import qualified Derive.Args as Args
import Derive.Attrs
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Call.Util
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.Instrument.Util as DUtil
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return synth_descs

synth_descs :: [MidiInst.SynthDesc]
synth_descs = MidiInst.make $ (MidiInst.softsynth synth (-12, 12) [])
    { MidiInst.extra_patches = patches }

synth :: Instrument.SynthName
synth = "kkt"

patches :: [MidiInst.Patch]
patches = concat [hang, wayang, kendang, kendang_composite]
    where
    hang = MidiInst.with_code hang_code
        [inst "hang1" hang_ks, inst "hang2" hang_ks]
    wayang =
        [ (Instrument.set_scale wayang_umbang $ inst "wayang-umbang" wayang_ks,
            MidiInst.default_scale Wayang.umbang_id wayang_code)
        , (Instrument.set_scale wayang_isep $ inst "wayang-isep" wayang_ks,
            MidiInst.default_scale Wayang.isep_id wayang_code)
        ]
    kendang = MidiInst.with_code (CUtil.drum_code Drums.kendang_tunggal) $
        map (CUtil.drum_instrument Drums.kendang_tunggal)
            [ inst wadon_name []
            , inst lanang_name []
            ]
    -- kendang-composite is a composite of kendang-wadon and kendang-lanang
    kendang_composite =
        MidiInst.with_code (kendang_composite_code (wadon_inst, lanang_inst))
            [CUtil.drum_instrument notes (inst "kendang-composite" [])]
            -- map (CUtil.drum_instrument notes)
            --     [ inst "kendang-composite" [] ]
        where
        notes = [(note, key) | (note, key, _) <- note_insts]
        note_insts = [(note, key, kendang_inst w)
            | (note, (w, _), key) <- Drums.kendang_composite]
        kendang_inst w = if Maybe.isJust w then wadon_inst else lanang_inst
        wadon_inst = Score.instrument synth wadon_name
        lanang_inst = Score.instrument synth lanang_name

    inst name ks = Instrument.set_keyswitches ks $
        Instrument.patch $ Instrument.instrument name [] (-12, 12)

wadon_name, lanang_name :: String
wadon_name = "kendang-wadon"
lanang_name = "kendang-lanang"

-- * hang

hang_code :: MidiInst.Code
hang_code = MidiInst.empty_code
    { MidiInst.note_calls = [Derive.make_lookup hang_calls]
    , MidiInst.cmds = [hang_cmd]
    }

hang_calls :: Derive.NoteCallMap
hang_calls = Derive.make_calls
    [(text, DUtil.attrs_note attrs) | (attrs, _, Just text, _) <- hang_strokes,
        -- Make sure to not shadow the default "" call.
        not (null text)]

hang_cmd :: Cmd.Cmd
hang_cmd = CUtil.keyswitches [(Keymap.physical_key char, text, key)
    | (_, key, Just text, Just char) <- hang_strokes]

-- | The order is important because it determines attr lookup priority.
hang_strokes :: [(Score.Attributes, Midi.Key, Maybe String, Maybe Char)]
hang_strokes =
    [ (center,  Key.c2,     Just "",            Just 'Z')
    , (edge,    Key.cs2,    Just "`pang2`",     Just 'X')
    , (slap,    Key.d2,     Just "`da3`",       Just 'C')
    , (middle,  Key.ds2,    Just "`zhong1`",    Just 'V')
    , (knuckle, Key.e2,     Just "`zhi3`",      Just 'B')
    , (no_attrs,Key.c2,     Nothing,            Nothing)
    ]

hang_ks :: [(Score.Attributes, Midi.Key)]
hang_ks = [(attrs, key) | (attrs, key, _, _) <- hang_strokes]


-- * gender wayang

wayang_code :: MidiInst.Code
wayang_code = MidiInst.empty_code
    { MidiInst.note_calls = [Derive.make_lookup wayang_calls] }

wayang_calls :: Derive.NoteCallMap
wayang_calls = Derive.make_calls [("", DUtil.note0_attrs muted)]

wayang_ks :: [(Score.Attributes, Midi.Key)]
wayang_ks = [(muted, Key.gs2), (open, Key.g2), (no_attrs, Key.g2)]

wayang_umbang :: Instrument.PatchScale
wayang_umbang =
    Instrument.make_patch_scale $ zip wayang_keys Wayang.note_numbers_umbang

wayang_isep :: Instrument.PatchScale
wayang_isep =
    Instrument.make_patch_scale $ zip wayang_keys Wayang.note_numbers_isep

wayang_keys :: [Midi.Key]
wayang_keys =
    [ Key.a2 -- 6..
    , Key.c3, Key.d3, Key.e3, Key.g3, Key.a3 -- 1. to 6.
    , Key.c4, Key.d4, Key.e4, Key.g4 -- 1 to 5
    ]

-- * kendang

kendang_composite_code :: (Score.Instrument, Score.Instrument) -> MidiInst.Code
kendang_composite_code insts@(wadon, lanang) = MidiInst.empty_code
    { MidiInst.note_calls =
        [Derive.make_lookup $ kendang_composite_calls insts]
    , MidiInst.cmds = [CUtil.inst_drum_cmd note_insts]
    }
    where
    note_insts = [(note, key, kendang_inst w)
        | (note, (w, _), key) <- Drums.kendang_composite]
    kendang_inst w = if Maybe.isJust w then wadon else lanang

type CompositeAttrs = (Maybe Score.Attributes, Maybe Score.Attributes)

kendang_composite_calls :: (Score.Instrument, Score.Instrument)
    -> Derive.NoteCallMap
kendang_composite_calls insts = Derive.make_calls
    [(Drums.note_name n, c_stroke insts attrs)
        | (n, attrs, _) <- Drums.kendang_composite]

c_stroke :: (Score.Instrument, Score.Instrument) -> CompositeAttrs
    -> Derive.NoteCall
c_stroke insts attrs =
    Derive.stream_generator "kendang-stroke" $
    Note.inverting $ \args -> CallSig.call0 args $
        Derive.d_at (Args.start args) $
            emit_stroke insts attrs

-- TODO emit filler strokes
--
-- filler strokes are "^" (pak <> soft) and "." (ka)
--
-- kP+otT kPkP+o+o kPuUtT+o
-- P.+.T^ P.P.+.+. P.o.T^+.
-- .P.+.T .P.P.+.+ .P.O.T^+
--
-- abstraction level is wrong
-- I want to take a stream of kPtT etc. to wadon and lanang streams.
-- Events with attrs is ok too.
-- So maybe this should be postproc instead of a bunch of calls.
--
-- With a bunch of calls I can't reliably tell the previous one.
--
-- So this means kendang-composite is a normal bunch of calls, but then there's
-- a postproc that turns (lanang <> pak) int (lanang, pak)
--
-- Can I make the instrument automatially apply a transformer?  It really does
-- apply only to the instrument since it is designed to work specifically with
-- the events the instrument creates.  But ideally I want to apply it only once
-- at the top level, if >inst now implies a transformation in addition to just
-- setting environment then (>inst a b c) is now different than
-- (>inst a (>inst b c))

-- At fast speeds omit the filler strokes and use de<>thumb.
-- If I omit tempo from track derivation, how do I know it's fast speed?
-- I can put the warp into a plain control and 'tempo_at' check that control
-- instead of the warp.
--
-- But a postproc is a nicer solution for this, I can just check actual
-- RealTime distance.

emit_stroke :: (Score.Instrument, Score.Instrument) -> CompositeAttrs
    -> Derive.EventDeriver
emit_stroke (wadon, lanang) (wadon_attrs, lanang_attrs) =
    emit wadon wadon_attrs <> emit lanang lanang_attrs
    where
    emit _ Nothing = mempty
    emit inst (Just attrs) = Call.Util.add_attrs attrs $
        Derive.with_instrument inst Call.Util.triggered_note
