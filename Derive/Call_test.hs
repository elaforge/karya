module Derive.Call_test where
import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.Test

import qualified Cmd.Cmd as Cmd
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Instrument.Util as Util
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified App.MidiInst as MidiInst


test_assign_controls = do
    let run inst_title cont_title val = extract $ DeriveTest.derive_tracks
            [ (cont_title, [(0, 0, val)])
            , ("*twelve", [(0, 0, "4c")])
            , (inst_title, [(0, 1, "")])
            ]
        extract = DeriveTest.extract e_event
        e_event e = (head (DeriveTest.e_pitch e),
            head <$> DeriveTest.e_control "cont" e)

    -- normal
    equal (run ">i" "cont" "1")
        ([((0, 60), Just (0, 1))], [])
    -- not seen
    equal (run ">i" "gont" "1")
        ([((0, 60), Nothing)], [])

    -- a non-existent control with no default is an error
    let (events, logs) = run ">i | %cont = %bonk" "gont" "1"
    equal events []
    strings_like logs ["not found: Control \"bonk\""]
    -- control assigned
    equal (run ">i | %cont = %gont" "gont" "1")
        ([((0, 60), Just (0, 1))], [])

    -- set a constant signal
    equal (run ">i | %cont = 42" "gont" "1")
        ([((0, 60), Just (0, 42))], [])
    -- set constant signal with a default
    equal (run ">i | %cont = %gont,42" "bonk" "1")
        ([((0, 60), Just (0, 42))], [])
    equal (run ">i | %cont = %gont,42" "gont" "1")
        ([((0, 60), Just (0, 1))], [])

    -- named pitch doesn't show up
    equal (run ">i" "*twelve #foo" "2c")
        ([((0, 60), Nothing)], [])
    -- assigned to default pitch, so it shows up
    equal (run ">i | # = #foo" "*twelve #foo" "2c")
        ([((0, 36), Nothing)], [])
    -- set constant pitch
    equal (run ">i | # = (1c)" "*twelve #foo" "2c")
        ([((0, 24), Nothing)], [])

test_environ_across_tracks = do
    let run tracks = DeriveTest.extract (DeriveTest.e_control "cont") $
            DeriveTest.derive_tracks ((">", [(0, 10, "")]) : tracks)

    -- first make sure srate works as I expect
    let interpolated = [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]
    equal (run [("cont", [(0, 0, "0"), (4, 0, "i 1")])])
        ([Just interpolated], [])
    equal (run [("cont | srate = 2", [(1, 0, "0"), (5, 0, "i 1")])])
        ([Just [(1, 0), (3, 0.5), (5, 1)]], [])

    -- now make sure srate in one track doesn't affect another
    let cont = ("cont", [(0, 0, "0"), (4, 0, "i 1")])
    equal (run [("cont2 | srate = 2", []), cont])
        ([Just interpolated], [])
    equal (run [cont, ("cont2 | srate = 2", [])])
        ([Just interpolated], [])

test_call_errors = do
    let extract r = case DeriveTest.extract DeriveTest.e_event r of
            (val, []) -> Right val
            (_, logs) -> Left $ Seq.join "\n" logs

    let run_title title = extract $
            DeriveTest.derive_tracks [(title, [(0, 1, "--1")])]
    left_like (run_title ">i | no-such-call") "call not found: no-such-call"
    left_like (run_title ">i | delay *bad-arg") "expected Control but got"
    left_like (run_title ">i | delay 1 2 3 4") "too many arguments"
    left_like (run_title ">i | delay") "not found and no default"
    left_like (run_title ">i | delay _") "not found and no default"
    left_like (run_title ">i | delay %delay") "not found and no default"

    let run_evt evt = extract $
            DeriveTest.derive_tracks [(">i", [(0, 1, evt)])]
    left_like (run_evt "no-such-call")
        "call not found: no-such-call"
    left_like (run_evt "tr")
        "non-generator in generator position: trill"
    let tr_result = extract $ DeriveTest.derive_tracks
            [(">", [(0, 4, "")]), ("*twelve", [(0, 0, "tr")])]
    left_like tr_result "ArgError: too few arguments"
    equal (run_evt "delay 2 | tr 2 |")
        (Right [(2, 1, "delay 2 | tr 2 |")])

test_val_call = do
    let extract = DeriveTest.extract (DeriveTest.e_control "cont")
    let run evt = extract $ DeriveTest.derive_tracks_with with_add1
            [(">", [(0, 1, "")]), ("cont", [(0, 0, evt)])]
        with_add1 = CallTest.with_val_call "add1" add_one
    equal (run "foobar")
        ([Just []], ["Error: control call not found: foobar"])
    equal (run "set 1")
        ([Just [(0, 1)]], [])
    equal (run "set (add1 1)")
        ([Just [(0, 2)]], [])
    equal (run "set (add1 (add1 1))")
        ([Just [(0, 3)]], [])
    let (res, logs) = run "set (add1 1 2)"
    equal res [Just []]
    strings_like logs ["too many arguments"]
    where
    add_one :: Derive.ValCall
    add_one = Derive.ValCall "add" $ \args -> CallSig.call1 args
        (CallSig.required "v") $ \val -> return (TrackLang.num (val + 1))

test_inst_call = do
    let extract = DeriveTest.extract (Score.attrs_list . Score.event_attributes)
    let run inst = extract $ DeriveTest.derive_tracks_with
            (set_inst_calls lookup_inst)
            [(inst, [(0, 1, "sn")])]
    equal (run ">s/1")
        ([], ["Error: note call not found: sn"])
    equal (run ">s/with-call")
        ([["snare"]], [])

test_recursive_call = do
    let extract = DeriveTest.extract DeriveTest.e_event
    let result = extract $ DeriveTest.derive_tracks_with with_recur
            [(">", [(0, 1, "recur")])]
        with_recur = CallTest.with_note_call "recur" recursive
    equal result ([], ["Error: call stack too deep: recursive"])
    where
    recursive :: Derive.NoteCall
    recursive = Derive.stream_generator "recursive" $
        \args -> Call.reapply args [TrackLang.call "recur" []]

test_repeat = do
    let run events = DeriveTest.e_logs $
            DeriveTest.derive_tracks_with with_show [(">", events)]
        with_show = CallTest.with_note_call "show" c_show
    equal (run [(0, 1, "\"")]) ["Error: note call not found: \""]
    equal (run [(0, 1, "show 1"), (1, 1, "\""), (2, 1, "\" 2")])
        ["[1]", "[1]", "[2]"]
    where
    c_show = Derive.stream_generator "show" $ \args -> do
        Log.warn $ Pretty.pretty (Derive.passed_vals args)
        return []

patch = Instrument.set_keymap [(Attrs.snare, 42)] $
    Instrument.patch (Instrument.instrument "with-call" [] (-1, 1))
(midi_db, _) = MidiDb.midi_db sdescs
    where
    sdescs = MidiInst.make $ (MidiInst.softsynth "s" (Just "wdev") (-2, 2) [])
        { MidiInst.extra_patches = [(patch, code)] }
    code = MidiInst.empty_code
        { MidiInst.note_calls = [Derive.make_lookup calls] }
    calls = Derive.make_calls [("sn", Util.with_attrs Attrs.snare)]
lookup_inst = fmap (Cmd.inst_calls . MidiDb.info_code)
    . MidiDb.lookup_instrument midi_db

set_inst_calls :: (Score.Instrument -> Maybe Derive.InstrumentCalls)
    -> Derive.Deriver d -> Derive.Deriver d
set_inst_calls calls deriver = do
    Derive.modify $ \st -> st { Derive.state_constant =
        (Derive.state_constant st) { Derive.state_instrument_calls = calls } }
    deriver
