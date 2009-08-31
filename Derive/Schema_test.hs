module Derive.Schema_test where
import qualified Control.Arrow as Arrow
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.NoteTrack as NoteTrack

import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score

import qualified Instrument.MidiDb as MidiDb

import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller

import qualified Derive.Derive_test as Derive_test
import qualified Util.Graph_test as Graph_test

import qualified Util.Tree


-- * cmds

node = Tree.Node
tid id = Track.TrackId (UiTest.mkid id)
fake_tid = tid "fake"
mk_track_info title tracknum = State.TrackInfo title fake_tid tracknum

mk_track_tree :: Tree.Forest (String, Int) -> State.TrackTree
mk_track_tree = map (fmap (uncurry mk_track_info))

mkalloc insts = Map.fromList
    [ (Score.Instrument inst, [(Midi.WriteDevice inst, fromIntegral chan)])
    | (chan, inst) <- Seq.enumerate insts]

midi_default_inst = Instrument.instrument
    synth "default" Nothing Controller.default_controllers (-12, 12)
midi_inst1 = (Instrument.instrument
    synth "inst1" Nothing Controller.default_controllers (-12, 12))
    { Instrument.inst_scale = i1scale }
inst1 = Score.Instrument "inst1"
inst2 = Score.Instrument "inst2"
default_inst = Score.Instrument "default_inst"
synth = Instrument.synth "synth" "synth" []
empty_scale = Pitch.ScaleId ""
i1scale = Pitch.ScaleId "i1"

lookup_midi :: MidiDb.LookupMidiInstrument
lookup_midi attrs inst
    | inst == default_inst = Just midi_default_inst
    | inst == inst1 = Just midi_inst1
    | otherwise = Nothing

test_get_defaults = do
    let iconfig = Instrument.Config (mkalloc ["default", "inst1"])
                (Just (Score.Instrument "default"))
        tree = mk_track_tree -- [">inst2", "*", ">inst1"]
                [ node ("*", 1) [node (">inst2", 0) []]
                , node (">inst1", 2) []
                ]
        mkcontext focused = Schema.cmd_context iconfig lookup_midi Cmd.NoEdit
            False focused tree
    let tracknums = map Just [0..3] ++ [Nothing]
    let res = map Schema.get_defaults (map mkcontext tracknums)
    equal (res!!0) (Just (Schema.NoteTrack (NoteTrack.PitchTrack False 1))
        , Nothing, empty_scale, Nothing)
    equal (res!!1) (Just Schema.PitchTrack, Nothing, empty_scale, Nothing)
    equal (res!!2) (Just (Schema.NoteTrack (NoteTrack.PitchTrack True 3))
        , Just midi_inst1, i1scale, Just (Midi.WriteDevice "inst1", 1))
    -- no tracknum, and out of range tracknum
    let def_inst = Just (Midi.WriteDevice "default", 0)
    equal (res!!3) (Nothing, Nothing, Instrument.default_scale, def_inst)
    equal (res!!4) (Nothing, Nothing, Instrument.default_scale, def_inst)

test_get_track_info = do
    let tree = mk_track_tree -- ["c0", ">inst1", "*", "c1", ">inst2", "c2"]
                [ node ("c0", 0)
                    [ node ("c1", 3) [node ("*", 2) [node (">inst1", 1) []]]
                    , node ("c2", 5) [node (">inst2", 4) []]
                    ]
                ]
    let tracknums = map Just [0..6] ++ [Nothing]
    let res = map (Schema.get_track_info tree) tracknums
    equal (res!!0) (Just Schema.ControlTrack, Just inst1, Just empty_scale)
    equal (res!!1) (Just (Schema.NoteTrack (NoteTrack.PitchTrack False 2))
        , Just inst1, Just empty_scale)
    equal (res!!2) (Just Schema.PitchTrack, Just inst1, Just empty_scale)
    equal (res!!3) (Just Schema.ControlTrack, Just inst1, Just empty_scale)
    equal (res!!4) (Just (Schema.NoteTrack (NoteTrack.PitchTrack True 5))
        , Just inst2, Nothing)
    equal (res!!5) (Just Schema.ControlTrack, Just inst2, Nothing)
    -- Nothing tracknum, and invalid tracknum
    equal (res!!6) (Nothing, Nothing, Nothing)
    equal (res!!7) (Nothing, Nothing, Nothing)


-- * compile

mksig = Signal.track_signal Signal.default_srate

test_compile = do
    let set = Signal.Set
        derive track = Arrow.first extract $
            derive_with_pitch Schema.compile track
        extract = either (Left . Derive.error_message)
            (Right . (map Score.event_controllers))

    let (res, logs) = derive ("*c2", [(0, 0, ".1")])
    equal logs []
    equal res $ Left ("compile: unknown ScaleId \"c2\"")

    let cont_signal = (Score.Controller "c1",
            mksig [(0, set, 3), (5, set, 2), (10, set, 1)])
        no_pitch = (Score.Controller "*twelve", mksig [])

    let (res, logs) = derive ("*twelve", [(0, 0, ".1")])
    equal logs ["compile: Note \".1\" not in ScaleId \"twelve\""]
    equal res $ Right (replicate 3 (Map.fromList [no_pitch, cont_signal]))

    -- TODO so here they all have the *scale controller, but I can't test
    -- further until I remove the pitch stuff from the note parser
    let (res, logs) = derive
            ("*twelve", [(0, 0, "4c-"), (10, 0, "4d-"), (20, 0, "i, 4e-")])
    let pitch_signal = (Score.Controller "*twelve",
            mksig [(0, set, 48), (5, set, 50), (10, Signal.Linear, 52)])
    equal logs []
    equal res $ Right (replicate 3 (Map.fromList [pitch_signal, cont_signal]))

test_compile_to_signals = do
    let derive track = Arrow.first extract $
            derive_with_pitch Schema.compile_to_signals track
        extract = either (Left . Derive.error_message) (Right . id)

    let (res, logs) = derive ("*bogus", [])
    equal logs []
    equal res (Left "compile_to_signals: unknown ScaleId \"bogus\"")

    let (_res, logs) = derive ("*twelve", [(10, 0, ".2")])
    equal logs ["compile_to_signals: Note \".2\" not in ScaleId \"twelve\""]

    let (res, logs) = derive
            ("*twelve", [(0, 0, "4c-"), (10, 0, "4d-"), (20, 0, "i, 4e-")])
    equal logs []
    -- tempo, c1, and pitch tracks get signals.
    equal (fmap (map fst) res)
        (Right $ map (Track.TrackId . mkid) ["b1.t0", "b1.t3", "b1.t2"])

    -- It's important that the tempo track *doesn't* apply, since these go to
    -- the UI.
    let set = Signal.Set
    equal (fmap (map snd) res) $ Right
        [ mksig [(0, set, 2)]
        , mksig [(0, set, 48), (10, set, 50), (20, Signal.Linear, 52)]
        , mksig [(0, set, 3), (10, set, 2), (20, set, 1)]
        ]

derive_with_pitch compiler pitch_track = (res, map Log.msg_text logs)
    where
    (state, track_tree) = mkstate_with_pitch pitch_track
    (res, _, _, logs, _) = Derive.derive Derive.empty_lookup_deriver
        state True (Derive_test.setup_deriver (compiler track_tree))
    mkstate_with_pitch pitch_track = (state, track_tree)
        where
        (tids, state) = UiTest.run_mkstate
            [ ("tempo", [(0, 0, "2")])
            , (">inst0", [(0, 5, ""), (10, 5, ""), (20, 5, "")])
            , ("c1", [(0, 0, "3"), (10, 0, "2"), (20, 0, "1")])
            , pitch_track
            ]
        track title tracknum = State.TrackInfo title (tids!!tracknum) tracknum
        track_tree =
            [ node (track "tempo" 0)
                [ node (track (fst pitch_track) 3)
                    [ node (track "c1" 2) [node (track ">inst0" 1) []]]
                ]
            ]

mkid = UiTest.mkid

-- * parse

skel_equal (Skeleton.Skeleton g1) (Skeleton.Skeleton g2) =
    Graph_test.graph_equal g1 g2

test_parse = do
    let mktracks titles =
            [mk_track_info name n | (n, name) <- Seq.enumerate titles]
    let n = Tree.Node
        mkskel = Skeleton.make
    let f = Schema.default_parser . mktracks

    -- They're both controllers, with no instrument track.
    skel_equal (f ["", ""]) (mkskel [(0, 1)])
    skel_equal (f [">i1"]) (mkskel [])
    skel_equal (f [">i1", "c1", "c2"]) (mkskel [(2, 1), (1, 0)])
    skel_equal (f ["c1", ">i1", "c2"]) (mkskel [(0, 2), (2, 1)])
    skel_equal
        (f ["c1", "tempo", "c2", ">i1", "c3", "tempo", ">i2", "c4"])
        (mkskel [(1, 2), (2, 4), (4, 3), (5, 7), (7, 6)])
