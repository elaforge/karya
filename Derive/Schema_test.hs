module Derive.Schema_test where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.NoteTrack as NoteTrack

import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score
import qualified Derive.Scale.Relative as Relative

import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal

import qualified Derive.DeriveTest as DeriveTest
import qualified Util.Graph_test as Graph_test


-- * cmds

test_get_track_info = do
    let tree = mk_track_tree
            -- ["c0", ">inst1", "add #", "c1", ">inst2", "add c2"]
            [ node ("c0", 0)
                [ node ("c1", 3) [node ("add #", 2) [node (">inst1", 1) []]]
                , node ("add c2", 5) [node (">inst2", 4) []]
                ]
            ]
        proj_scale = Pitch.ScaleId "proj"
    let tracknums = map Just [0..6] ++ [Nothing]
    let res = map (Schema.get_track_info proj_scale tree) tracknums
    equal (res!!0) (Just Schema.ControlTrack, Just inst1, proj_scale)
    equal (res!!1) (Just (Schema.NoteTrack (NoteTrack.ExistingTrack 2)),
        Just inst1, Relative.scale_id)
    equal (res!!2) (Just Schema.PitchTrack, Just inst1, Relative.scale_id)
    equal (res!!3) (Just Schema.ControlTrack, Just inst1, proj_scale)
    equal (res!!4)
        (Just (Schema.NoteTrack (NoteTrack.CreateTrack 4 "*proj" 5)),
            Just inst2, proj_scale)
    equal (res!!5) (Just Schema.ControlTrack, Just inst2, proj_scale)
    -- Nothing tracknum, and invalid tracknum
    equal (res!!6) (Nothing, Nothing, proj_scale)
    equal (res!!7) (Nothing, Nothing, proj_scale)
    where
    inst1 = Score.Instrument "inst1"
    inst2 = Score.Instrument "inst2"

    mk_track_tree :: Tree.Forest (String, Int) -> State.TrackTree
    mk_track_tree = map (fmap (uncurry mk_track_info))

-- * compile

test_compile = do
    let controls = either Left (Right . map Score.event_controls)
        pitches = either Left (Right . map Score.event_pitch)

    let derive track = DeriveTest.e_logs $ DeriveTest.derive_tracks
            [ ("tempo", [(0, 0, "2")])
            , (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , track
            , ("c1", [(0, 0, "3"), (1, 0, "2"), (2, 0, "1")])
            ]

    let (res, logs) = derive ("*c2", [(0, 0, ".1")])
    strings_like logs ["pitch_call: unknown ScaleId \"c2\""]
    equal res (Right [])

    let cont_signal = Map.union Derive.initial_controls
            (Map.fromList [(Score.Control "c1",
                mksig [(0, 3), (0.5, 2), (1, 1)])])
        no_pitch = PitchSignal.empty

    let (res, logs) = derive ("*twelve", [(0, 0, ".1")])
    strings_like logs ["call not found: .1"]
    equal (controls res) (Right [cont_signal, cont_signal, cont_signal])
    equal (pitches res) (Right [no_pitch, no_pitch, no_pitch])

    let (res, logs) = derive
            ("*twelve", [(0, 0, "4c"), (4, 0, "4d"), (12, 0, "i (4e)")])
    let complete_psig = PitchSignal.signal (Pitch.ScaleId "twelve")
            ([(0, (60, 60, 0)), (2, (62, 62, 0))]
                ++ DeriveTest.pitch_interpolate 2 62 6 64)
    let psig trunc = PitchSignal.truncate trunc complete_psig

    equal logs []
    -- The pitch signal gets truncated so it doesn't look like the note's decay
    -- wants to change pitch.
    equal (controls res) (Right [cont_signal, cont_signal, cont_signal])
    equal (pitches res) (Right [psig 1, psig 2, psig 6])
    where
    mksig = Signal.signal

-- * parse

test_parse = do
    let mktracks titles =
            [mk_track_info name n | (n, name) <- Seq.enumerate titles]
    let mkskel = Skeleton.make
    let f = Schema.default_parser . mktracks

    -- They're both controls, with no instrument track.
    skel_equal (f ["", ""]) (mkskel [(0, 1)])
    skel_equal (f [">i1"]) (mkskel [])
    skel_equal (f [">i1", "c1", "c2"]) (mkskel [(2, 1), (1, 0)])
    skel_equal (f ["c1", ">i1", "c2"]) (mkskel [(0, 2), (2, 1)])
    skel_equal
        (f ["c1", "tempo", "c2", ">i1", "c3", "tempo", ">i2", "c4"])
        (mkskel [(1, 2), (2, 4), (4, 3), (5, 7), (7, 6)])
    where
    skel_equal (Skeleton.Skeleton g1) (Skeleton.Skeleton g2) =
        Graph_test.graph_equal g1 g2

-- * util

node = Tree.Node
mk_track_info title tracknum =
    State.TrackInfo title (UiTest.tid "fake") tracknum
