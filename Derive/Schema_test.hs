module Derive.Schema_test where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal

import qualified Derive.DeriveTest as DeriveTest
import qualified Util.Graph_test as Graph_test


-- * compile

test_compile = do
    let controls = map Score.event_controls
        pitches = map Score.event_pitch

    let derive track = DeriveTest.extract id $ DeriveTest.derive_tracks
            [ ("tempo", [(0, 0, "2")])
            , track
            , (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("c1", [(0, 0, "3"), (1, 0, "2"), (2, 0, "1")])
            ]

    let (_, logs) = derive ("*c2", [(0, 0, ".1")])
    strings_like logs ["unknown ScaleId \"c2\""]

    let mkcont vals = Map.union Derive.initial_controls
            (Map.singleton (Score.Control "c1") (mksig vals))
        no_pitch = PitchSignal.empty

    let (events, logs) = derive ("*twelve", [(0, 0, ".1")])
    strings_like logs ["call not found: .1"]
    equal (controls events)
        [mkcont [(0, 3)], mkcont [(0.5, 2)], mkcont [(1, 1)]]
    equal (pitches events) [no_pitch, no_pitch, no_pitch]

    let (events, logs) = derive
            ("*twelve", [(0, 0, "4c"), (4, 0, "4d"), (12, 0, "i (4e)")])
    let complete_psig = PitchSignal.signal (Pitch.ScaleId "twelve")
            ([(0, (60, 60, 0)), (2, (62, 62, 0))]
                ++ DeriveTest.pitch_interpolate 2 62 6 64)
    let psig trunc = PitchSignal.truncate trunc complete_psig

    equal logs []
    -- The pitch signal gets truncated so it doesn't look like the note's decay
    -- wants to change pitch.
    equal (pitches events) [psig 1, psig 2, psig 6]
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
    skel_equal (f [">i1", "c1", "c2"]) (mkskel [(0, 1), (1, 2)])

    skel_equal (f ["c1", ">i1", "c2"]) (mkskel [(0, 1), (1, 2)])
    skel_equal (f ["c0", ">i1", "c1", ">i2", "tempo", ">i3"])
        (mkskel [(0, 1), (1, 2), (0, 3), (4, 5)])
    skel_equal (f [">i1", "c1", ">i2", "c2"])
        (mkskel [(0, 1), (2, 3)])
    where
    skel_equal (Skeleton.Skeleton g1) (Skeleton.Skeleton g2) =
        Graph_test.graph_equal g1 g2

-- * util

node = Tree.Node
mk_track_info title tracknum =
    State.TrackInfo title (UiTest.tid "fake") tracknum
