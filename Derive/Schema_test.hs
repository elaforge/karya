module Derive.Schema_test where
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.TestSetup as TestSetup
import qualified Ui.Track as Track

import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema
import Derive.Schema (Skeleton(..), TrackType(..))
import qualified Derive.Score as Score

import qualified Derive.Derive_test as Derive_test

import qualified Perform.Signal as Signal
import qualified Perform.Midi.Instrument as Instrument

import Util.PPrint

mkid = TestSetup.mkid

ruler = Schema.Track Nothing (Block.RId (Ruler.RulerId (mkid "ruler"))) 0
tid id = Block.TId (Track.TrackId (mkid id)) (Ruler.RulerId (mkid "r1"))
inst n = Schema.Track (Just (">inst" ++ show n)) (tid ("i" ++ show n)) 0
cont name track_id = Schema.Track (Just name) (tid track_id) 0

-- | Reduce a skeleton down to a string which is easier to read than the nested
-- data structure.
reduce :: Schema.Skeleton -> String
reduce (Schema.SkelController ctracks itrack) =
    Seq.join "," (map reduce_track ctracks)
    ++ "(" ++ maybe "" reduce itrack ++ ")"
reduce (Schema.SkelNote track) = reduce_track track
reduce (Schema.SkelMerge tracks) = Seq.join " + " (map reduce tracks)

reduce_track = reduce_tracklike . Schema.track_id

reduce_tracklike (Block.TId tid _) = Id.id_name (Id.unpack_id tid)
reduce_tracklike (Block.DId _color) = "DIV"
reduce_tracklike (Block.RId rid) = Id.id_name (Id.unpack_id rid)

-- TODO test with rulers and dividers

test_parse = do
    let reducet tracks = reduce (Schema.default_parser (ruler:tracks))
    -- They're both controllers, with no instrument track.
    equal (reducet [cont "" "c1", cont "" "c2"]) "c1,c2()"
    equal (reducet [inst 1]) "i1"
    equal (reducet [inst 1, cont "control1" "c1"]) "c1(i1)"
    equal (reducet [cont "tempo" "tempo1", inst 1, cont "tempo" "tempo2"])
        "tempo1(i1) + tempo2()"

    -- orphaned control
    equal (reducet [cont "control1" "c1", inst 1]) "c1() + i1"

tracks_from_state state = either (fail . show) id $ State.eval state $ do
    block <- State.get_block (Block.BlockId (mkid "b1"))
    Schema.block_tracks block

test_compile_skeleton = do
    let get_skel state = Schema.default_parser (tracks_from_state state)
        mk pitch_track = (state, get_skel state)
            where
            (_tids, state) = TestSetup.run_mkstate
                [ ("tempo", [(0, 0, "2")])
                , (">inst0", [(0, 5, ""), (10, 5, ""), (20, 5, "")])
                , ("c1", [(0, 0, "3"), (10, 0, "2"), (20, 0, "1")])
                , pitch_track
                ]
        set = Signal.Set
        mksig = Signal.track_signal Signal.default_srate
        cont_signal = (Score.Controller "c1",
            mksig [(0, set, 3), (5, set, 2), (10, set, 1)])
        no_pitch = (Score.Controller "*twelve", mksig [])
        derive pitch_track = (extract res, map Log.msg_text logs)
            where
            extract = either (Left . Derive.error_message)
                (Right . (map Score.event_controllers))
            (state, skel) = mk pitch_track
            d = Schema.compile_skeleton skel
            (res, _, _, logs, _) = Derive.derive Derive.empty_lookup_deriver
                state True (Derive_test.setup_deriver d)

    let (res, logs) = derive ("*c2", [(0, 0, ".1")])
    equal logs []
    equal res $ Left ("unknown scale ScaleId \"c2\"")

    let (res, logs) = derive ("*twelve", [(0, 0, ".1")])
    equal logs ["note Note \".1\" not in scale ScaleId \"twelve\""]
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
    let (_tids, state) = TestSetup.run_mkstate
            [ ("tempo", [(0, 0, "2")])
            , (">inst0", [])
            , ("c1", [(0, 0, "3"), (10, 0, "2"), (20, 0, "1")])
            , ("*c2", [(0, 0, ".1"), (10, 0, ".2"), (20, 0, ".4")])
            ]
        skel = Schema.default_parser (tracks_from_state state)
    let d = Schema.compile_to_signals skel
    let (res, _, _, logs, _) = Derive.derive Derive.empty_lookup_deriver state
            True (Derive_test.setup_deriver d)

    -- tempo, c1, and c2 tracks get signals.
    -- I don't verify the signals since it seems too hard at the moment.
    equal (fmap (map fst) res)
        (Right $ map (Track.TrackId . mkid) ["b1.t0", "b1.t2", "b1.t3"])

    -- It's important that the tempo track *doesn't* apply, since these go to
    -- the UI.
    let set = Signal.Set
        mksig = Signal.track_signal Signal.default_srate
    equal (fmap (map snd) res) $ Right
        [ mksig [(0, set, 2)]
        , mksig [(0, set, 3), (10, set, 2), (20, set, 1)]
        , mksig [(0, set, 0.1), (10, set, 0.2), (20, set, 0.4)]
        ]
    equal logs []

default_config = Instrument.config [] Nothing

test_default_cmds = do
    equal 1 1
    return ()


tracknums tracks =
    [t { Schema.track_tracknum = n } | (n, t) <- Seq.enumerate tracks]

test_track_type_of = do
    -- Make sure to properly ignore the 0th ruler track.
    let types tracks =
            map (\n -> Schema.track_type_of n skel) [1..length tracks - 1]
            where skel = Schema.default_parser (tracknums tracks)
        eq tracks expected = equal (types (ruler:tracks)) expected
    eq [cont "" "c1", cont "" "c2"]
        [Just (ControllerTrack []), Just (ControllerTrack [])]
    print $ types [inst 1, cont "vel" "c1", inst 2, cont "vel" "c2"]
    eq [inst 1, cont "vel" "c1", inst 2, cont "vel" "c2"]
        [ Just (NoteTrack (Score.Instrument "inst1"))
        , Just (ControllerTrack [Score.Instrument "inst1"])
        , Just (NoteTrack (Score.Instrument "inst2"))
        , Just (ControllerTrack [Score.Instrument "inst2"])
        ]

    return ()
