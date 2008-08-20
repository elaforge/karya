module Derive.Schema_test where

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

import qualified Perform.Midi.Instrument as Instrument

import Util.PPrint

mkid = TestSetup.mkid

ruler = Schema.Track Nothing (Block.RId (Ruler.RulerId (mkid "ruler"))) 0
tid id = Block.TId (Track.TrackId (mkid id)) (Ruler.RulerId (mkid "r1"))
inst n = Schema.Track (Just (">inst" ++ show n)) (tid ("i" ++ show n)) 0
cont name track_id = Schema.Track (Just name) (tid track_id) 0

-- Reduce a skeleton down to a string which is easier to read than the nested
-- data structure.
reduce :: Schema.Skeleton -> String
reduce (Schema.Controller ctracks itrack) =
    Seq.join "," (map reduce_track ctracks)
    ++ "(" ++ maybe "" reduce itrack ++ ")"
reduce (Schema.Instrument _ track) = reduce_track track
reduce (Schema.Merge tracks) = Seq.join " + " (map reduce tracks)

reduce_track = reduce_tracklike . Schema.track_id

reduce_tracklike (Block.TId tid _) = Id.id_name (Id.unpack_id tid)
reduce_tracklike (Block.DId _color) = "DIV"
reduce_tracklike (Block.RId rid) = Id.id_name (Id.unpack_id rid)

-- TODO test with rulers and dividers

test_parse = do
    let eq tracks str = do
            equal (reduce (Schema.default_parser (ruler:tracks))) str
    -- They're both controllers, with no instrument track.
    eq [cont "" "c1", cont "" "c2"] "c1,c2()"
    eq [inst 1] "i1"
    eq [inst 1, cont "control1" "c1"] "c1(i1)"
    eq [cont "tempo" "tempo1", inst 1, cont "tempo" "tempo2"]
        "tempo1(i1) + tempo2()"

    -- orphaned control
    eq [cont "control1" "c1", inst 1] "c1() + i1"

test_compile_to_signals = do
    let parse tracks = Schema.default_parser tracks
        (_tids, state) = TestSetup.run_mkstate
            [ ("tempo", [(0, 0, "2")])
            , (">inst0", [])
            , ("c1", [(0, 0, "3"), (10, 0, "2"), (20, 0, "1")])
            , ("c2", [(0, 0, ".1"), (10, 0, ".2"), (20, 0, ".4")])
            ]
        tracks = either (fail . show) id $ State.eval state $ do
            block <- State.get_block (Block.BlockId (mkid "b1"))
            Schema.block_tracks block
        skel = parse tracks
    let d = Schema.compile_to_signals skel
    -- It's important that the tempo track *doesn't* apply, since these go to
    -- the UI.
    let (res, _, _, logs) = Derive.derive Derive.empty_lookup_deriver state
            (Derive_test.setup_deriver d)
    pprint skel
    pprint res
    -- tempo, c1, and c2 tracks get signals.
    -- I don't verify the signals since it seems to hard at the moment.
    equal (fmap (map fst) res)
        (Right $ map (Track.TrackId . mkid) ["b1.0", "b1.2", "b1.3"])
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
