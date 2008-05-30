module Derive.Schema_test where

import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track

import qualified Derive.Schema as Schema
import Derive.Schema (Skeleton(..), TrackType(..))
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument


tid id = Block.TId (Track.TrackId id) (Ruler.RulerId "r1")
inst n = Schema.Track (Just (">inst" ++ show n)) (tid ("i" ++ show n)) 0
cont name = Schema.Track (Just name) (tid "control_id") 0

reduce :: Schema.Skeleton -> String
reduce (Schema.Controller ctracks itrack) =
    Seq.join "," (map (maybe "_" id . Schema.track_title) ctracks)
    ++ "(" ++ maybe "*" reduce itrack ++ ")"
reduce (Schema.Instrument _ track) = maybe "_" id (Schema.track_title track)
reduce (Schema.Merge tracks) = Seq.join " + " (map reduce tracks)

-- TODO test with rulers and dividers

test_parse = do
    let eq tracks str = equal (reduce (Schema.default_parser tracks)) str
    -- They're both controllers, with no instrument track.
    eq [cont "", cont ""] ",(*)"
    eq [inst 1] ">inst1"
    eq [inst 1, cont "control1"] "control1(>inst1)"
    eq [cont "tempo", inst 1, cont "tempo"] "tempo(>inst1) + tempo()"

    -- orphaned control
    eq [cont "control1", inst 1] "control1() + >inst1"

default_config = Instrument.config [] Nothing

test_get_addr = do
    let inst = Score.Instrument "inst"
    equal (Schema.get_addr default_config inst) Nothing

test_default_cmds = do
    equal 1 1
    return ()


tracknums tracks =
    [t { Schema.track_tracknum = n } | (n, t) <- Seq.enumerate tracks]

test_track_type_of = do
    let types tracks = let skel = Schema.default_parser (tracknums tracks)
            in map (\n -> Schema.track_type_of n skel) [0..length tracks - 1]
        eq tracks expected = equal (types tracks) expected
    eq [cont "", cont ""]
        [Just (ControllerTrack []), Just (ControllerTrack [])]
    print $ types [inst 1, cont "vel", inst 2, cont "vel"]
    eq [inst 1, cont "vel", inst 2, cont "vel"]
        [ Just (InstrumentTrack (Score.Instrument "inst1"))
        , Just (ControllerTrack [Score.Instrument "inst1"])
        , Just (InstrumentTrack (Score.Instrument "inst2"))
        , Just (ControllerTrack [Score.Instrument "inst2"])
        ]

    return ()
