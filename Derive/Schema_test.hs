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


ruler = Schema.Track Nothing (Block.RId (Ruler.RulerId "ruler")) 0
tid id = Block.TId (Track.TrackId id) (Ruler.RulerId "r1")
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

reduce_tracklike (Block.TId tid _) = Track.un_track_id tid
reduce_tracklike (Block.DId _color) = "DIV"
reduce_tracklike (Block.RId rid) = Ruler.un_ruler_id rid

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
        [ Just (InstrumentTrack (Score.Instrument "inst1"))
        , Just (ControllerTrack [Score.Instrument "inst1"])
        , Just (InstrumentTrack (Score.Instrument "inst2"))
        , Just (ControllerTrack [Score.Instrument "inst2"])
        ]

    return ()
