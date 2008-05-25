module Derive.Schema_test where

import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track

import qualified Derive.Schema as Schema


tid id = Block.TId (Track.TrackId id) (Ruler.RulerId "r1")
inst n = Schema.Track (Just (">inst" ++ show n)) (tid ("i" ++ show n))
cont name = Schema.Track (Just name) (tid "control_id")

reduce :: Schema.Skeleton -> String
reduce (Schema.Controller ctracks itrack) =
    Seq.join " " (map (maybe "_" id . Schema.track_title) ctracks)
    ++ "(" ++ maybe "" reduce itrack ++ ")"
reduce (Schema.Instrument track) = maybe "_" id (Schema.track_title track)
reduce (Schema.Merge tracks) = Seq.join " + " (map reduce tracks)

-- TODO test with rulers and dividers

test_parse = do
    let eq tracks str = equal (reduce (Schema.parse tracks)) str
    eq [inst 1] ">inst1"
    eq [inst 1, cont "control1"] "control1(>inst1)"
    eq [cont "tempo", inst 1, cont "tempo"] "tempo(>inst1) + tempo()"

    -- orphaned control
    eq [cont "control1", inst 1] "control1() + >inst1"
