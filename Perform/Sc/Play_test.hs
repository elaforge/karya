-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Sc.Play_test where
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified Vivid.OSC as OSC

import qualified Util.Seq as Seq
import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Sc.Note as Note
import qualified Perform.Sc.Play as Play

import           Global
import           Types
import           Util.Test


test_notes_to_osc :: Test
test_notes_to_osc = do
    let f = LEvent.events_of . Play.notes_to_osc (nid 10) . map LEvent.Event
    equal (f []) []
    equal (f notes)
        [ (0, Play.s_new "sine" (nid 10) [(pitch, 400)])
        , (0.5, Play.n_set (nid 10) [(pitch, 300)])
        , (1, Play.n_set (nid 10) [(pitch, 200)])
        , (1, Play.s_new "sine" (nid 11) [(pitch, 500)])
        , (1.5, Play.n_set (nid 11) [(pitch, 600)])
        ]

    equal (f
        [mknote 0 [(gate, [(0, 1), (4, 0)]), (pitch, [(0, 50), (2, 48)])]])
        [ (0, Play.s_new "sine" (nid 10) [(gate, 1), (pitch, 50)])
        , (2, Play.n_set (nid 10) [(pitch, 48)])
        , (4, Play.n_set (nid 10) [(gate, 0)])
        ]

test_control_oscs :: Test
test_control_oscs = do
    let f = Play.control_oscs (nid 10) . mkcontrols
        set = Play.n_set (nid 10)
    equal (f [(gate, [(0, 1), (4, 0)]), (pitch, [(0, 40), (2, 42)])])
        [ (0, set [(gate, 1), (pitch, 40)])
        , (2, set [(pitch, 42)])
        , (4, set [(gate, 0)])
        ]

test_streaming :: Test
test_streaming = do
    let f = Play.to_bundles now . Play.notes_to_osc (nid 10) . map LEvent.Event
        now = Time.UTCTime (toEnum 0) 0
    let endless = map (\t -> mknote t [(pitch, [(t, 400)])]) (Seq.range_ 0 1)
    -- If it's not streaming, this would hang.
    equal (length (take 10 (f endless))) 10

nid :: Int.Int32 -> Play.NodeId
nid = Play.NodeId

gate :: Note.ControlId
gate = Note.ControlId 0

pitch :: Note.ControlId
pitch = Note.ControlId 1

mknote :: RealTime -> [(Note.ControlId, [(RealTime, Double)])] -> Note.Note
mknote start controls = Note.Note "sine" start (mkcontrols controls)

notes :: [Note.Note]
notes =
    [ mknote 0 [(pitch, [(0, 400), (0.5, 300), (1, 200)])]
    , mknote 1 [(pitch, [(1, 500), (1.5, 600)])]
    ]

mkcontrols :: [(Note.ControlId, [(RealTime, Double)])]
    -> Map Note.ControlId MSignal.Signal
mkcontrols = Map.fromList . map (second MSignal.from_pairs)

playN :: IO ()
playN = do
    now <- Time.getCurrentTime
    let oscs = LEvent.events_of $ Play.to_bundles now $
            Play.notes_to_osc (Play.NodeId 10) $ map LEvent.Event notes
    mapM_ (Play.send Play.server_port . OSC.encodeOSCBundle) oscs
