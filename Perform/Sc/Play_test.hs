-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Sc.Play_test where
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Vivid.OSC as OSC

import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Sc.Note as Note
import qualified Perform.Sc.Play as Play

import           Global
import           Types
import           Util.Test


test_break1 :: Test
test_break1 = do
    let f = Play.break1 fst
    equal (f ([] :: [[(Int, Char)]])) ([], [])
    equal (f [[(0, 'a'), (1, 'b')], [(1, 'c')], [(0, 'd')], [(3, 'e')]])
        ([(0, 'a'), (0, 'd')], [[(1, 'b')], [(1, 'c')], [], [(3, 'e')]])
    equal (Play.rotate_on fst
        [[(0, 'a'), (1, 'b')], [(1, 'c')], [(0, 'd')], [(3, 'e')]])
        [[(0, 'a'), (0, 'd')], [(1, 'b'), (1, 'c')], [(3, 'e')]]

test_notes_to_osc :: Test
test_notes_to_osc = do
    let sid = Play.SynthId
    let f = Play.notes_to_osc (sid 10)
    equal (f []) []
    equal (f notes)
        [ (0, Play.s_new "sine" (sid 10) [(dur, 2), (freq, 400)])
        , (0.5, Play.n_set (sid 10) [(freq, 300)])
        , (1, Play.n_set (sid 10) [(freq, 200)])
        , (1, Play.s_new "sine" (sid 11) [(dur, 2), (freq, 500)])
        , (1.5, Play.n_set (sid 11) [(freq, 600)])
        ]

dur :: Note.ControlId
dur = Note.ControlId 2

freq :: Note.ControlId
freq = Note.ControlId 1

notes :: [Note.Note]
notes =
    [ Note.Note "sine" 0 2 dur
        (mkcontrols [(freq, [(0, 400), (0.5, 300), (1, 200)])])
    , Note.Note "sine" 1 2 dur
        (mkcontrols [(freq, [(1, 500), (1.5, 600)])])
    ]

mkcontrols :: [(Note.ControlId, [(RealTime, Double)])]
    -> Map Note.ControlId MSignal.Signal
mkcontrols = Map.fromList . map (second MSignal.from_pairs)

playN :: IO ()
playN = do
    now <- Time.getCurrentTime
    let oscs = Play.to_bundles now $
            Play.notes_to_osc (Play.SynthId 10) notes
    mapM_ (Play.send Play.server_port . OSC.encodeOSCBundle) oscs
