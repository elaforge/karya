-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Sc.Convert_test where
import qualified Data.Map as Map

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Sc.Convert as Convert
import qualified Perform.Sc.Note as Note
import qualified Perform.Sc.Patch as Patch

import           Global
import           Types
import           Util.Test


test_convert :: Test
test_convert = do
    let f = Convert.convert_event 1 patch . DeriveTest.mkevent
    equal (f (2, 2, "4c", [("c", [(0, 0), (8, 8)])], "inst"))
        [ LEvent.Event $ mknote 2 2
            [(cc, [(2, 2), (3, 3), (4, 4)]), (cpitch, [(2, 60)])]
        ]

mknote :: RealTime -> RealTime -> [(Note.ControlId, [(RealTime, Double)])]
    -> Note.Note
mknote start dur controls = Note.Note
    { patch = "patch"
    , start = start
    , duration = dur
    , duration_control = Note.ControlId 1
    , controls = mkcontrols controls
    }

mkcontrols :: [(Note.ControlId, [(RealTime, Double)])]
    -> Map Note.ControlId MSignal.Signal
mkcontrols = Map.fromList . map (second MSignal.from_pairs)

cc :: Note.ControlId
cc = Note.ControlId 2

cpitch :: Note.ControlId
cpitch = Note.ControlId 3

patch = Patch.Patch
    { name = "patch"
    , filename = "patch"
    , duration_control = Note.ControlId 1
    , controls = Map.fromList [("c", cc), ("pitch", cpitch)]
    }
