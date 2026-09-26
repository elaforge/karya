-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Instrument.DUtil_test where
import qualified Data.Set as Set

import qualified Util.Lists as Lists
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Instrument.DUtil as DUtil

import qualified Perform.NN as NN

import           Util.Test


test_composite :: Test
test_composite = do
    let run = DeriveTest.extract extract
            . DeriveTest.deriveTracksSetup with ""
        extract e =
            ( DeriveTest.eInstrument e, DeriveTest.eAttributes e
            , DeriveTest.ePitch e
            , DeriveTest.eControl "c1" e, DeriveTest.eControl "c2" e
            )
        with = CallTest.withNoteGenerator "a" $
            DUtil.redirectPitch "redir"
                "+pitch" (Set.fromList ["c1"])
                "+nopitch" (Set.fromList ["c2"])
        title = "> | redir-pitched = i1 | redir-unpitched = i2"
    let (result, logs) = run
            [ (title, [(0, 1, "a")])
            , ("*", [(0, 0, "4c")])
            , ("c1", [(0, 0, ".5")])
            , ("c2", [(0, 0, "1")])
            ]
    equal result
        [ ("i1", "+pitch", "4c", [(0, 0.5)], [])
        , ("i2", "+nopitch", "?", [], [(0, 1)])
        ]
    equal logs []

test_constant_controls :: Test
test_constant_controls = do
    let run extract pitch controls tracks = DeriveTest.extract extract $
            DeriveTest.deriveTracksSetup (with pitch controls) "" $
                (">", [(0, 4, "a")]) : tracks
        eNn = Lists.dropDups id . map snd . DeriveTest.eNns
        eC = DeriveTest.eControlVals "c"
        with pitch controls = CallTest.withNoteGenerator "a" $
            DUtil.constantControls pitch (Set.fromList controls)
    equal (run eNn False [] [("*", [(0, 0, "4c"), (2, 0, "4d")])])
        ([[NN.c4, NN.d4]], [])
    equal (run eNn True [] [("*", [(0, 0, "4c"), (2, 0, "4d")])])
        ([[NN.c4]], [])
    equal (run eNn False [] [("*", [(0, 0, "4c")]), ("t-dia", [(2, 0, "1")])])
        ([[NN.c4, NN.d4]], [])
    equal (run eNn True [] [("*", [(0, 0, "4c")]), ("t-dia", [(2, 0, "1")])])
        ([[NN.c4]], [])

    equal (run eC False [] [("c", [(0, 0, "1"), (2, 0, "2")])])
        ([[1, 2]], [])
    equal (run eC False ["c"] [("c", [(0, 0, "1"), (2, 0, "2")])])
        ([[1]], [])
