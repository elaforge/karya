-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Instrument.DUtil_test where
import qualified Data.Set as Set

import Util.Test
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Instrument.DUtil as DUtil


test_composite = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks_setup with ""
        extract e = (DeriveTest.e_instrument e, DeriveTest.e_attributes e,
            DeriveTest.e_pitch e,
            DeriveTest.e_control "c1" e, DeriveTest.e_control "c2" e)
        with = CallTest.with_note_generator "a" $ DUtil.redirect_pitch "redir"
            "+pitch" (Just (Set.fromList ["c1"])) "+nopitch" Nothing
        title = "> | redir-pitched = i1 | redir-unpitched = i2"
    let (result, logs) = run
            [ (title, [(0, 1, "a")])
            , ("*", [(0, 0, "4c")])
            , ("c1", [(0, 0, ".5")]), ("c2", [(0, 0, "1")])
            ]
    equal result
        [ ("i1", "+pitch", "4c", [(0, 0.5)], [])
        , ("i2", "+nopitch", "?", [], [(0, 1)])
        ]
    equal logs []
