-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Call.Bali.Reyong_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Call.Bali.Reyong as Reyong
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Environ as Environ
import qualified Derive.Score as Score


test_kilitan = do
    let run notes = group_voices $ DeriveTest.extract extract $
            DeriveTest.derive_tracks $
            UiTest.note_spec (" | scale = legong", notes, [])
        extract e = (DeriveTest.e_environ_val Environ.voice e :: Maybe Int,
            (Score.event_start e, DeriveTest.e_pitch e))
        group_voices = first (lookup (Just 1) . Seq.group_fst)
    equal (run [(0, 0, "X --"), (1, 0, "O --"), (2, 0, "+ --")])
        (Just [(0, "3u"), (1, "3e"), (1, "3a"), (2, "3e"), (2, "3a")], [])
    equal (run [(4, -4, ">kilit -- 3u"), (8, -4, "kilit -- 3u")])
        (Just
            [ (1, "3u"), (2, "3u"), (3, "3a"), (4, "3u")
            , (5, "3a"), (6, "3u"), (7, "3a"), (8, "3u")
            ], [])

test_positions = do
    -- Force all the positions because of partial functions in there.
    forM_ Reyong.reyong_positions $ \pos -> equal (extract pos) (extract pos)
    where
    ds = [Reyong.I .. Reyong.A]
    extract (Reyong.Position _ cek byong pickup cycle) =
        (cek, byong, map pickup ds, map cycle ds)
