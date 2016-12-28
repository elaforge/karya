-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Id_test where
import Util.Test
import qualified Ui.Id as Id


test_read_id = do
    let parse mkid a = [(mkid (Id.read_id a), "")]
    equal (reads "(bid \"a/b\")") (parse Id.BlockId "a/b")
    -- Funny ok for BlockId.
    equal (reads "(bid \"a/&b\")") (parse Id.BlockId "a/&b")
    -- Except a space.
    equal (reads "(bid \"a/ b\")") ([] :: [(Id.BlockId, String)])

    -- Other Ids.
    equal (reads "(vid \"a/b\")") (parse Id.ViewId "a/b")
    -- Funny characters not ok for ViewId.
    equal (reads "(vid \"a/&b\")") ([] :: [(Id.ViewId, String)])
    equal (reads "(vid \"a/b\")") ([] :: [(Id.RulerId, String)])
    equal (reads "(rid \"a/b\")") (parse Id.RulerId "a/b")
    equal (reads "(tid \"a/b\")") (parse Id.TrackId "a/b")
