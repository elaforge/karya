-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Exports from "Util.Test.Testing".  This is meant to be imported
-- unqualified.
module Util.Test.Global (module Util.Test.Testing) where
import Util.Test.Testing
       (check, equal, equal_fmt, right_equal, not_equal, equalf, strings_like,
        left_like, match, throws, io_equal, io_human, pause, success, failure,
        expect_right, quickcheck, q_equal, prettyp, pprint)
