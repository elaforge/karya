-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Exports from "Util.Test.Testing".  This is meant to be imported
-- unqualified.
module Util.Test.Global (module Util.Test.Testing) where
import           Util.Test.Testing
       (check, check_val, equal, equal_fmt, equal_on, equalf, expect_right,
        failure, hedgehog, io_equal, io_human, left_like, match, not_equal,
        pause, pprint, prettyp, property, q_equal, quickcheck, right_equal,
        strings_like, success, throws, (/==), (===), Test, Profile,
        ModuleMeta(ModuleMeta))
