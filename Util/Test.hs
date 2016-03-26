-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Exports from "Util.Testing".  This is meant to be imported unqualified.
module Util.Test (module Util.Testing) where
import Util.Testing (
    -- * pure assertions
    check, equal, right_equal, not_equal, equalf, strings_like
    , left_like , match
    -- * exception assertions
    , throws
    -- * io assertions
    , io_equal, io_human, pause
    -- * low level
    , success, failure
    -- * extracting
    , expect_right
    -- * pretty printing
    , prettyp, pprint
    )
