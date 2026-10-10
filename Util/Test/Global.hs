-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Exports from "Util.Test.Testing".  This is meant to be imported
-- unqualified.
module Util.Test.Global (module Util.Test.Testing) where
import           Util.Test.Testing
       (check, checkVal, equal, equalFmt, equalOn, equalf, expectRight,
        failure, hedgehog, io, ioEqual, ioHuman, leftLike, match, notEqual,
        pause, pprint, prettyp, property, qEqual, quickcheck, rightEqual,
        stringsLike, success, throws, (/==), (===), Test, Profile,
        ModuleMeta(ModuleMeta))
