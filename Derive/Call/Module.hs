-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Module where
import qualified Data.String as String
import qualified Data.Text as Text

import Global


-- | The module name is dot-separated by convention, and should consist of
-- lowercase letters, digits, and dots.
newtype Module = Module Text.Text
    deriving (Eq, Ord, Show, String.IsString)

instance Pretty Module where pretty (Module m) = m

instance Monoid Module where
    mempty = ""
    mappend (Module m1) (Module m2)
        | Text.null m1 = Module m2
        | Text.null m2 = Module m1
        | otherwise = Module $ m1 <> "." <> m2

-- | This marks a standard library of \"fundamental\" calls.  They may also
-- interact more intimately with the builtin derivation machinery.  Imported
-- implicitly.
prelude :: Module
prelude = "prelude"

-- | Internal calls are used to implement the basic track calls.  You should
-- never need to call them directly, and they can probably be omitted from the
-- documentation.  Imported implicitly.
internal :: Module
internal = "internal"

-- | Per-score calls, loaded from a definitions file.  Imported implicitly.
local :: Module
local = "local"

-- | A pseudo-module that scale degree calls live in.
scale :: Module
scale = "scale"

-- | Parent module for instrument-specific calls.  This is also used for
-- instrument calls, though it doesn't really matter since the instrument
-- acts like an implicit import.
instrument :: Module
instrument = "inst"

-- | Only emits lilypond, emits no \"normal\" events.  You never need to use
-- these if you aren't generating lilypond.  This is different from the @ly@
-- tag, which simply marks that the call can emit lilypond.
ly :: Module
ly = "ly"

-- | Calls for ornaments that occur in European music.  They generally
-- correspond to things you might see in staff notation, and many of them can
-- emit lilypond as well.
europe :: Module
europe = "europe"

bali :: Module
bali = "bali"

india :: Module
india = "india"
