-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The @import@ call, and support.
module Derive.Call.Import where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import Util.Control
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Sig as Sig


calls :: Derive.Callable d => Derive.CallMaps d
calls = Derive.transformer_call_map [("import", c_import)]

c_import :: Derive.Callable d => Derive.Transformer d
c_import = Derive.transformer Module.prelude "import" mempty
    "Import the given modules into scope. Calls of all types (note, control,\
    \ pitch, val) are imported."
    $ Sig.callt (Sig.many1 "module" "Import these modules.") $ \modules _ ->
        Derive.with_imported $ Set.fromList $
            map Module.Module (NonEmpty.toList modules)
