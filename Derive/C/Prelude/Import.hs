-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The @import@ call, and support.
module Derive.C.Prelude.Import where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Sig as Sig


calls :: Derive.Callable d => Derive.CallMaps d
calls = Derive.transformer_call_map
    [ ("import", c_import)
    , ("imports", c_import_symbol)
    ]

c_import :: Derive.Callable d => Derive.Transformer d
c_import = Derive.transformer Module.prelude "import" mempty
    "Import the given modules into scope. Calls of all types (note, control,\
    \ pitch, val) are imported. If names clash, the ones from later modules\
    \ win."
    $ Sig.callt (Sig.many1 "module" "Import these modules.") $ \modules _ d ->
        foldr (Derive.with_imported False) d $
            map Module.Module (NonEmpty.toList modules)

c_import_symbol :: Derive.Callable d => Derive.Transformer d
c_import_symbol = Derive.transformer Module.prelude "import-symbol" mempty
    "Import a single symbol, or list of symbols."
    $ Sig.callt ((,)
    <$> Sig.required "module" "Import this module."
    <*> Sig.many1 "symbol" "Import these symbols."
    ) $ \(module_, syms) _args ->
        Derive.with_imported_symbols (Module.Module module_)
            (Set.fromList (NonEmpty.toList syms))
