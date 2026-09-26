-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The @import@ call, and support.
module Derive.C.Prelude.Import (library) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Library as Library
import qualified Derive.Scale as Scale
import qualified Derive.Sig as Sig


library :: Library.Library
library = Library.polyTransformers
    [ ("import", cImport)
    , ("imports", cImportSymbol)
    , ("scale", cScale)
    ]

cImport :: Derive.CallableExpr d => Derive.Transformer d
cImport = Derive.transformer Module.prelude "import" mempty
    "Import the given modules into scope. Calls of all types (note, control,\
    \ pitch, val) are imported. If names clash, the ones from later modules\
    \ win."
    $ Sig.callt (Sig.many1 "module" "Import these modules.") $ \modules _ d ->
        foldr (Derive.withImported False) d $
            map Module.Module (NonEmpty.toList modules)

cImportSymbol :: Derive.CallableExpr d => Derive.Transformer d
cImportSymbol = Derive.transformer Module.prelude "import-symbol" mempty
    "Import a single symbol, or list of symbols. This imports from all\
    \ namespaces simultaneously: note, control, pitch, and val.\
    \ TODO fix it if it's a problem."
    $ Sig.callt ((,)
    <$> Sig.required "module" "Import this module."
    <*> Sig.many1 "symbol" "Import these symbols."
    ) $ \(module_, syms) _args ->
        Derive.withImportedSymbols (Module.Module module_)
            (Set.fromList (NonEmpty.toList syms))

cScale :: Derive.CallableExpr d => Derive.Transformer d
cScale = Derive.transformer Module.prelude "scale" mempty
    "Bring a scale into scope."
    $ Sig.callt ((,)
    <$> Sig.requiredEnv "scale" Sig.Unprefixed "Look up scale by name."
    <*> Sig.many "args" "Scale arguments."
    ) $ \(name, scaleArgs) _args deriver -> do
        scale <- Scale.get (Derive.CallName name) scaleArgs
        -- Set env var so subsequent calls default it.
        Derive.withValRaw EnvKey.scale name $
            Derive.withScale scale deriver
