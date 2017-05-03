-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The Symbol and CallId types, and ToCall class, in a module with few
-- dependencies so modules that otherwise have no dependency on
-- Derive.BaseTypes can use it.
module Derive.Symbol where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.String as String

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import qualified Derive.ShowVal as ShowVal
import Global


newtype Symbol = Symbol Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, String.IsString,
        Serialize.Serialize, ShowVal.ShowVal)
instance Pretty.Pretty Symbol where pretty = ShowVal.show_val

unsym :: Symbol -> Text
unsym (Symbol sym) = sym

-- | Symbols used in function call position.  This is just to document that
-- a symbol is expected to be looked up in the scope.
type CallId = Symbol

-- | This is meant for types which can be turned into call names.  For
-- example, drum strokes might have a parsed form which can be turned into
-- calls.
class ToCall a where
    to_call :: a -> CallId
