-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Some standard call names.  These are used to construct calls symbolically,
-- or so instruments can override standard symbols.
--
-- 'Derive.Expr.ToExpr' and 'Derive.Expr.generator' are useful for constructing
-- symbolic expressions.
module Derive.Symbols where
import qualified Derive.Expr as Expr


-- | Call used by the infix @=@ syntax.
equal :: Expr.Symbol
equal = "="

mute :: Expr.Symbol
mute = "m"

-- | Cause notes to be stronger or weaker.
accent, weak :: Expr.Symbol
accent = "v"
weak = "^"

