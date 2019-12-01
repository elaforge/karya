-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Some standard call names.  These are used to construct calls symbolically,
-- or so instruments can override standard symbols.  These form a kind of API,
-- so they should be coordinated between the different places that use them.
--
-- 'Derive.Expr.ToExpr' and 'Derive.Expr.generator' are useful for constructing
-- symbolic expressions.
module Derive.Symbols where
import qualified Derive.Expr as Expr


-- | This is called implicitly for note track titles, e.g. '>foo +bar' becomes
-- 'note-track foo +bar'.  It's mostly to reuse the call machinery for note
-- track titles, but as a bonus you can override it to customize note track
-- titles.
note_track :: Expr.Symbol
note_track = "note-track"

-- | The standard note call is bound to this, in addition to "".  Internally
-- calls wanting a standard note call this though, so you can rebind "" locally
-- without getting recursion.
default_note :: Expr.Symbol
default_note = "NOTE"

-- | If this is bound, scale degree pitch calls will be passed to this as a
-- single pitch argument.  Otherwise, they set the pitch, like the @set@ call.
-- So unlike 'default_note', where you rebind the "" call and 'default_note' is
-- the escape hatch, you rebind 'default_pitch' itself to hook the pitch
-- evaluation.
default_pitch :: Expr.Symbol
default_pitch = "PITCH"

null_note :: Expr.Symbol
null_note = ""

-- | Call used by the infix @=@ syntax.
equal :: Expr.Symbol
equal = "="

mute :: Expr.Symbol
mute = "m"

-- | Cause notes to be stronger or weaker.
accent, weak :: Expr.Symbol
accent = "v"
weak = "^"
