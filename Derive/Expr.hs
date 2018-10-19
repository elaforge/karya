-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | The Str and Symbol types, and ToExpr class, in a module with few
-- dependencies so modules can make exprs without incurring a dependency on
-- "Derive.BaseTypes", and more importantly, 'Derive.BaseTypes.Val', which
-- drags in tons of stuff.
module Derive.Expr where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.ShowVal as ShowVal
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Global


-- | The only operator is @|@, so a list suffices for an AST.
--
-- This is parameterized by the val unparsed exprs are @Expr Text@ while parsed
type Expr val = NonEmpty (Call val)
data Call val = Call Symbol [Term val]
    deriving (Show, Read, Eq, Functor)
data Term val = ValCall (Call val) | Literal val
    deriving (Show, Read, Eq, Functor)

instance String.IsString (Call val) where
    fromString = call0 . String.fromString
instance String.IsString (Expr val) where
    fromString = generator0 . String.fromString

instance ShowVal.ShowVal val => ShowVal.ShowVal (Expr val) where
    show_val = Text.strip . Text.intercalate " | " . map ShowVal.show_val
        . NonEmpty.toList

instance ShowVal.ShowVal val => ShowVal.ShowVal (Call val) where
    show_val (Call (Symbol sym) terms) =
        sym <> if null terms then ""
            else " " <> Text.unwords (map ShowVal.show_val terms)
instance ShowVal.ShowVal val => Pretty (Call val) where
    pretty = ShowVal.show_val

instance ShowVal.ShowVal val => ShowVal.ShowVal (Term val) where
    show_val (ValCall call) = "(" <> ShowVal.show_val call <> ")"
    show_val (Literal val) = ShowVal.show_val val
instance ShowVal.ShowVal val => Pretty (Term val) where
    pretty = ShowVal.show_val

-- | Name of a call, used to look it up in the namespace.
--
-- This is parsed by Parse.p_call_symbol, so it can have any character except
-- space, =, or ) for val calls.  It's not enforced though, especially since
-- there's an IsString instance, but if you put in a space you'll get a messed
-- up expression.
newtype Symbol = Symbol Text
    deriving (Eq, Ord, Read, Show, Semigroup, Monoid, DeepSeq.NFData,
        String.IsString, Pretty, Serialize.Serialize)

unsym :: Symbol -> Text
unsym (Symbol sym) = sym

instance ShowVal.ShowVal Symbol where
    show_val (Symbol sym) = sym

expr :: [Call val] -> Call val -> Expr val
expr trans gen = hd :| tl
    where hd : tl = trans ++ [gen]

generator :: Call val -> Expr val
generator = expr []

-- | Generator with no arguments.
generator0 :: Symbol -> Expr val
generator0 = generator . call0

-- | Split into (transformers, generator).  Inverse of 'expr'.
split :: Expr val -> ([Call val], Call val)
split = Seq.ne_viewr

-- | Make a Call with Literal args.
call :: Symbol -> [val] -> Call val
call sym args = Call sym (map Literal args)

call0 :: Symbol -> Call val
call0 sym = Call sym []

val_call :: Symbol -> [a] -> Term a
val_call sym args = ValCall (call sym args)

transform :: Call a -> Expr a -> Expr a
transform call (hd :| tl) = call :| (hd : tl)

transform0 :: Symbol -> Expr a -> Expr a
transform0 = transform . call0

-- | Shortcut to transform an Expr.
with :: ToExpr a => Symbol -> a -> Expr MiniVal
with sym = transform0 sym . to_expr

-- ** transform

str_to_scale_id :: Str -> Pitch.ScaleId
str_to_scale_id = Pitch.ScaleId . unstr

scale_id_to_str :: Pitch.ScaleId -> Str
scale_id_to_str (Pitch.ScaleId s) = Str s

map_symbol :: (Symbol -> Symbol) -> Call a -> Call a
map_symbol f (Call call args) = Call (f call) args

-- | Transform the 'Literal's in an expression.
map_literals :: (a -> b) -> Expr a -> Expr b
map_literals = fmap . fmap

-- | Transform only the Symbol in the generator position.
map_generator :: (Symbol -> Symbol) -> Expr a -> Expr a
map_generator f (call1 :| calls) = case calls of
    [] -> map_symbol f call1 :| []
    _ : _ -> call1 :| Seq.map_last (map_symbol f) calls

-- * ToExpr

-- | This is meant for types which can be turned into a tracklang expression.
-- For example, drum strokes might have a parsed form which can be turned into
-- calls.
class ToExpr a where
    to_expr :: a -> Expr MiniVal

-- * Str

newtype Str = Str Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, String.IsString,
        Serialize.Serialize, ShowVal.ShowVal)
instance Pretty Str where pretty = ShowVal.show_val

unstr :: Str -> Text
unstr (Str str) = str

-- | Yes, it's yet another Val variant.  This one is even more mini than
-- RestrictedEnviron.Val.
-- TODO NOTE [val-and-minival]
data MiniVal = VNum !(ScoreTypes.Typed Signal.Y) | VStr !Str
    deriving (Eq, Show)

instance String.IsString MiniVal where
    fromString = VStr. String.fromString

instance ShowVal.ShowVal MiniVal where
    show_val (VNum v) = ShowVal.show_val v
    show_val (VStr v) = ShowVal.show_val v

instance Pretty MiniVal where pretty = ShowVal.show_val

num :: Double -> MiniVal
num = VNum . ScoreTypes.untyped
