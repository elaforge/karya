-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | The 'Str' and 'Symbol' types, and 'ToExpr' class.
--
-- They are split into a module with few dependencies so modules can make exprs
-- without incurring a dependency on "Derive.DeriveT", and specifically
-- 'Derive.DeriveT.Val', which drags in tons of stuff.
module Derive.Expr where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.NEs as NEs
import qualified Util.Lists as Lists
import qualified Util.Serialize as Serialize

import qualified Derive.ScoreT as ScoreT
import           Derive.ShowVal (ShowVal(show_val))
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global


-- | A full toplevel expression, sometimes called a "pipeline", because it looks
-- like "transform | transform | generator arg arg".  Since the only operator
-- is @|@, which is basically just application, a list suffices for an AST.
--
-- This is parameterized by the literal value, so a tokenized expr is
-- @Expr Text@ while fully parsed one would be @Expr Val@.
type Expr val = NonEmpty (Call val)
data Call val = Call Symbol [Term val]
    deriving (Show, Read, Eq, Functor)
data Term val = ValCall (Call val) | Literal val
    deriving (Show, Read, Eq, Functor)

instance String.IsString (Call val) where
    fromString = call0 . String.fromString
instance String.IsString (Expr val) where
    fromString = generator0 . String.fromString

-- These ShowVal instances are tested in Derive.Parse_test:

instance ShowVal (Expr Text) where show_val = show_val_expr
instance ShowVal (Call Text) where show_val = show_val_call (Just . id)
instance ShowVal (Term Text) where show_val = show_val_term

instance ShowVal (Expr MiniVal) where show_val = show_val_expr
instance ShowVal (Call MiniVal) where
    show_val = show_val_call $ \case
        VStr (Str op) -> Just op
        _ -> Nothing
instance ShowVal (Term MiniVal) where show_val = show_val_term

instance Pretty (Call Text) where pretty = show_val
instance Pretty (Term Text) where pretty = show_val
instance Pretty (Call MiniVal) where pretty = show_val
instance Pretty (Term MiniVal) where pretty = show_val

-- Previously I used 'instance ShowVal val => ShowVal (Expr val)', but that
-- doesn't let me have a specialized version for Call Val, unless I want to do
-- overlapping instances, which I don't.

show_val_expr :: ShowVal (Call val) => Expr val -> Text
show_val_expr = Text.strip . Text.intercalate " | " . map show_val
    . NonEmpty.toList

show_val_call :: ShowVal (Term val) => (val -> Maybe Text) -> Call val -> Text
show_val_call literal_str_of = \case
    -- This inverts 'Derive.Parse.p_equal'.
    Call (Symbol "=") [lhs, rhs] ->
        Text.unwords [show_val lhs, "=", show_val rhs]
    Call (Symbol "=") [lhs, rhs, Literal op]
        | Just op <- literal_str_of op -> Text.unwords
            [ show_val lhs
            , "=" <> op
            , show_val rhs
            ]
    Call (Symbol sym) terms -> Text.unwords $ sym : map show_val terms

show_val_term :: (ShowVal val, ShowVal (Call val)) => Term val -> Text
show_val_term = \case
    ValCall call -> "(" <> show_val call <> ")"
    Literal val -> show_val val

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

instance ShowVal Symbol where
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
split = NEs.unsnoc

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
    _ : _ -> call1 :| Lists.mapLast (map_symbol f) calls

-- * ToExpr

-- | This is meant for types which can be turned into a tracklang expression.
-- For example, drum strokes might have a parsed form which can be turned into
-- calls.
class ToExpr a where
    to_expr :: a -> Expr MiniVal

-- * Str

newtype Str = Str Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, String.IsString,
        Serialize.Serialize, ShowVal)
instance Pretty Str where pretty = show_val

unstr :: Str -> Text
unstr (Str str) = str

-- * MiniVal

-- | Yes, it's yet another Val variant.  This one is even more mini than
-- REnv.Val.
-- TODO NOTE [val-and-minival]
data MiniVal = VNum !(ScoreT.Typed Signal.Y) | VStr !Str
    deriving (Eq, Ord, Show)

instance String.IsString MiniVal where
    fromString = VStr. String.fromString

instance ShowVal MiniVal where
    show_val (VNum v) = show_val v
    show_val (VStr v) = show_val v

instance Pretty MiniVal where pretty = show_val

instance Serialize.Serialize MiniVal where
    put (VNum a) = Serialize.put_tag 0 >> Serialize.put a
    put (VStr a) = Serialize.put_tag 1 >> Serialize.put a
    get = Serialize.get_tag >>= \case
        0 -> VNum <$> Serialize.get
        1 -> VStr <$> Serialize.get
        tag -> Serialize.bad_tag "MiniVal" tag

class ToVal a where to_val :: a -> MiniVal

instance ToVal Int where to_val a = to_val (fromIntegral a :: Double)
instance ToVal Double where to_val = VNum . ScoreT.untyped
instance ToVal Text where to_val = VStr . Str
