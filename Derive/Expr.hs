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
import           Derive.ShowVal (ShowVal(showVal))
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
    deriving (Show, Read, Eq, Functor, Foldable, Traversable)
data Term val = ValCall (Call val) | Literal val
    deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance String.IsString (Call val) where
    fromString = call0 . String.fromString
instance String.IsString (Expr val) where
    fromString = generator0 . String.fromString

-- These ShowVal instances are tested in Derive.Parse_test:

instance ShowVal (Expr Text) where showVal = showValExpr
instance ShowVal (Call Text) where showVal = showValCall (Just . id)
instance ShowVal (Term Text) where showVal = showValTerm

instance ShowVal (Expr MiniVal) where showVal = showValExpr
instance ShowVal (Call MiniVal) where
    showVal = showValCall $ \case
        VStr (Str op) -> Just op
        _ -> Nothing
instance ShowVal (Term MiniVal) where showVal = showValTerm

instance Pretty (Call Text) where pretty = showVal
instance Pretty (Term Text) where pretty = showVal
instance Pretty (Call MiniVal) where pretty = showVal
instance Pretty (Term MiniVal) where pretty = showVal

-- Previously I used 'instance ShowVal val => ShowVal (Expr val)', but that
-- doesn't let me have a specialized version for Call Val, unless I want to do
-- overlapping instances, which I don't.

showValExpr :: ShowVal (Call val) => Expr val -> Text
showValExpr = Text.strip . Text.intercalate " | " . map showVal
    . NonEmpty.toList

showValCall :: ShowVal (Term val) => (val -> Maybe Text) -> Call val -> Text
showValCall literalStrOf = \case
    -- This inverts 'Derive.Parse.p_equal'.
    Call (Symbol "=") [lhs, rhs] ->
        Text.unwords [showVal lhs, "=", showVal rhs]
    Call (Symbol "=") [lhs, rhs, Literal op]
        | Just op <- literalStrOf op -> Text.unwords
            [ showVal lhs
            , "=" <> op
            , showVal rhs
            ]
    Call (Symbol sym) terms -> Text.unwords $ sym : map showVal terms

showValTerm :: (ShowVal val, ShowVal (Call val)) => Term val -> Text
showValTerm = \case
    ValCall call -> "(" <> showVal call <> ")"
    Literal val -> showVal val

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
    showVal (Symbol sym) = sym

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

valCall :: Symbol -> [a] -> Term a
valCall sym args = ValCall (call sym args)

transform :: Call a -> Expr a -> Expr a
transform call (hd :| tl) = call :| (hd : tl)

transform0 :: Symbol -> Expr a -> Expr a
transform0 = transform . call0

-- | Shortcut to transform an Expr.
with :: ToExpr a => Symbol -> a -> Expr MiniVal
with sym = transform0 sym . toExpr

-- ** transform

strToScaleId :: Str -> Pitch.ScaleId
strToScaleId = Pitch.ScaleId . unstr

scaleIdToStr :: Pitch.ScaleId -> Str
scaleIdToStr (Pitch.ScaleId s) = Str s

mapSymbol :: (Symbol -> Symbol) -> Call a -> Call a
mapSymbol f (Call call args) = Call (f call) args

-- | Transform the 'Literal's in an expression.
mapLiterals :: (a -> b) -> Expr a -> Expr b
mapLiterals = fmap . fmap

-- | Transform only the Symbol in the generator position.
mapGenerator :: (Symbol -> Symbol) -> Expr a -> Expr a
mapGenerator f (call1 :| calls) = case calls of
    [] -> mapSymbol f call1 :| []
    _ : _ -> call1 :| Lists.mapLast (mapSymbol f) calls

-- * ToExpr

-- | This is meant for types which can be turned into a tracklang expression.
-- For example, drum strokes might have a parsed form which can be turned into
-- calls.
class ToExpr a where
    toExpr :: a -> Expr MiniVal

-- * Str

newtype Str = Str Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, String.IsString,
        Serialize.Serialize, ShowVal)
instance Pretty Str where pretty = showVal

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
    showVal (VNum v) = showVal v
    showVal (VStr v) = showVal v

instance Pretty MiniVal where pretty = showVal

instance Serialize.Serialize MiniVal where
    put (VNum a) = Serialize.putTag 0 >> Serialize.put a
    put (VStr a) = Serialize.putTag 1 >> Serialize.put a
    get = Serialize.getTag >>= \case
        0 -> VNum <$> Serialize.get
        1 -> VStr <$> Serialize.get
        tag -> Serialize.badTag "MiniVal" tag

class ToVal a where toVal :: a -> MiniVal

instance ToVal Int where toVal a = toVal (fromIntegral a :: Double)
instance ToVal Double where toVal = VNum . ScoreT.untyped
instance ToVal Text where toVal = VStr . Str
