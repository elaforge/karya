-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | The Str and CallId types, and ToExpr class, in a module with few
-- dependencies so modules can make exprs without incurring a dependency on
-- "Derive.BaseTypes", and more importantly, 'Derive.BaseTypes.Val', which
-- drags in tons of stuff.
module Derive.Expr where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import qualified Derive.ShowVal as ShowVal
import Global


-- | The only operator is @|@, so a list suffices for an AST.
--
-- This is parameterized by the val unparsed exprs are @Expr Text@ while parsed
type Expr val = NonEmpty (Call val)
data Call val = Call CallId [Term val] deriving (Show)
data Term val = ValCall (Call val) | Literal val deriving (Show)

instance ShowVal.ShowVal val => ShowVal.ShowVal (Expr val) where
    show_val = Text.intercalate " | " . map ShowVal.show_val . NonEmpty.toList

instance ShowVal.ShowVal val => ShowVal.ShowVal (Call val) where
    show_val (Call (CallId sym) terms) =
        sym <> if null terms then ""
            else " " <> Text.unwords (map ShowVal.show_val terms)

instance ShowVal.ShowVal val => ShowVal.ShowVal (Term val) where
    show_val (ValCall call) = "(" <> ShowVal.show_val call <> ")"
    show_val (Literal val) = ShowVal.show_val val

-- | Name of a call, used to look it up in the namespace.
--
-- This is parsed by Parse.p_call_symbol, so it can have any character except
-- space, =, or ) for val calls.  It's not enforced though, especially since
-- there's an IsString instance, but if you put in a space you'll get a messed
-- up expression.
newtype CallId = CallId Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, String.IsString,
        Pretty.Pretty, Serialize.Serialize)

uncall :: CallId -> Text
uncall (CallId sym) = sym

instance ShowVal.ShowVal CallId where
    show_val (CallId sym) = sym

-- | Make a Call with Literal args.
call :: CallId -> [val] -> Call val
call call_id args = Call call_id (map Literal args)

call0 :: CallId -> Call val
call0 call_id = Call call_id []

expr :: [Call val] -> Call val -> Expr val
expr trans gen = hd :| tl
    where (hd : tl) = trans ++ [gen]

expr1 :: CallId -> Expr val
expr1 = expr [] . call0

-- * ToExpr

-- | This is meant for types which can be turned into a tracklang expression.
-- For example, drum strokes might have a parsed form which can be turned into
-- calls.
class ToExpr a where
    to_expr :: a -> Expr Text

class ToCall a where
    to_call :: a -> CallId

-- * Str

newtype Str = Str Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, String.IsString,
        Serialize.Serialize, ShowVal.ShowVal)
instance Pretty.Pretty Str where pretty = ShowVal.show_val

unstr :: Str -> Text
unstr (Str str) = str
