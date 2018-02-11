-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'c_equal' call, which implements @=@.
--
-- The equal call is heavily overloaded because I want to reuse the nice infix
-- syntax.  Unfortunately it results in lots of cryptic prefixes.  Is it worth
-- it?
module Derive.C.Prelude.Equal (
    library, c_equal
    , transform_expr
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global


-- * note

library :: Library.Library
library = mconcat
    [ Library.generators [("=", c_equal_generator)]
    , Library.poly_transformers
        [ ("=", c_equal)
        , ("default-merge", c_default_merge)
        ]
    ]

-- * util

-- | Parse an expression containing only equal calls and turn it into a
-- transformer.  'Eval.eval_transform_expr' is more general, but only
-- transforms Streams, because most transforms work with a stream.  So by
-- having a more restrictive input, this can have a more general output:
-- contravariance I guess?
transform_expr :: Text -> Derive.Deriver a -> Derive.Deriver a
transform_expr expr deriver = do
    parsed <- Derive.require_right id $ Parse.parse_expr expr
    transforms <- if is_empty_expr parsed
        then return []
        else mapM apply $ NonEmpty.toList parsed
    foldr ($) deriver transforms
    where
    apply expr = do
        (lhs, rhs) <- Derive.require
            ("expected an assignment: " <> ShowVal.show_val expr)
            (equal_expr expr)
        Derive.require_right id $ parse_equal Set lhs rhs

is_empty_expr :: Expr.Expr a -> Bool
is_empty_expr (Expr.Call "" [] :| []) = True
is_empty_expr _ = False

equal_expr :: BaseTypes.Call -> Maybe (Text, BaseTypes.Val)
equal_expr (Expr.Call (Expr.Symbol "=")
        [Expr.Literal (BaseTypes.VStr (Expr.Str lhs)), Expr.Literal val]) =
    Just (lhs, val)
equal_expr _ = Nothing

-- * implementation

c_equal :: Derive.CallableExpr d => Derive.Transformer d
c_equal = Derive.transformer Module.prelude "equal" Tags.subs equal_doc $
    Sig.callt equal_args $ \(lhs, rhs, merge) _args deriver -> do
        transform <- Derive.require_right id $ parse_equal merge lhs rhs
        transform deriver

c_equal_generator :: Derive.Generator Derive.Note
c_equal_generator = Derive.generator Module.prelude "equal" Tags.subs
    "Similar to the transformer, this will evaluate the notes below in\
    \ a transformed environ." $
    Sig.call equal_args $ \(lhs, rhs, merge) args -> do
        transform <- Derive.require_right id $ parse_equal merge lhs rhs
        transform . Sub.derive . concat =<< Sub.sub_events args

equal_args :: Sig.Parser (Text, BaseTypes.Val, Merge)
equal_args = (,,)
    <$> Sig.required "lhs" "Assign to this. This looks like a Str, but\
        \ can actualy contain any characters except `=`, due to the special\
        \ infix parsing for `=`. Symbolic prefixes determine what is\
        \ assigned, and the valid types for the rhs."
    <*> Sig.required "rhs" "Source of the assignment."
    <*> (parse_merge <$> Sig.defaulted "merge" "set" merge_doc)

merge_doc :: Doc.Doc
merge_doc = "Merge operator. This can be `default` to use the default for the\
    \ control, `set` to replace the old signal, or one of the operators from\
    \ 'Derive.Deriver.Monad.mergers': "
    <> Doc.commas (map ShowVal.doc (Map.keys Derive.mergers)) <> "."
    <> " There are also symbolic aliases, to support `=+` syntax: "
    <> Doc.pretty symbol_to_merge

data Merge = Default | Set | Merge Expr.Symbol deriving (Show)

instance ShowVal.ShowVal Merge where
    show_val Default = "default"
    show_val Set = "set"
    show_val (Merge sym) = ShowVal.show_val sym

parse_merge :: Expr.Symbol -> Merge
parse_merge name
    | name == "set" = Set
    | name == "default" = Default
    | otherwise = Merge $ Map.findWithDefault name name symbol_to_merge

symbol_to_merge :: Map Expr.Symbol Expr.Symbol
symbol_to_merge = Map.fromList
    [ ("+", "add")
    , ("-", "sub")
    , ("*", "mul")
    , ("@", "scale")
    ]

equal_doc :: Doc.Doc
equal_doc =
    "Evaluate the deriver with a value set. Special parser support means this\
    \ can be called infix.  The arguments can take many forms to set different\
    \ kinds of values.\
    \\nSet an environ value by setting a plain symbol or unset it by assigning\
    \ to `_`: `x = 42` or `x = _`.\
    \\nAlias instrument names like: `>alias = >inst`.\
    \\nIf the lhs is prefixed with `>>`, `^`, `*`, `.`, or `-`, it will add\
    \ a new name for a ^note, *pitch, .control, or -val call, respectively.\
    \ It sets the generator by default, but will set the transformer if you\
    \ prefix another `-`. `>>` is special cased to only create a note\
    \ transformer, whose name will be prefixed with `>`. This is used to set\
    \ an instrument transformer, which can apply a transformer when an\
    \ instrument is set by the title of a note track, as implemented by\
    \ by `note-track`.\
    \\nE.g.: set note generator: `^phrase = some-block`,\
    \ note transformer: `^-mute = +mute+loose`,\
    \ control transfomrer: `.-i = t`, val call: `-4c = (5c)`.\
    \\nYou can bypass all this cryptic prefix garbage by using a `ky` file.\
    \ It has more space available so it can use a more readable syntax.\
    \\nIf you bind a call to a quoted expression, this creates a new\
    \ call: `^abc = \"(a b c)` will create a `abc` call, which is a macro for\
    \ `a b c`. The created call does not take arguments.\
    \\nSet constant signals by assigning to a signal literal: `%c = .5` or\
    \ pitch: `#p = (4c)`.  `# = (4c)` sets the default pitch signal.\
    \ You can rename a signal via `%a = %b` or `#x = #y`. Control signal\
    \ assignment also supports the same merge functions as the control track:\
    \ `%a = .5 add` or `%a = %b add`.  However, the second example throws an\
    \ error if `%b` is a ControlFunction. `%a = .5 default` will combine with\
    \ `a`'s default merge function. Assigning to `_` unsets the control, and\
    \ any ControlFunction.\n\
    \ The `=` operator can be suffixed with symbol, which will become the last\
    \ argument, so `%x=+1` becomes `%x = 1 '+'`.  Note that the order\
    \ is backwards from the usual `+=`, which is ultimately because the first\
    \ word can have almost any character except space and `=`. Also, `x=-1` is\
    \ ambiguous, and parsed as `x =- 1`."
    -- Previously > was for binding note calls, but that was taken by
    -- instrument aliasing.  ^ at least looks like a rotated >.

parse_equal :: Merge -> Text -> BaseTypes.Val
    -> Either Text (Derive.Deriver a -> Derive.Deriver a)
parse_equal Set lhs rhs
    -- Assign to call.
    | Just new <- Text.stripPrefix "^" lhs = Right $
        override_call new rhs
            (Derive.s_generator#Derive.s_note)
            (Derive.s_transformer#Derive.s_note)
    | Just new <- Text.stripPrefix ">>" lhs = Right $
        override_transformer (">" <> new) rhs
            (Derive.s_transformer#Derive.s_note)
    | Just new <- Text.stripPrefix "*" lhs = Right $
        override_call new rhs
            (Derive.s_generator#Derive.s_pitch)
            (Derive.s_transformer#Derive.s_pitch)
    | Just new <- Text.stripPrefix "." lhs = Right $
        override_call new rhs
            (Derive.s_generator#Derive.s_control)
            (Derive.s_transformer#Derive.s_control)
    | Just new <- Text.stripPrefix "-" lhs = Right $ override_val_call new rhs
parse_equal Set lhs rhs
    -- Create instrument alias.
    | Just new <- Text.stripPrefix ">" lhs = case rhs of
        BaseTypes.VStr (Expr.Str inst) -> Right $
            Derive.with_instrument_alias (Score.Instrument new)
                (Score.Instrument inst)
        _ -> Left $ "aliasing an instrument expected an instrument rhs, got "
            <> pretty (ValType.type_of rhs)
parse_equal merge lhs rhs
    -- Assign to control.
    | Just control <- is_control =<< parse_val lhs = case rhs of
        BaseTypes.VControlRef rhs -> Right $ \deriver ->
            Typecheck.to_signal_or_function rhs >>= \case
                Left sig -> do
                    merger <- get_merger control merge
                    Derive.with_merged_control merger control sig deriver
                Right f -> case merge of
                    Set -> Derive.with_control_function control f deriver
                    merge -> Derive.throw_arg_error $ merge_error merge
        BaseTypes.VNum rhs -> Right $ \deriver -> do
            merger <- get_merger control merge
            Derive.with_merged_control merger control (fmap Signal.constant rhs)
                deriver
        BaseTypes.VControlFunction f -> case merge of
            Set -> Right $ Derive.with_control_function control f
            merge -> Left $ merge_error merge
        BaseTypes.VNotGiven -> Right $ Derive.remove_controls [control]
        _ -> Left $ "binding a control expected a control, num, control\
            \ function, or _, but got " <> pretty (ValType.type_of rhs)
    where
    is_control (BaseTypes.VControlRef (BaseTypes.LiteralControl c)) = Just c
    is_control _ = Nothing
parse_equal _ lhs rhs
    -- Assign to pitch control.
    | Just control <- is_pitch =<< parse_val lhs = case rhs of
        BaseTypes.VPitch rhs -> Right $ \deriver ->
            Derive.with_named_pitch control (PSignal.constant rhs) deriver
        BaseTypes.VPControlRef rhs -> Right $ \deriver -> do
            sig <- Call.to_psignal rhs
            Derive.with_named_pitch control sig deriver
        BaseTypes.VNum (Score.Typed Score.Nn nn) -> Right $ \deriver ->
            Derive.with_named_pitch control
                (PSignal.constant (PSignal.nn_pitch (Pitch.nn nn))) deriver
        _ -> Left $ "binding a pitch signal expected a pitch, pitch"
            <> " control, or nn, but got " <> pretty (ValType.type_of rhs)
    where
    is_pitch (BaseTypes.VPControlRef (BaseTypes.LiteralControl c)) = Just c
    is_pitch _ = Nothing
parse_equal Set lhs val = Right $ Derive.with_val lhs val
parse_equal (Merge merge) lhs (BaseTypes.VNum (Score.Typed Score.Untyped val)) =
    Right $ \deriver -> do
        merger <- Derive.get_control_merge merge
        Derive.with_merged_numeric_val merger lhs val deriver
parse_equal merge _ _ = Left $ merge_error merge

merge_error :: Merge -> Text
merge_error merge = "merge is only supported when assigning to a control or\
    \ untyped numeric env: " <> ShowVal.show_val merge

-- | Unlike 'Derive.MergeDefault', the default is Derive.Set.
get_merger :: Score.Control -> Merge -> Derive.Deriver Derive.Merger
get_merger control merge = case merge of
    Set -> return Derive.Set
    Default -> Derive.get_default_merger control
    Merge merge -> Derive.get_control_merge merge

parse_val :: Text -> Maybe BaseTypes.Val
parse_val = either (const Nothing) Just . Parse.parse_val

-- | Look up a call with the given Symbol and add it as an override to the
-- scope given by the lenses.  I wanted to pass just one lens, but apparently
-- they're not sufficiently polymorphic.
override_call :: (Derive.CallableExpr d1, Derive.CallableExpr d2)
    => Text -> BaseTypes.Val
    -> Lens Derive.Scopes (Derive.ScopePriority (Derive.Generator d1))
    -> Lens Derive.Scopes (Derive.ScopePriority (Derive.Transformer d2))
    -> Derive.Deriver a -> Derive.Deriver a
override_call lhs rhs generator transformer deriver
    | Just stripped <- Text.stripPrefix "-" lhs =
        override_transformer stripped rhs transformer deriver
    | otherwise = override_generator_scope
        =<< resolve_source quoted_generator rhs
    where
    override_generator_scope call = Derive.with_scopes add deriver
        where
        add = generator %= Derive.add_priority Derive.PrioOverride
            (Derive.single_call (Expr.Symbol lhs) call)

-- | Make an expression into a transformer and stick it into the
-- 'Derive.PrioOverride' slot.
override_transformer :: Derive.CallableExpr d => Text -> BaseTypes.Val
    -> Lens Derive.Scopes (Derive.ScopePriority (Derive.Transformer d))
    -> Derive.Deriver a -> Derive.Deriver a
override_transformer lhs rhs transformer deriver =
    override_scope =<< resolve_source quoted_transformer rhs
    where
    override_scope call = Derive.with_scopes
        (transformer %= Derive.add_priority Derive.PrioOverride
            (Derive.single_call (Expr.Symbol lhs) call))
        deriver

override_val_call :: Text -> BaseTypes.Val -> Derive.Deriver a
    -> Derive.Deriver a
override_val_call lhs rhs deriver = do
    call <- resolve_source quoted_val_call rhs
    let add = Derive.s_val %= Derive.add_priority Derive.PrioOverride
            (Derive.single_call (Expr.Symbol lhs) call)
    Derive.with_scopes add deriver

-- | A VQuoted becomes a call, a Str is expected to name a call, and
-- everything else is turned into a Str via ShowVal.  This will cause
-- a parse error for un-showable Vals, but what else is new?
resolve_source :: Derive.Callable call => (BaseTypes.Quoted -> call)
    -> BaseTypes.Val -> Derive.Deriver call
resolve_source make_call rhs = case rhs of
    BaseTypes.VQuoted quoted -> return $ make_call quoted
    BaseTypes.VStr (Expr.Str sym) -> get_call (Expr.Symbol sym)
    _ -> get_call (Expr.Symbol (ShowVal.show_val rhs))

get_call :: forall call. Derive.Callable call => Expr.Symbol
    -> Derive.Deriver call
get_call sym = maybe (Derive.throw $ Eval.unknown_symbol name sym) return
    =<< Derive.lookup_call sym
    where name = Derive.callable_name (Proxy :: Proxy call)


-- * quoted

-- | Create a new call from a quoted expression.  This is flirting with
-- function definiion, but is really just macro expansion, with all the
-- variable capture problems implied.  But since the only variables I have are
-- calls maybe it's not so bad.
quoted_generator :: Derive.CallableExpr d => BaseTypes.Quoted
    -> Derive.Generator d
quoted_generator quoted@(BaseTypes.Quoted expr) =
    Derive.generator quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0 $ \args -> Eval.eval_expr True (Args.context args) expr

quoted_transformer :: Derive.CallableExpr d
    => BaseTypes.Quoted -> Derive.Transformer d
quoted_transformer quoted@(BaseTypes.Quoted expr) =
    Derive.transformer quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0t $ \args deriver ->
        Eval.eval_transformers (Args.context args) (NonEmpty.toList expr)
            deriver

quoted_val_call :: BaseTypes.Quoted -> Derive.ValCall
quoted_val_call quoted = Derive.val_call quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0 $ \args -> do
        call <- case quoted of
            BaseTypes.Quoted (call :| []) -> return $ Expr.ValCall call
            _ -> Derive.throw $
                "expected a val call, but got a full expression: "
                <> ShowVal.show_val quoted
        Eval.eval (Args.context args) call

-- | Pseudo-module for val calls generated from a quoted expression.
quoted_module :: Module.Module
quoted_module = "quoted"


-- * other

c_default_merge :: Derive.Taggable d => Derive.Transformer d
c_default_merge = Derive.transformer Module.prelude "default-merge" mempty
    "Set the default merge operators for controls. These apply when the\
    \ control track doesn't have an explicit operator."
    $ Sig.callt ((,)
    <$> (parse_merge <$> Sig.required "merge" merge_doc)
    <*> (NonEmpty.toList <$> Sig.many1 "control" "Control names.")
    ) $ \(merge, controls) _args deriver -> do
        mergers <- mapM (\c -> get_merger c merge) controls
        Internal.with_default_merge (Map.fromList (zip controls mergers))
            deriver
