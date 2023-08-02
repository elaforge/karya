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
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Id as Id

import           Global


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

equal_expr :: DeriveT.Call -> Maybe (Text, DeriveT.Val)
equal_expr (Expr.Call (Expr.Symbol "=")
        [Expr.Literal (DeriveT.VStr (Expr.Str lhs)), Expr.Literal val]) =
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

equal_args :: Sig.Parser (Text, DeriveT.Val, Merge)
equal_args = (,,)
    <$> Sig.required_env "lhs" Sig.None
        "Assign to this. This looks like a Str, but\
        \ can actualy contain any characters except `=`, due to the special\
        \ infix parsing for `=`. Symbolic prefixes determine what is\
        \ assigned, and the valid types for the rhs."
    <*> Sig.required_env "rhs" Sig.None "Source of the assignment."
    <*> (parse_merge <$> Sig.defaulted "merge" ("set" :: Text) merge_doc)

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
    \\nPrefixes:\
    \\n- `>>` - Create a note transformer, whose name will be prefixed with\
    \ `>`.  This is used to set an instrument transformer, which can apply\
    \ a transformer when an instrument is set by the title of a note track, as\
    \ implemented by by `note-track`.\
    \\n- `-val` - Bind a val call.\
    \\n- `^note`, `*pitch`, `.control` - Bind the respective call type\
    \ generator.\
    \\n- `^-note`, `*-pitch`, `.-control` - As above, but bind transformer.\
    \\n- `>alias = >inst` - alias instrument name\
    \\nE.g.: set note generator: `^phrase = some-block`,\
    \ note transformer: `^-mute = +mute+loose`,\
    \ control transfomrer: `.-i = t`, val call: `-4c = (5c)`.\
    \\nYou can bypass all this cryptic prefix garbage by using a `ky` file.\
    \ It has more space available so it can use a more readable syntax.\
    \\nIf you bind a call to a quoted expression, this creates a new\
    \ call: `^abc = \"(a b c)` will create a `abc` call, which is a macro for\
    \ `a b c`. The created call does not take arguments.\
    \\nSet the default pitch signal with `#`, e.g. `# = (4c)` or `# = 45nn`.\
    \ Control signal assignment also supports the same merge functions as the\
    \ control track: `a = .5 add` or `a = %b add`.  However, the second example\
    \ throws an error if `%b` is a ControlFunction. `a = .5 default` will\
    \ combine with `a`'s default merge function. Assigning to `_` unsets the\
    \ control, and any ControlFunction.\
    \\nThe `=` operator can be suffixed with symbol, which will become the last\
    \ argument, so `%x=+1` becomes `%x = 1 '+'`.  Note that the order\
    \ is backwards from the usual `+=`, which is ultimately because the first\
    \ word can have almost any character except space and `=`. Also, `x=-1` is\
    \ ambiguous, and parsed as `x =- 1`."
    -- Previously > was for binding note calls, but that was taken by
    -- instrument aliasing.  ^ at least looks like a rotated >.

parse_equal :: Merge -> Text -> DeriveT.Val
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
        DeriveT.VStr (Expr.Str inst) -> Right $
            Derive.with_instrument_alias (ScoreT.Instrument new)
                (ScoreT.Instrument inst)
        _ -> Left $ "aliasing an instrument expected an instrument rhs, got "
            <> pretty (ValType.specific_type_of rhs)
-- TODO should I centralize the parsing of #?  Or is equal the only place that
-- needs this notation where # is state_pitch?  I used to parse a VPControlRef
parse_equal Set "#" rhs = case rhs of
    DeriveT.VPitch p -> Right $ Derive.with_pitch (PSignal.constant p)
    DeriveT.VPSignal sig -> Right $ Derive.with_pitch sig
    -- I could also convert a signal, but not sure it's useful.
    DeriveT.VSignal (ScoreT.Typed ScoreT.Nn sig)
        | Just nn <- Signal.constant_val sig ->
            Right $ Derive.with_pitch
                (PSignal.constant (PSignal.nn_pitch (Pitch.nn nn)))
    DeriveT.VNotGiven -> Right $ Derive.with_pitch mempty
    DeriveT.VPControlRef ref -> Right $ \deriver -> do
        sig <- Typecheck.resolve_pitch_ref ref
        Derive.with_pitch sig deriver
    _ -> Left $ "binding a pitch signal expected a pitch, pitch"
        <> " signal, or nn, but got " <> pretty (ValType.specific_type_of rhs)
parse_equal Set lhs rhs
    | not (Id.valid_symbol lhs) = Left $
        "tried to assign to invalid symbol name: " <> ShowVal.show_val lhs
    | otherwise = Right $ Derive.with_val lhs rhs
-- if rhs is a signal or number, then merge is ok
parse_equal merge lhs rhs = case rhs of
    DeriveT.VSignal sig -> Right $ merge_signal sig
    _ -> Left $ "merge is only supported for numeric types, tried to merge "
        <> pretty (ValType.specific_type_of rhs) <> " with "
        <> ShowVal.show_val merge
    where
    merge_signal sig deriver = do
        merger <- get_merger control merge
        Derive.with_merged_control merger control sig deriver
    control = ScoreT.Control lhs

-- | Unlike 'Derive.MergeDefault', the default is Derive.Set.
get_merger :: ScoreT.Control -> Merge -> Derive.Deriver Derive.Merger
get_merger control = \case
    Set -> return Derive.Set
    Default -> Derive.get_default_merger control
    Merge merge -> Derive.get_control_merge merge

-- | Look up a call with the given Symbol and add it as an override to the
-- scope given by the lenses.  I wanted to pass just one lens, but apparently
-- they're not sufficiently polymorphic.
override_call :: (Derive.CallableExpr d1, Derive.CallableExpr d2)
    => Text -> DeriveT.Val
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
override_transformer :: Derive.CallableExpr d => Text -> DeriveT.Val
    -> Lens Derive.Scopes (Derive.ScopePriority (Derive.Transformer d))
    -> Derive.Deriver a -> Derive.Deriver a
override_transformer lhs rhs transformer deriver =
    override_scope =<< resolve_source quoted_transformer rhs
    where
    override_scope call = Derive.with_scopes
        (transformer %= Derive.add_priority Derive.PrioOverride
            (Derive.single_call (Expr.Symbol lhs) call))
        deriver

override_val_call :: Text -> DeriveT.Val -> Derive.Deriver a
    -> Derive.Deriver a
override_val_call lhs rhs deriver = do
    call <- resolve_source quoted_val_call rhs
    let add = Derive.s_val %= Derive.add_priority Derive.PrioOverride
            (Derive.single_call (Expr.Symbol lhs) call)
    Derive.with_scopes add deriver

-- | A VQuoted becomes a call, a Str is expected to name a call, and
-- everything else is turned into a Str via ShowVal.  This will cause
-- a parse error for un-showable Vals, but what else is new?
resolve_source :: Derive.Callable call => (DeriveT.Quoted -> call)
    -> DeriveT.Val -> Derive.Deriver call
resolve_source make_call rhs = case rhs of
    DeriveT.VQuoted quoted -> return $ make_call quoted
    DeriveT.VStr (Expr.Str sym) -> get_call (Expr.Symbol sym)
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
quoted_generator :: Derive.CallableExpr d => DeriveT.Quoted
    -> Derive.Generator d
quoted_generator quoted@(DeriveT.Quoted expr) =
    Derive.generator quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0 $ \args -> Eval.eval_expr True (Args.context args) expr

quoted_transformer :: Derive.CallableExpr d
    => DeriveT.Quoted -> Derive.Transformer d
quoted_transformer quoted@(DeriveT.Quoted expr) =
    Derive.transformer quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0t $ \args deriver ->
        Eval.eval_transformers (Args.context args) (NonEmpty.toList expr)
            deriver

quoted_val_call :: DeriveT.Quoted -> Derive.ValCall
quoted_val_call quoted = Derive.val_call quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0 $ \args -> do
        call <- case quoted of
            DeriveT.Quoted (call :| []) -> return $ Expr.ValCall call
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
