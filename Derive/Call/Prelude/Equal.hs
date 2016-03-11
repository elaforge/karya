-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'c_equal' call, which implements @=@.
--
-- The equal call is heavily overloaded because I want to reuse the nice infix
-- syntax.  Unfortunately it results in lots of cryptic prefixes.  Is it worth
-- it?
module Derive.Call.Prelude.Equal (note_calls, control_calls, pitch_calls) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global


-- * note

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [("=", c_equal_generator)]
    [("=", c_equal), (default_merge, c_default_merge)]

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.transformer_call_map
    [("=", c_equal), (default_merge, c_default_merge)]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.transformer_call_map
    [("=", c_equal), (default_merge, c_default_merge)]

default_merge :: BaseTypes.CallId
default_merge = "default-merge"

-- * implementation

c_equal :: Derive.Callable d => Derive.Transformer d
c_equal = Derive.transformer Module.prelude "equal" Tags.subs
    equal_doc (Sig.parsed_manually equal_arg_doc equal_transformer)

c_equal_generator :: Derive.Generator Derive.Note
c_equal_generator = Derive.generator Module.prelude "equal" Tags.subs
    "Similar to the transformer, this will evaluate the notes below in\
    \ a transformed environ."
    (Sig.parsed_manually equal_arg_doc generate)
    where
    generate args = Sub.derive . map (fmap (equal_transformer args))
        . concat =<< Sub.sub_events args

equal_arg_doc :: Text
equal_arg_doc = "Many types."

equal_doc :: Text
equal_doc =
    "Evaluate the deriver with a value set. Special parser support means this\
    \ can be called infix.  The arguments can take many forms to set different\
    \ kinds of values.\
    \\nSet an environ value by setting a plain symbol or unset it by assigning\
    \ to `_`: `x = 42` or `x = _`.\
    \\nAlias instrument names like: `>alias = >inst`.\
    \\nIf the symbol is prefixed with `>>`, `^`, `*`, `.`, or `-`, it will add\
    \ a new name for a ^note, *pitch, .control, or -val call, respectively.\
    \ It sets the generator by default, but will set the transformer if you\
    \ prefix another `-`. `>>` is special cased to only create a note\
    \ transformer, whose name will be prefixed with `>`. This is used to set\
    \ an instrument transformer, which can apply a transformer when an\
    \ instrument is set by the title of a note track, as implemented by\
    \ by `note-track`.\
    \\nYou need quoting for symbols that don't match 'Derive.Parse.p_symbol'.\
    \\nE.g.: set note generator: `^phrase = some-block`,\
    \ note transformer: `^-mute = +mute+loose`,\
    \ control transfomrer: `'.-i' = t`, val call: `'-4c' = 5c`.\
    \\nYou can bypass all this cryptic prefix garbage by using a `ky` file.\
    \ It has more space available so it can use a more readable syntax.\
    \\nIf you bind a call to a quoted expression, this creates a new\
    \ call: `^abc = \"(a b c)` will create a `abc` call, which is a macro for\
    \ `a b c`. The created call does not take arguments.\
    \\nSet constant signals by assigning to a signal literal: `%c = .5` or\
    \ pitch: `#p = (4c)`.  `# = (4c)` sets the default pitch signal.\
    \ You can rename a signal via `%a = %b` or `#x = #y`. Control signal\
    \ assignment also supports the same merge operators as the control track:\
    \ `%a = add .5` or `%a = add %b`.  However, the second example throws an\
    \ error if `%b` is a ControlFunction. `%a = _ .5` will combine with `a`'s\
    \ default merge operator. Assigning to `_` unsets the control, and any\
    \ control function."
    -- Previously > was for binding note calls, but that was taken by
    -- instrument aliasing.  ^ at least looks like a rotated >.

equal_transformer :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
equal_transformer args deriver =
    either Derive.throw_arg_error ($deriver) $ case Derive.passed_vals args of
        -- The first arg is definitely a symbol because that's how the parser
        -- parses it.
        [BaseTypes.VSymbol lhs, val] -> parse_equal Nothing lhs val
        [BaseTypes.VSymbol lhs, BaseTypes.VSymbol merge, val] ->
            parse_equal (Just (Merge merge)) lhs val
        [BaseTypes.VSymbol lhs, BaseTypes.VNotGiven, val] ->
            parse_equal (Just Default) lhs val
        args -> Left $ "unexpected arg types: "
            <> Text.intercalate ", " (map (pretty . ValType.type_of) args)

parse_equal :: Maybe Merge -> BaseTypes.Symbol -> BaseTypes.Val
    -> Either Text (Derive.Deriver a -> Derive.Deriver a)
parse_equal Nothing (BaseTypes.Symbol lhs) rhs
    -- Assign to call.
    | Just new <- Text.stripPrefix "^" lhs = Right $
        override_call new rhs "note"
            (Derive.s_generator#Derive.s_note)
            (Derive.s_transformer#Derive.s_note)
    | Just new <- Text.stripPrefix ">>" lhs = Right $
        override_transformer (">" <> new) rhs "note"
            (Derive.s_transformer#Derive.s_note)
    | Just new <- Text.stripPrefix "*" lhs = Right $
        override_call new rhs "pitch"
            (Derive.s_generator#Derive.s_pitch)
            (Derive.s_transformer#Derive.s_pitch)
    | Just new <- Text.stripPrefix "." lhs = Right $
        override_call new rhs "control"
            (Derive.s_generator#Derive.s_control)
            (Derive.s_transformer#Derive.s_control)
    | Just new <- Text.stripPrefix "-" lhs = Right $ override_val_call new rhs
parse_equal Nothing (BaseTypes.Symbol lhs) rhs
    -- Create instrument alias.
    | Just new <- Text.stripPrefix ">" lhs = case rhs of
        BaseTypes.VInstrument inst -> Right $
            Derive.with_instrument_alias (Score.Instrument new) inst
        _ -> Left $ "aliasing an instrument expected an instrument rhs, got "
            <> pretty (ValType.type_of rhs)
parse_equal maybe_merge lhs rhs
    -- Assign to control.
    | Just control <- is_control =<< parse_val lhs = case rhs of
        BaseTypes.VControlRef rhs -> Right $ \deriver ->
            Typecheck.to_signal_or_function rhs >>= \x -> case x of
                Left sig -> do
                    merger <- get_merger control maybe_merge
                    Derive.with_merged_control merger control sig deriver
                Right f -> case maybe_merge of
                    Just merge -> Derive.throw_arg_error $ merge_error merge
                    Nothing -> Derive.with_control_function control f deriver
        BaseTypes.VNum rhs -> Right $ \deriver -> do
            merger <- get_merger control maybe_merge
            Derive.with_merged_control merger control (fmap Signal.constant rhs)
                deriver
        BaseTypes.VControlFunction f -> case maybe_merge of
            Just merge -> Left $ merge_error merge
            Nothing -> Right $ Derive.with_control_function control f
        BaseTypes.VNotGiven -> Right $ Derive.remove_controls [control]
        _ -> Left $ "binding a control expected a control, num, control\
            \ function, or _, but got " <> pretty (ValType.type_of rhs)
    where
    is_control (BaseTypes.VControlRef (BaseTypes.LiteralControl c)) = Just c
    is_control _ = Nothing
parse_equal maybe_merge lhs rhs
    -- Assign to pitch control.
    | Just control <- is_pitch =<< parse_val lhs = case rhs of
        BaseTypes.VPitch rhs -> Right $ \deriver -> do
            merger <- get_pitch_merger maybe_merge
            Derive.with_merged_pitch merger control (PSignal.constant rhs)
                deriver
        BaseTypes.VPControlRef rhs -> Right $ \deriver -> do
            sig <- Call.to_psignal rhs
            merger <- get_pitch_merger maybe_merge
            Derive.with_merged_pitch merger control sig deriver
        BaseTypes.VNum (Score.Typed Score.Nn nn) -> Right $ \deriver -> do
            merger <- get_pitch_merger maybe_merge
            Derive.with_merged_pitch merger control
                (PSignal.constant (PSignal.nn_pitch (Pitch.nn nn))) deriver
        _ -> Left $ "binding a pitch signal expected a pitch, pitch"
            <> " control, or nn, but got " <> pretty (ValType.type_of rhs)
    where
    is_pitch (BaseTypes.VPControlRef (BaseTypes.LiteralControl c)) = Just c
    is_pitch _ = Nothing
parse_equal (Just merge) _ _ = Left $ merge_error merge
parse_equal Nothing lhs val = Right $ Derive.with_val lhs val

merge_error :: Merge -> Text
merge_error merge = "operator is only supported when assigning to a control: "
    <> case merge of
        Default -> "_"
        Merge sym -> pretty sym

data Merge = Default | Merge BaseTypes.CallId deriving (Show)

-- | Unlike 'Derive.MergeDefault', the default is Derive.Set.
get_merger :: Score.Control -> Maybe Merge
    -> Derive.Deriver (Derive.Merger Signal.Control)
get_merger control maybe_merge = case maybe_merge of
    Nothing -> return Derive.Set
    Just Default -> Derive.get_default_merger control
    Just (Merge merge) -> Derive.get_control_merge merge

get_pitch_merger :: Maybe Merge
    -> Derive.Deriver (Derive.Merger PSignal.PSignal)
get_pitch_merger maybe_merge = case maybe_merge of
    Nothing -> return Derive.Set
    Just Default -> return Derive.Set
    Just (Merge name) -> Derive.get_pitch_merger name

parse_val :: BaseTypes.Symbol -> Maybe BaseTypes.Val
parse_val = either (const Nothing) Just . Parse.parse_val . BaseTypes.unsym

-- | Look up a call with the given CallId and add it as an override to the
-- scope given by the lenses.  I wanted to pass just one lens, but apparently
-- they're not sufficiently polymorphic.
override_call :: (Derive.Callable d1, Derive.Callable d2) =>
    Text -> BaseTypes.Val -> Text
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Generator d1))
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Transformer d2))
    -> Derive.Deriver a -> Derive.Deriver a
override_call lhs rhs name generator transformer deriver
    | Just stripped <- Text.stripPrefix "-" lhs =
        override_transformer stripped rhs name transformer deriver
    | otherwise = override_generator_scope
        =<< resolve_source (name <> " generator") generator
            quoted_generator rhs
    where
    override_generator_scope call = Derive.with_scopes modify deriver
        where modify = generator#Derive.s_override %= (single_lookup lhs call :)

-- | Make an expression into a transformer and stick it into the
-- 'Derive.s_override' slot.
override_transformer :: Derive.Callable d => Text -> BaseTypes.Val -> Text
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Transformer d))
    -> Derive.Deriver a -> Derive.Deriver a
override_transformer lhs rhs name transformer deriver =
    override_scope =<< resolve_source (name <> " transformer") transformer
            quoted_transformer rhs
    where
    override_scope call = Derive.with_scopes
        (transformer#Derive.s_override %= (single_lookup lhs call :)) deriver

-- | A VQuoted becomes a call, a Symbol is expected to name a call, and
-- everything else is turned into a Symbol via ShowVal.  This will cause
-- a parse error for un-showable Vals, but what else is new?
resolve_source :: Text -> Lens Derive.Scopes (Derive.ScopeType a)
    -> (BaseTypes.Quoted -> a) -> BaseTypes.Val -> Derive.Deriver a
resolve_source name lens make_quoted rhs = case rhs of
    BaseTypes.VQuoted quoted -> return $ make_quoted quoted
    BaseTypes.VSymbol call_id -> get_call name (lens #$) call_id
    _ -> get_call name (lens #$) (BaseTypes.Symbol (ShowVal.show_val rhs))

override_val_call :: Text -> BaseTypes.Val -> Derive.Deriver a
    -> Derive.Deriver a
override_val_call lhs rhs deriver = do
    call <- resolve_source "val" Derive.s_val quoted_val_call rhs
    let modify = Derive.s_val#Derive.s_override
            %= (single_val_lookup lhs call :)
    Derive.with_scopes modify deriver

get_call :: Text -> (Derive.Scopes -> Derive.ScopeType call)
    -> BaseTypes.CallId -> Derive.Deriver call
get_call name get call_id =
    maybe (Derive.throw $ Eval.unknown_call_id name call_id)
        return =<< Derive.lookup_with get call_id

single_lookup :: Text -> Derive.Call d
    -> Derive.LookupCall (Derive.Call d)
single_lookup name = Derive.LookupMap . Map.singleton (BaseTypes.Symbol name)

single_val_lookup :: Text -> Derive.ValCall
    -> Derive.LookupCall Derive.ValCall
single_val_lookup name =
    Derive.LookupMap . Map.singleton (BaseTypes.Symbol name)


-- * quoted

-- | Create a new call from a quoted expression.  This is flirting with
-- function definiion, but is really just macro expansion, with all the
-- variable capture problems implied.  But since the only variables I have are
-- calls maybe it's not so bad.
quoted_generator :: Derive.Callable d => BaseTypes.Quoted -> Derive.Generator d
quoted_generator quoted@(BaseTypes.Quoted expr) =
    Derive.generator quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.show_val quoted)
    $ Sig.call0 $ \args -> Eval.eval_expr False (Args.context args) expr

quoted_transformer :: Derive.Callable d => BaseTypes.Quoted
    -> Derive.Transformer d
quoted_transformer quoted@(BaseTypes.Quoted expr) =
    Derive.transformer quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.show_val quoted)
    $ Sig.call0t $ \args deriver ->
        Eval.eval_transformers (Args.context args) (NonEmpty.toList expr)
            deriver

quoted_val_call :: BaseTypes.Quoted -> Derive.ValCall
quoted_val_call quoted = Derive.val_call quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.show_val quoted)
    $ Sig.call0 $ \args -> do
        call <- case quoted of
            BaseTypes.Quoted (call :| []) -> return $ BaseTypes.ValCall call
            _ -> Derive.throw $
                "expected a val call, but got a full expression: "
                <> ShowVal.show_val quoted
        Eval.eval (Args.context args) call

-- | Pseudo-module for val calls generated from a quoted expression.
quoted_module :: Module.Module
quoted_module = "quoted"


-- * other

c_default_merge :: Derive.Callable d => Derive.Transformer d
c_default_merge = Derive.transformer Module.prelude "default-merge" mempty
    "Set the default merge operators for controls. These apply when the\
    \ control track doesn't have an explicit operator."
    $ Sig.callt ((,)
    <$> Sig.required "merge" "Merge operator, from\
        \ 'Derive.Deriver.Monad.default_control_op_map'."
    <*> Sig.many1 "control" "Control names."
    ) $ \(op_name, controls) _args deriver -> do
        merge <- Derive.get_control_merge op_name
        let defaults = Map.fromList $ map (, merge) (NonEmpty.toList controls)
        Internal.with_default_merge defaults deriver
