-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
-- | Export 'c_equal' call, which implements @=@.
module Derive.Call.Equal (
    c_equal
    , equal_arg_doc, equal_doc
    , equal_transformer
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Parse as Parse
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal


c_equal :: Derive.Callable d => Derive.Transformer d
c_equal = Derive.transformer Module.prelude "equal" Tags.subs
    equal_doc (Sig.parsed_manually equal_arg_doc equal_transformer)

equal_arg_doc :: Text
equal_arg_doc = "Many types."

equal_doc :: Text
equal_doc =
    "Evaluate the deriver with a value set. A special rule means this can be\
    \ called infix.  The arguments can take many forms to set different kinds\
    \ of values.\
    \\nSet environ values by setting a plain symbol or unset it by assigning\
    \ to `_`: `x = 42` or `x = _`.\
    \\nIf the symbol is prefixed with `>`, `*`, `.`, or `-`, it will set a\
    \ note, pitch, control, or val call, respectively. It sets the generator\
    \ by default, but will set the transformer if you prefix another `-`.  You\
    \ need quoting for symbols that don't match 'Derive.Parse.p_symbol'.\
    \ E.g.: set note generator: `>x = some-block`, note transformer: `>-x = t`,\
    \ control transfomrer: `'.-i' = t`, pitch val call: `'-4c' = 5c`.\
    \\nIf you bind a call to a quoted expression, this creates a new call:\
    \ `>new = \"(a b c)` will create a `new` call, which is a macro for\
    \ `a b c`. It looks like function definition, but it's purely a macro,\
    \ so don't bind `>n = \"(n x)` unless you like infinite recursion.\
    \ Also, the created call does not take arguments (yet!).\
    \\nSet constant signals by assigning to a signal literal: `%c = .5` or\
    \ pitch: `#p = (4c)`.  `# = (4c)` sets the default pitch signal."

equal_transformer :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
equal_transformer args deriver = case Derive.passed_vals args of
    [TrackLang.VSymbol assignee, val] ->
        case parse_equal assignee val deriver of
            Left err -> Derive.throw_arg_error err
            Right d -> d
    args -> Derive.throw_arg_error $ "unexpected arg types: "
        <> Seq.join ", " (map (pretty . TrackLang.type_of) args)

parse_equal :: TrackLang.Symbol -> TrackLang.Val -> Derive.Deriver a
    -> Either String (Derive.Deriver a)
parse_equal (TrackLang.Symbol assignee) (is_source -> Just source) deriver
    | Just new <- Text.stripPrefix ">" assignee = Right $
        override_call new source deriver "note"
            (Derive.s_generator#Derive.s_note)
            (Derive.s_transformer#Derive.s_note)
    | Just new <- Text.stripPrefix "*" assignee = Right $
        override_call new source deriver "pitch"
            (Derive.s_generator#Derive.s_pitch)
            (Derive.s_transformer#Derive.s_pitch)
    | Just new <- Text.stripPrefix "." assignee = Right $
        override_call new source deriver "control"
            (Derive.s_generator#Derive.s_control)
            (Derive.s_transformer#Derive.s_control)
    | Just new <- Text.stripPrefix "-" assignee = Right $
        override_val_call new source deriver
parse_equal (parse_val -> Just assignee) val deriver
    | Just control <- is_control assignee = case val of
        TrackLang.VControl val -> Right $
            Util.to_signal_or_function val >>= \x -> case x of
                Left sig -> Derive.with_control control sig deriver
                Right f -> Derive.with_control_function control f deriver
        TrackLang.VNum val -> Right $
            Derive.with_control control (fmap Signal.constant val) deriver
        TrackLang.VControlFunction f -> Right $
            Derive.with_control_function control f deriver
        _ -> Left $ "binding a control expects a control, num, or control\
            \ function, but got " <> pretty (TrackLang.type_of val)
    | Just control <- is_pitch assignee = case val of
        TrackLang.VPitch val -> Right $
            Derive.with_pitch control (PitchSignal.constant val) deriver
        TrackLang.VPitchControl val -> Right $ do
            sig <- Util.to_pitch_signal val
            Derive.with_pitch control sig deriver
        _ -> Left $ "binding a pitch signal expects a pitch or pitch"
            <> " control, but got " <> pretty (TrackLang.type_of val)
    where
    is_control (TrackLang.VControl (TrackLang.LiteralControl c)) = Just c
    is_control _ = Nothing
    is_pitch (TrackLang.VPitchControl (TrackLang.LiteralControl c))
        | c == Controls.null = Just Nothing
        | otherwise = Just (Just c)
    is_pitch _ = Nothing
parse_equal assignee val deriver = Right $ Derive.with_val assignee val deriver

type Source = Either TrackLang.CallId TrackLang.Quoted

is_source :: TrackLang.Val -> Maybe Source
is_source (TrackLang.VSymbol a) = Just $ Left a
is_source (TrackLang.VQuoted a) = Just $ Right a
is_source _ = Nothing

parse_val :: TrackLang.Symbol -> Maybe TrackLang.Val
parse_val = either (const Nothing) Just . Parse.parse_val . TrackLang.unsym

-- | Look up a call with the given CallId and add it as an override to the
-- scope given by the lenses.  I wanted to pass just one lens, but apparently
-- they're not sufficiently polymorphic.
override_call :: (Derive.Callable d1, Derive.Callable d2)
    => Text -> Source -> Derive.Deriver a -> Text
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Generator d1))
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Transformer d2))
    -> Derive.Deriver a
override_call assignee source deriver name_ generator transformer
    | Just stripped <- Text.stripPrefix "-" assignee =
        override_scope stripped transformer
            =<< resolve (name_ <> " transformer")
                transformer (return . quoted_transformer) source
    | otherwise = override_scope assignee generator
        =<< resolve (name_ <> " generator")
            generator (return . quoted_generator) source
    where
    override_scope assignee lens call =
        Derive.with_scopes modify deriver
        where modify = lens#Derive.s_override %= (single_lookup assignee call :)
    resolve name lens = either (get_call name (lens #$))

override_val_call :: Text -> Source -> Derive.Deriver a -> Derive.Deriver a
override_val_call assignee source deriver = do
    call <- either (get_call "val" (Derive.s_val #$)) (return . quoted_val_call)
        source
    let modify = Derive.s_val#Derive.s_override
            %= (single_val_lookup assignee call :)
    Derive.with_scopes modify deriver

get_call :: Text -> (Derive.Scopes -> Derive.ScopeType call)
    -> TrackLang.CallId -> Derive.Deriver call
get_call name get call_id =
    maybe (Derive.throw $ untxt $ Call.unknown_call_id name call_id)
        return =<< Derive.lookup_with get call_id

single_lookup :: Text -> Derive.Call d
    -> Derive.LookupCall (Derive.Call d)
single_lookup name = Derive.LookupMap . Map.singleton (TrackLang.Symbol name)

single_val_lookup :: Text -> Derive.ValCall
    -> Derive.LookupCall Derive.ValCall
single_val_lookup name =
    Derive.LookupMap . Map.singleton (TrackLang.Symbol name)


-- * quoted

-- | Create a new call from a quoted expression.  This is flirting with
-- function definiion, but is really just macro expansion, with all the
-- variable capture problems implied.  But since the only variables I have are
-- calls maybe it's not so bad.
quoted_generator :: Derive.Callable d => TrackLang.Quoted -> Derive.Generator d
quoted_generator quoted@(TrackLang.Quoted expr) =
    Derive.make_call quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.show_val quoted)
    $ Sig.call0 $ \args -> Call.eval_expr (quoted_cinfo args quoted) expr

quoted_transformer :: Derive.Callable d => TrackLang.Quoted
    -> Derive.Transformer d
quoted_transformer quoted@(TrackLang.Quoted expr) =
    Derive.make_call quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.show_val quoted)
    $ Sig.call0t $ \args deriver ->
        Call.apply_transformers (quoted_cinfo args quoted)
            (NonEmpty.toList expr) deriver

quoted_val_call :: TrackLang.Quoted -> Derive.ValCall
quoted_val_call quoted = Derive.val_call quoted_module "quoted-call" mempty
    ("Created from expression: " <> ShowVal.show_val quoted)
    $ Sig.call0 $ \args -> do
        call <- case quoted of
            TrackLang.Quoted (call :| []) -> return $ TrackLang.ValCall call
            _ -> Derive.throw $
                "expected a val call, but got a full expression: "
                <> untxt (ShowVal.show_val quoted)
        Call.eval (quoted_cinfo args quoted) call

quoted_cinfo :: Derive.PassedArgs d -> TrackLang.Quoted -> Derive.CallInfo d
quoted_cinfo args (TrackLang.Quoted expr) = (Derive.passed_info args)
    { Derive.info_expr = ShowVal.show_val expr }

-- | Pseudo-module for val calls generated from a quoted expression.
quoted_module :: Module.Module
quoted_module = "quoted"
