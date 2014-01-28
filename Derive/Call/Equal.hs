-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
-- | Export 'c_equal' call, which implements @=@.
module Derive.Call.Equal where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Call as Call
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.ParseBs as ParseBs
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal


c_equal :: (Derive.Callable d) => Derive.Transformer d
c_equal = Derive.transformer "equal" (Tags.prelude <> Tags.subs) equal_doc
    (Sig.parsed_manually equal_arg_doc equal_transformer)

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
    \ by default, but will set the transformer if you add another `-`.  You\
    \ need quoting for symbols that don't match 'Derive.ParseBs.p_symbol'.\
    \ E.g.: set note generator: `>x = some-block`, note transformer: `>-x = t`,\
    \ control transfomrer: `'.-i' = t`, pitch val call: `'-4c' = 5c`.\
    \\nSet constant signals by assigning to a signal literal: `%c = .5` or\
    \ pitch: `#p = (4c)`.  `# = (4c)` sets the default pitch signal."

equal_transformer :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
equal_transformer args deriver = case Derive.passed_vals args of
    [TrackLang.VSymbol assignee, val] ->
        case parse_equal assignee val deriver of
            Left err -> Derive.throw_arg_error err
            Right d -> d
    args -> Derive.throw_arg_error $ "unexpected arg types: "
        <> Seq.join ", " (map (Pretty.pretty . TrackLang.type_of) args)

parse_equal :: TrackLang.Symbol -> TrackLang.Val -> Derive.Deriver a
    -> Either String (Derive.Deriver a)
parse_equal (TrackLang.Symbol assignee) (TrackLang.VSymbol sym) deriver
    | Just new <- Text.stripPrefix ">" assignee = Right $
        override_call new sym deriver "note"
            (Derive.s_generator#Derive.s_note)
            (Derive.s_transformer#Derive.s_note)
    | Just new <- Text.stripPrefix "*" assignee = Right $
        override_call new sym deriver "pitch"
            (Derive.s_generator#Derive.s_pitch)
            (Derive.s_transformer#Derive.s_pitch)
    | Just new <- Text.stripPrefix "." assignee = Right $
        override_call new sym deriver "control"
            (Derive.s_generator#Derive.s_control)
            (Derive.s_transformer#Derive.s_control)
    | Just new <- Text.stripPrefix "-" assignee = Right $
        override_val_call new sym deriver
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
            \ function, but got " <> Pretty.pretty (TrackLang.type_of val)
    | Just control <- is_pitch assignee = case val of
        TrackLang.VPitch val -> Right $
            Derive.with_pitch control (PitchSignal.constant val) deriver
        TrackLang.VPitchControl val -> Right $ do
            sig <- Util.to_pitch_signal val
            Derive.with_pitch control sig deriver
        _ -> Left $ "binding a pitch signal expects a pitch or pitch"
            <> " control, but got " <> Pretty.pretty (TrackLang.type_of val)
    where
    is_control (TrackLang.VControl (TrackLang.LiteralControl c)) = Just c
    is_control _ = Nothing
    is_pitch (TrackLang.VPitchControl (TrackLang.LiteralControl c))
        | c == Controls.null = Just Nothing
        | otherwise = Just (Just c)
    is_pitch _ = Nothing
parse_equal assignee val deriver = Right $ Derive.with_val assignee val deriver

parse_val :: TrackLang.Symbol -> Maybe TrackLang.Val
parse_val = either (const Nothing) Just . ParseBs.parse_val . TrackLang.unsym

-- | Look up a call with the given CallId and add it as an override to the
-- scope given by the lenses.  I wanted to pass just one lens, but apparently
-- they're not sufficiently polymorphic.
override_call :: Text -> TrackLang.CallId -> Derive.Deriver a
    -> Text
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Call d1))
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Call d2))
    -> Derive.Deriver a
override_call assignee source deriver name generator transformer
    | Just stripped <- Text.stripPrefix "-" assignee =
        override_scope stripped (name <> " transformer") transformer
    | otherwise = override_scope assignee (name <> " generator") generator
    where
    override_scope assignee name lens = do
        call <- get_call name (lens #$) source
        let modify = lens#Derive.s_override %= (single_lookup assignee call :)
        Derive.with_scopes modify deriver

override_val_call :: Text -> TrackLang.CallId -> Derive.Deriver a
    -> Derive.Deriver a
override_val_call assignee source deriver = do
    call <- get_call "val" (Derive.s_val #$) source
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
single_lookup name = Derive.map_lookup . Map.singleton (TrackLang.Symbol name)

single_val_lookup :: Text -> Derive.ValCall
    -> Derive.LookupCall Derive.ValCall
single_val_lookup name =
    Derive.map_val_lookup . Map.singleton (TrackLang.Symbol name)
