-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | I don't know where to put these.
module Derive.Call.Misc where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("multiple", c_multiple)
    , ("solo", c_solo)
    ]
    <> poly_calls

control_calls :: Derive.CallMaps Derive.Control
control_calls = poly_calls

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = poly_calls

poly_calls :: Derive.Taggable d => Derive.CallMaps d
poly_calls = Derive.transformer_call_map
    [ ("when-e", c_when_e False)
    , ("unless-e", c_when_e True)
    , ("when-c", c_when_c False)
    , ("unless-c", c_when_c True)
    ]

c_multiple :: Derive.Transformer Derive.Note
c_multiple = Derive.transformer Module.prelude "multiple" mempty
    "Derive the transformed score under different transformers."
    $ Sig.callt (Sig.many1 "transformer" "Derive under each transformer.")
    $ \transformers args deriver ->
        mconcat $ map (apply (Args.info args) deriver)
            (NonEmpty.toList transformers)
    where
    apply cinfo deriver trans =
        Eval.eval_transformers cinfo (to_transformer trans) deriver

to_transformer :: Either TrackLang.Quoted Score.Instrument -> [TrackLang.Call]
to_transformer val = case val of
    Left (TrackLang.Quoted expr) -> NonEmpty.toList expr
    Right inst -> [TrackLang.literal_call ParseTitle.note_track_symbol
        [TrackLang.to_val inst]]

c_when_c :: Derive.Taggable d => Bool -> Derive.Transformer d
c_when_c inverted = Derive.transformer Module.prelude "when-c" mempty
    "Only derive if the control has the given value. E.g., you can use a\
    \ `%var` control to select among different variations."
    $ Sig.callt ((,)
    <$> Sig.required "val" "Value."
    <*> Sig.defaulted "control" (Sig.control "var" 0) "Control."
    ) $ \(val, control) args deriver ->
        ifM (invert . has_control control val =<< Args.real_start args)
            deriver (return mempty)
    where invert = if inverted then (not <$>) else id

has_control :: TrackLang.ValControl -> Int -> RealTime -> Derive.Deriver Bool
has_control control val pos = do
    cval <- Util.control_at control pos
    return $ round cval == val

c_when_e :: Derive.Taggable d => Bool -> Derive.Transformer d
c_when_e inverted = Derive.transformer Module.prelude "when-e" mempty
    "Only derive if environ value is set to the given value. In a block\
    \ derived multiple times by different instruments, this can be used to\
    \ solo a bit of score to one particular instrument."
    $ Sig.callt ((,)
    <$> Sig.required "name" "Environ key."
    <*> Sig.defaulted "value" Nothing "Environ value. If not given, require\
        \ only that the environ key is set."
    ) $ \(name, maybe_value) _args deriver ->
        ifM (invert $ has_environ name maybe_value) deriver (return mempty)
    where invert = if inverted then (not <$>) else id

c_solo :: Derive.Transformer Derive.Note
c_solo = Derive.transformer Module.prelude "solo" mempty
    "Only derive if `inst` is set to the given value. This is a specialized\
    \ version of `when-e`."
    $ Sig.callt (Sig.required "inst" "Instrument.")
    $ \inst _args deriver ->
        ifM (has_environ Environ.instrument (Just (TrackLang.VInstrument inst)))
            deriver mempty

has_environ :: TrackLang.ValName -> Maybe TrackLang.Val -> Derive.Deriver Bool
has_environ name maybe_val = Derive.lookup_val name >>= \x -> case x of
    Nothing -> return False
    Just env_val -> case maybe_val of
        Nothing -> return True
        Just val -> case TrackLang.vals_equal val env_val of
            Nothing -> Derive.throw $ untxt $ "vals can't be compared: "
                <> ShowVal.show_val val <> " " <> ShowVal.show_val env_val
            Just t -> return t
