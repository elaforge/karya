-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | I don't know where to put these.
module Derive.Call.Misc where
import qualified Data.List.NonEmpty as NonEmpty

import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("multiple", c_multiple)
    , ("solo-env", c_solo_env)
    , ("solo", c_solo)
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
        Eval.apply_transformers True cinfo (to_transformer trans) deriver

to_transformer :: Either TrackLang.Quoted Score.Instrument -> [TrackLang.Call]
to_transformer val = case val of
    Left (TrackLang.Quoted expr) -> NonEmpty.toList expr
    Right inst -> [TrackLang.literal_call ParseTitle.note_track_symbol
        [TrackLang.to_val inst]]

c_solo_env :: Derive.Transformer Derive.Note
c_solo_env = Derive.transformer Module.prelude "solo-env" mempty
    "Only derive if environ value is set to the given value. In a block\
    \ derived multiple times by different instruments, this can be used to\
    \ solo a bit of score to one particular instrument."
    $ Sig.callt ((,)
    <$> Sig.required "name" "Environ key."
    <*> Sig.required "value" "Environ value."
    ) $ \(name, value) _args deriver ->
        ifM (has_environ name value) deriver mempty

c_solo :: Derive.Transformer Derive.Note
c_solo = Derive.transformer Module.prelude "solo" mempty
    "Only derive if `inst` is set to the given value. This is a specialized\
    \ version of `solo-env`."
    $ Sig.callt (Sig.required "inst" "Instrument.")
    $ \inst _args deriver ->
        ifM (has_environ Environ.instrument (TrackLang.VInstrument inst))
            deriver mempty

has_environ :: TrackLang.ValName -> TrackLang.Val -> Derive.Deriver Bool
has_environ name val = Derive.lookup_val name >>= \x -> case x of
    Nothing -> return False
    Just env_val -> case TrackLang.vals_equal val env_val of
        Nothing -> Derive.throw $ untxt $ "vals can't be compared: "
            <> ShowVal.show_val val <> " " <> ShowVal.show_val env_val
        Just t -> return t
