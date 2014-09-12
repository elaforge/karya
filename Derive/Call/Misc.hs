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
import qualified Derive.Eval as Eval
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("multiple", c_multiple)
    ]

c_multiple :: Derive.Transformer Derive.Note
c_multiple = Derive.transformer Module.prelude "multiple" mempty
    "Derive the arguments under different transformers."
    $ Sig.callt (Sig.many1 "transformer" "Derive under each transformer.")
    $ \transformers args deriver ->
        mconcat $ map (apply (Args.info args) deriver)
            (NonEmpty.toList transformers)
    where
    apply cinfo deriver trans = Eval.apply_transformers cinfo
        (to_transformer trans) deriver

to_transformer :: Either TrackLang.Quoted
    (Either TrackLang.Symbol Score.Instrument) -> [TrackLang.Call]
to_transformer val = case val of
    Left (TrackLang.Quoted expr) -> NonEmpty.toList expr
    Right (Left sym) -> [TrackLang.call0 sym]
    Right (Right inst) ->
        [TrackLang.call0 (TrackLang.Symbol (Score.inst_name inst))]
