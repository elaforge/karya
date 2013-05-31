-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.CallTest where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Scope as Scope
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


transform :: (Derive.EventDeriver -> Derive.EventDeriver) -> Derive.Result
transform trans = DeriveTest.derive State.empty $
    Derive.with_constant_pitch Nothing DeriveTest.default_scale
        (DeriveTest.mkpitch12 "4c") (trans (DeriveTest.c_note 0 1))

run_pitch :: [(ScoreTime, String)] -> [(RealTime, Pitch.NoteNumber)]
run_pitch = run_with_scale "twelve"

run_with_scale :: String -> [(ScoreTime, String)]
    -> [(RealTime, Pitch.NoteNumber)]
run_with_scale scale events = extract $ DeriveTest.derive_tracks $
    [ (">", [(0, 10, "")])
    , ('*' : scale, [(start, 0, text) | (start, text) <- events])
    ]
    where extract = head . DeriveTest.extract_events DeriveTest.e_nns

-- | Run a control track and extract the control signal it produces.
run_control :: [(ScoreTime, String)] -> [(Signal.X, Signal.Y)]
run_control events = extract $ DeriveTest.derive_tracks
    [ (">", [(0, 10, "")])
    , ("cont", [(start, 0, text) | (start, text) <- events])
    ]
    where
    extract = head . DeriveTest.extract_events
        (Signal.unsignal . Score.typed_val . get . Score.event_controls)
    get fm = case Map.lookup (Score.Control "cont") fm of
        Nothing -> error "expected a 'cont' control"
        Just c -> c

-- * call map

with_note_call :: Text -> Derive.NoteCall
    -> Derive.Deriver a -> Derive.Deriver a
with_note_call name call =
    Derive.with_scope $ Scope.add_note_lookup (single_lookup name call)

with_control_call :: Text -> Derive.ControlCall
    -> Derive.Deriver a -> Derive.Deriver a
with_control_call name call = with_control_lookup (single_lookup name call)

with_control_lookup :: Derive.LookupCall Derive.ControlCall
    -> Derive.Deriver a -> Derive.Deriver a
with_control_lookup = Derive.with_scope . Scope.add_control_lookup

with_val_call :: Text -> Derive.ValCall
    -> Derive.Deriver a -> Derive.Deriver a
with_val_call name call =
    Derive.with_scope $ Scope.add_val_lookup (single_val_lookup name call)

single_lookup :: Text -> Derive.Call d -> Derive.LookupCall (Derive.Call d)
single_lookup name call =
    Derive.map_lookup (Map.singleton (TrackLang.Symbol name) call)

single_val_lookup :: Text -> Derive.ValCall
    -> Derive.LookupCall Derive.ValCall
single_val_lookup name call =
    Derive.map_val_lookup (Map.singleton (TrackLang.Symbol name) call)

-- * calls

c_show_args :: (Derive.Derived d) => Derive.Call d
c_show_args = Derive.generator "show-args" mempty "doc" $
    Sig.parsed_manually "doc" $ \args -> do
        Log.warn $ Seq.join ", " $
            map (untxt . ShowVal.show_val) (Derive.passed_vals args)
        return []

generator :: (Derive.Derived d) =>
    (Derive.PassedArgs (Derive.Elem d) -> Derive.LogsDeriver d)
    -> Derive.Call d
generator = Derive.stream_generator "test" mempty "test doc" . Sig.call0

-- * PassedArgs

expr :: String -> TrackLang.Expr
expr = either (error . ("CallTest.expr: " ++)) id . ParseBs.parse_expr
    . ParseBs.from_string

val :: Text -> TrackLang.Val
val = either (error . ("CallTest.val: " ++)) id . ParseBs.parse_val
