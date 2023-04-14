-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for testing calls.
module Derive.Call.CallTest where
import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types


transform :: (Derive.NoteDeriver -> Derive.NoteDeriver) -> Derive.Result
transform trans = DeriveTest.derive Ui.empty $
    Derive.with_constant_pitch
        (DeriveTest.mkpitch12 "4c") (trans (DeriveTest.c_note 0 1))

run_pitch_ :: Text -> [(ScoreTime, Text)] -> Derive.Result
run_pitch_ title events = DeriveTest.derive_tracks title
    [ (">", [(0, 10, "")])
    , ("*", [(start, 0, text) | (start, text) <- events])
    ]

run_pitch :: Text -> [(ScoreTime, Text)] -> [(RealTime, Pitch.NoteNumber)]
run_pitch title = extract . run_pitch_ title
    where extract = head . DeriveTest.extract_events DeriveTest.e_nns

-- | Run a control track and extract the control signal it produces.
run_control :: [(ScoreTime, Text)] -> [(Signal.X, Signal.Y)]
run_control events = run_control_dur [(p, 0, t) | (p, t) <- events]

run_control_dur :: [UiTest.EventSpec] -> [(Signal.X, Signal.Y)]
run_control_dur events = extract $
    DeriveTest.derive_tracks "" [(">", [(0, 10, "")]), ("cont", events)]
    where
    extract = Seq.drop_dups id . head . DeriveTest.extract_events
        (Signal.to_pairs . ScoreT.typed_val . get . Score.event_controls)
    get fm = case Map.lookup "cont" fm of
        Nothing -> error "expected a 'cont' control"
        Just c -> c

-- * call map

-- | Example:
--
-- > DeriveTest.derive_tracks_setup (CallTest.with_note_generator "g" c_gen)
-- > where c_gen = CallTest.generator $ \args -> do ...
with_note_generator :: Expr.Symbol -> Derive.Generator Derive.Note
    -> DeriveTest.SetupA a
with_note_generator name call = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_generator#Derive.s_note %= override (Derive.single_call name call)

with_pitch_generator :: Expr.Symbol -> Derive.Generator Derive.Pitch
    -> DeriveTest.SetupA a
with_pitch_generator name call = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_generator#Derive.s_pitch %= override (Derive.single_call name call)

with_control_generator :: Expr.Symbol -> Derive.Generator Derive.Control
    -> DeriveTest.SetupA a
with_control_generator name call =
    DeriveTest.with_deriver $ Derive.with_scopes $
        Derive.s_generator#Derive.s_control
            %= override (Derive.single_call name call)

with_note_generators :: [(Expr.Symbol, Derive.Generator Derive.Note)]
    -> DeriveTest.SetupA a
with_note_generators calls = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_generator#Derive.s_note %= override (lookup_map calls)

with_note_transformer :: Expr.Symbol -> Derive.Transformer Derive.Note
    -> DeriveTest.SetupA a
with_note_transformer name call = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_transformer#Derive.s_note
        %= override (Derive.single_call name call)

with_val_call :: Expr.Symbol -> Derive.ValCall -> DeriveTest.SetupA a
with_val_call name call = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_val %= override (Derive.single_call name call)

override :: Derive.CallMap call -> Derive.ScopePriority call
    -> Derive.ScopePriority call
override = Derive.add_priority Derive.PrioOverride

lookup_map :: [(Expr.Symbol, call)] -> Derive.CallMap call
lookup_map calls = Derive.CallMap
    { call_map = Map.fromList calls
    , call_patterns = []
    }

-- * calls

-- | Run a val call, and return what it returned.
run_val :: Maybe Text -> Text -> (Maybe DeriveT.Val, [Text])
run_val transform call =
    extract $ DeriveTest.derive_tracks_setup
        (with_note_generator "capture" c_capture) ""
        [(">", [(0, 1, maybe "" (<> " | ") transform
            <> "capture (" <> call <> ")")])]
    where
    extract = first (Monad.join . Lists.head) . DeriveTest.extract
        (Env.lookup "capture" . Score.event_environ)
    c_capture :: Derive.Generator Derive.Note
    c_capture = Derive.generator module_ "capture" mempty "Capture env." $
        Sig.call (Sig.required "val" "Val.") $ \val _args ->
            Derive.with_val "capture" (val :: DeriveT.Val) Call.note

c_show_args :: Derive.CallableExpr d => Derive.Generator d
c_show_args = Derive.generator module_ "show-args" mempty "doc" $
    Sig.call (Sig.many_vals "arg" "doc") $ \_ args -> do
        Log.warn $ Text.intercalate ", " $
            map ShowVal.show_val (Derive.passed_vals args)
        return Stream.empty

generator :: Derive.Taggable d =>
    (Derive.PassedArgs d -> Derive.Deriver (Stream.Stream d))
    -> Derive.Generator d
generator = Derive.generator module_ "test" mempty "test doc" . Sig.call0

generator1 :: Derive.Taggable d => (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Generator d
generator1 = Derive.generator1 module_ "test" mempty "test doc" . Sig.call0

transformer :: Derive.Taggable d
    => (Derive.PassedArgs d -> Derive.Deriver (Stream.Stream d)
        -> Derive.Deriver (Stream.Stream d))
    -> Derive.Transformer d
transformer = transformer_args . Sig.call0t

transformer_args :: Derive.WithArgDoc (Derive.TransformerF d)
    -> Derive.Transformer d
transformer_args = Derive.transformer module_ "test" mempty "test doc"

-- * PassedArgs

module_ :: Module.Module
module_ = "test-module"

expr :: Text -> DeriveT.Expr
expr = either (error . untxt . ("CallTest.expr: " <>)) id . Parse.parse_expr

val :: Text -> DeriveT.Val
val = either (error . untxt . ("CallTest.val: " <>)) id . Parse.parse_val
