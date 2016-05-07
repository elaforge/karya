-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for testing calls.
module Derive.Call.CallTest where
import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.BaseTypes as BaseTypes

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


transform :: (Derive.NoteDeriver -> Derive.NoteDeriver) -> Derive.Result
transform trans = DeriveTest.derive State.empty $
    Derive.with_constant_pitch
        (DeriveTest.mkpitch12 "4c") (trans (DeriveTest.c_note 0 1))

run_pitch_ :: String -> [(ScoreTime, String)] -> Derive.Result
run_pitch_ title events = DeriveTest.derive_tracks title
    [ (">", [(0, 10, "")])
    , ("*", [(start, 0, text) | (start, text) <- events])
    ]

run_pitch :: String -> [(ScoreTime, String)] -> [(RealTime, Pitch.NoteNumber)]
run_pitch title = extract . run_pitch_ title
    where extract = head . DeriveTest.extract_events DeriveTest.e_nns

-- | Run a control track and extract the control signal it produces.
run_control :: [(ScoreTime, String)] -> [(Signal.X, Signal.Y)]
run_control events = run_control_dur [(p, 0, t) | (p, t) <- events]

run_control_dur :: [UiTest.EventSpec] -> [(Signal.X, Signal.Y)]
run_control_dur events = extract $
    DeriveTest.derive_tracks "" [(">", [(0, 10, "")]), ("cont", events)]
    where
    -- Slicing implementation details can make dups, but they don't matter for
    -- performance.
    extract = Seq.drop_dups snd . head . DeriveTest.extract_events
        (Signal.unsignal . Score.typed_val . get
            . Score.event_transformed_controls)
    get fm = case Map.lookup "cont" fm of
        Nothing -> errorStack "expected a 'cont' control"
        Just c -> c

-- * call map

with_note_generator :: BaseTypes.CallId -> Derive.Generator Derive.Note
    -> DeriveTest.SetupA a
with_note_generator name call = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_generator#Derive.s_note#Derive.s_override
        %= (single_lookup name call :)

with_pitch_generator :: BaseTypes.CallId -> Derive.Generator Derive.Pitch
    -> DeriveTest.SetupA a
with_pitch_generator name call = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_generator#Derive.s_pitch#Derive.s_override
        %= (single_lookup name call :)

with_control_generator :: BaseTypes.CallId -> Derive.Generator Derive.Control
    -> DeriveTest.SetupA a
with_control_generator name call =
    DeriveTest.with_deriver $ Derive.with_scopes $
        Derive.s_generator#Derive.s_control#Derive.s_override
            %= (single_lookup name call :)

with_note_generators :: [(BaseTypes.CallId, Derive.Generator Derive.Note)]
    -> DeriveTest.SetupA a
with_note_generators calls = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_generator#Derive.s_note#Derive.s_override %= (lookup_map calls :)

with_note_transformer :: BaseTypes.CallId -> Derive.Transformer Derive.Note
    -> DeriveTest.SetupA a
with_note_transformer name call = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_transformer#Derive.s_note#Derive.s_override
        %= (single_lookup name call :)

with_val_call :: BaseTypes.CallId -> Derive.ValCall -> DeriveTest.SetupA a
with_val_call name call = DeriveTest.with_deriver $ Derive.with_scopes $
    Derive.s_val#Derive.s_override %= (single_lookup name call :)

single_lookup :: BaseTypes.CallId -> call -> Derive.LookupCall call
single_lookup name = Derive.LookupMap . Map.singleton name

lookup_map :: [(BaseTypes.CallId, call)] -> Derive.LookupCall call
lookup_map = Derive.LookupMap . Map.fromList

-- * calls

-- | Run a val call, and return what it returned.
run_val :: Maybe String -> String -> (Maybe BaseTypes.Val, [String])
run_val transform call = extract $ DeriveTest.derive_tracks_setup
        (with_note_generator "capture" c_capture) ""
        [(">", [(0, 1, maybe "" (<> " | ") transform
            <> "capture (" <> call <> ")")])]
    where
    extract = first (Monad.join . Seq.head) . DeriveTest.extract
        (Env.lookup "capture" . Score.event_environ)
    c_capture :: Derive.Generator Derive.Note
    c_capture = Derive.generator module_ "capture" mempty "Capture env." $
        Sig.call (Sig.required "val" "Val.") $ \val _args ->
            Derive.with_val "capture" (val :: BaseTypes.Val) Call.note

c_show_args :: Derive.Callable d => Derive.Generator d
c_show_args = Derive.generator module_ "show-args" mempty "doc" $
    Sig.parsed_manually "doc" $ \args -> do
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


-- * PassedArgs

module_ :: Module.Module
module_ = "test-module"

expr :: Text -> BaseTypes.Expr
expr =
    either (errorStack . ("CallTest.expr: " ++) . untxt) id . Parse.parse_expr

val :: Text -> BaseTypes.Val
val = either (errorStack . ("CallTest.val: " ++) . untxt) id . Parse.parse_val
