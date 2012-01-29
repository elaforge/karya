module Derive.Call.CallTest where
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

import qualified Ui.State as State
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


transform :: (Derive.EventDeriver -> Derive.EventDeriver) -> Derive.Result
transform trans = DeriveTest.derive State.empty $
    Derive.with_constant_pitch Nothing DeriveTest.default_scale
        (DeriveTest.mkpitch "a") (trans (DeriveTest.c_note 0 1))

run_pitch :: [(Double, String)] -> [(RealTime, Pitch.NoteNumber)]
run_pitch = run_with_scale "twelve"

run_with_scale :: String -> [(Double, String)]
    -> [(RealTime, Pitch.NoteNumber)]
run_with_scale scale events = extract $ DeriveTest.derive_tracks
    [ (">", [(0, 10, "")])
    , ('*' : scale, [(start, 0, text) | (start, text) <- events])
    ]
    where extract = head . DeriveTest.extract_events DeriveTest.e_pitch

-- | Run a control track and extract the control signal it produces.
run_control :: [(Double, String)] -> [(Signal.X, Signal.Y)]
run_control events = extract $ DeriveTest.derive_tracks_tempo
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

with_note_call :: String -> Derive.NoteCall
    -> Derive.Deriver a -> Derive.Deriver a
with_note_call name call = Derive.with_scope $ \scope -> scope
    { Derive.scope_note =
        add_builtin (single_lookup name call) (Derive.scope_note scope) }

with_control_call :: String -> Derive.ControlCall
    -> Derive.Deriver a -> Derive.Deriver a
with_control_call name call = Derive.with_scope $ \scope -> scope
    { Derive.scope_control =
        add_builtin (single_lookup name call) (Derive.scope_control scope) }

with_val_call :: String -> Derive.ValCall
    -> Derive.Deriver a -> Derive.Deriver a
with_val_call name call = Derive.with_scope $ \scope -> scope
    { Derive.scope_val =
        add_builtin (single_lookup name call) (Derive.scope_val scope) }

add_builtin :: Derive.LookupCall call -> Derive.ScopeType call
    -> Derive.ScopeType call
add_builtin lookup stype =
    stype { Derive.stype_builtin = lookup : Derive.stype_builtin stype }

single_lookup :: String -> call -> Derive.LookupCall call
single_lookup name call call_id
    | TrackLang.Symbol name == call_id = return $ Just call
    | otherwise = return Nothing


-- * PassedArgs

expr :: String -> TrackLang.Expr
expr = either (error . ("CallTest.expr: " ++)) id . ParseBs.parse_expr . B.pack

val :: String -> TrackLang.Val
val = either (error . ("CallTest.val: " ++)) id . ParseBs.parse_val
