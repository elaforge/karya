module Derive.Call.CallTest where
import qualified Data.Map as Map

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal

import qualified Derive.Score as Score


transform :: Derive.Transformer -> Derive.Result Derive.Events
transform deriver = DeriveTest.derive_note $
    Derive.with_constant_pitch Nothing (Pitch.Degree 60) $
    deriver DeriveTest.d_note

run_pitch :: [(Double, String)]
    -> Either String [(PitchSignal.X, PitchSignal.Y)]
run_pitch = run_with_scale "twelve"

run_with_scale :: String -> [(Double, String)]
    -> Either String [(PitchSignal.X, PitchSignal.Y)]
run_with_scale scale events = extract $ DeriveTest.derive_tracks_tempo
    [ (">", [(0, 10, "")])
    , ('*' : scale, [(start, 0, text) | (start, text) <- events])
    ]
    where
    extract = fmap head . DeriveTest.extract_events_only
        (PitchSignal.unsignal . Score.event_pitch)

run_control :: [(Double, String)] -> Either String [(Signal.X, Signal.Y)]
run_control events = extract $ DeriveTest.derive_tracks_tempo
    [ (">", [(0, 10, "")])
    , ("cont", [(start, 0, text) | (start, text) <- events])
    ]
    where
    extract = fmap head . DeriveTest.extract_events_only
        (Signal.unsignal . get . Score.event_controls)
    get fm = case Map.lookup (Score.Control "cont") fm of
        Nothing -> error "expected a 'cont' control"
        Just c -> c

-- * call map

with_note_call :: String -> Derive.NoteCall
    -> Derive.Deriver a -> Derive.Deriver a
with_note_call name call =
    Derive.with_scopes [Derive.NoteScope (single_lookup name call)]

with_val_call :: String -> Derive.ValCall
    -> Derive.Deriver a -> Derive.Deriver a
with_val_call name call =
    Derive.with_scopes [Derive.ValScope (single_lookup name call)]

single_lookup name call call_id
    | TrackLang.Symbol name == call_id = return $ Just call
    | otherwise = return Nothing
