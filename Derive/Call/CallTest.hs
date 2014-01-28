-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.CallTest where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


transform :: (Derive.NoteDeriver -> Derive.NoteDeriver) -> Derive.Result
transform trans = DeriveTest.derive State.empty $
    Derive.with_constant_pitch Nothing
        (DeriveTest.mkpitch12 "4c") (trans (DeriveTest.c_note 0 1))

run_pitch_ :: [(ScoreTime, String)] -> Derive.Result
run_pitch_ events = DeriveTest.derive_tracks
    [ (">", [(0, 10, "")])
    , ("*", [(start, 0, text) | (start, text) <- events])
    ]

run_pitch :: [(ScoreTime, String)] -> [(RealTime, Pitch.NoteNumber)]
run_pitch = extract . run_pitch_
    where extract = head . DeriveTest.extract_events DeriveTest.e_nns

-- | Run a control track and extract the control signal it produces.
run_control :: [(ScoreTime, String)] -> [(Signal.X, Signal.Y)]
run_control events = extract $ DeriveTest.derive_tracks
    [ (">", [(0, 10, "")])
    , ("cont", [(start, 0, text) | (start, text) <- events])
    ]
    where
    -- Slicing implementation details can make dups, but they don't matter for
    -- performance.
    extract = Seq.drop_dups snd . head . DeriveTest.extract_events
        (Signal.unsignal . Score.typed_val . get . Score.event_controls)
    get fm = case Map.lookup "cont" fm of
        Nothing -> error "expected a 'cont' control"
        Just c -> c

-- * call map

with_note_generator :: TrackLang.CallId -> Derive.Generator Derive.Note
    -> Derive.Deriver a -> Derive.Deriver a
with_note_generator name call = Derive.with_scopes $
    Derive.s_generator#Derive.s_note#Derive.s_override
        %= (single_lookup name call :)

with_note_generators :: [(TrackLang.CallId, Derive.Generator Derive.Note)]
    -> Derive.Deriver a -> Derive.Deriver a
with_note_generators calls = Derive.with_scopes $
    Derive.s_generator#Derive.s_note#Derive.s_override %= (map_lookup calls :)

with_note_transformer :: TrackLang.CallId -> Derive.Transformer Derive.Note
    -> Derive.Deriver a -> Derive.Deriver a
with_note_transformer name call = Derive.with_scopes $
    Derive.s_transformer#Derive.s_note#Derive.s_override
        %= (single_lookup name call :)

with_val_call :: TrackLang.CallId -> Derive.ValCall
    -> Derive.Deriver a -> Derive.Deriver a
with_val_call name call = Derive.with_scopes $
    Derive.s_val#Derive.s_override %= (single_val_lookup name call :)

single_lookup :: TrackLang.CallId -> Derive.Call d
    -> Derive.LookupCall (Derive.Call d)
single_lookup name call = Derive.map_lookup (Map.singleton name call)

map_lookup :: [(TrackLang.CallId, Derive.Call d)]
    -> Derive.LookupCall (Derive.Call d)
map_lookup = Derive.map_lookup . Map.fromList

single_val_lookup :: TrackLang.CallId -> Derive.ValCall
    -> Derive.LookupCall Derive.ValCall
single_val_lookup name call =
    Derive.map_val_lookup (Map.singleton name call)

-- * calls

-- | Run a val call, and return what it returned.
run_val :: Maybe String -> String -> (Maybe TrackLang.Val, [String])
run_val transform call = extract $ DeriveTest.derive_tracks_with
        (with_note_generator "capture" c_capture)
        [(">", [(0, 1, maybe "" (<> " | ") transform
            <> "capture (" <> call <> ")")])]
    where
    extract = first (join . Seq.head) . DeriveTest.extract
        (TrackLang.lookup_val "capture" . Score.event_environ)
    c_capture :: Derive.Generator Derive.Note
    c_capture = Derive.make_call "capture" mempty "Capture env." $
        Sig.call (Sig.required "val" "Val.") $ \val _args ->
            Derive.with_val "capture" (val :: TrackLang.Val) Util.note

c_show_args :: (Derive.Callable d) => Derive.Generator d
c_show_args = Derive.generator "show-args" mempty "doc" $
    Sig.parsed_manually "doc" $ \args -> do
        Log.warn $ Seq.join ", " $
            map (untxt . ShowVal.show_val) (Derive.passed_vals args)
        return []

generator :: Derive.ToTagged y =>
    Sig.Generator y d -> Derive.Call (Sig.Generator y d)
generator = Derive.make_call "test" mempty "test doc" . Sig.call0

-- * PassedArgs

expr :: Text -> TrackLang.Expr
expr = either (error . ("CallTest.expr: " ++)) id . ParseBs.parse_expr
    . ParseBs.from_text

val :: Text -> TrackLang.Val
val = either (error . ("CallTest.val: " ++)) id . ParseBs.parse_val
