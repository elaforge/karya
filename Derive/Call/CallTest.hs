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
    Derive.withConstantPitch
        (DeriveTest.mkpitch12 "4c") (trans (DeriveTest.cNote 0 1))

runPitch_ :: Text -> [(ScoreTime, Text)] -> Derive.Result
runPitch_ title events = DeriveTest.deriveTracks title
    [ (">", [(0, 10, "")])
    , ("*", [(start, 0, text) | (start, text) <- events])
    ]

runPitch :: Text -> [(ScoreTime, Text)] -> [(RealTime, Pitch.NoteNumber)]
runPitch title = extract . runPitch_ title
    where extract = head . DeriveTest.extractEvents DeriveTest.eNns

-- | Run a control track and extract the control signal it produces.
runControl :: [(ScoreTime, Text)] -> [(Signal.X, Signal.Y)]
runControl events = runControlDur [(p, 0, t) | (p, t) <- events]

runControlDur :: [UiTest.EventSpec] -> [(Signal.X, Signal.Y)]
runControlDur events = extract $
    DeriveTest.deriveTracks "" [(">", [(0, 10, "")]), ("cont", events)]
    where
    extract = Lists.dropDups id . head . DeriveTest.extractEvents
        (Signal.toPairs . ScoreT.valOf . get . Score.eventControls)
    get fm = case Map.lookup "cont" fm of
        Nothing -> error "expected a 'cont' control"
        Just c -> c

-- * call map

-- | Example:
--
-- > DeriveTest.derive_tracks_setup (CallTest.with_note_generator "g" c_gen)
-- > where c_gen = CallTest.generator $ \args -> do ...
withNoteGenerator :: Expr.Symbol -> Derive.Generator Derive.Note
    -> DeriveTest.SetupA a
withNoteGenerator name call = DeriveTest.withDeriver $ Derive.withScopes $
    Derive.sGenerator#Derive.sNote %= override (Derive.singleCall name call)

withPitchGenerator :: Expr.Symbol -> Derive.Generator Derive.Pitch
    -> DeriveTest.SetupA a
withPitchGenerator name call = DeriveTest.withDeriver $ Derive.withScopes $
    Derive.sGenerator#Derive.sPitch %= override (Derive.singleCall name call)

withControlGenerator :: Expr.Symbol -> Derive.Generator Derive.Control
    -> DeriveTest.SetupA a
withControlGenerator name call =
    DeriveTest.withDeriver $ Derive.withScopes $
        Derive.sGenerator#Derive.sControl
            %= override (Derive.singleCall name call)

withNoteGenerators :: [(Expr.Symbol, Derive.Generator Derive.Note)]
    -> DeriveTest.SetupA a
withNoteGenerators calls = DeriveTest.withDeriver $ Derive.withScopes $
    Derive.sGenerator#Derive.sNote %= override (lookupMap calls)

withNoteTransformer :: Expr.Symbol -> Derive.Transformer Derive.Note
    -> DeriveTest.SetupA a
withNoteTransformer name call = DeriveTest.withDeriver $ Derive.withScopes $
    Derive.sTransformer#Derive.sNote
        %= override (Derive.singleCall name call)

withValCall :: Expr.Symbol -> Derive.ValCall -> DeriveTest.SetupA a
withValCall name call = DeriveTest.withDeriver $ Derive.withScopes $
    Derive.sVal %= override (Derive.singleCall name call)

override :: Derive.CallMap call -> Derive.ScopePriority call
    -> Derive.ScopePriority call
override = Derive.addPriority Derive.PrioOverride

lookupMap :: [(Expr.Symbol, call)] -> Derive.CallMap call
lookupMap calls = Derive.CallMap
    { callMap = Map.fromList calls
    , callPatterns = []
    }

-- * calls

-- | Run a val call, and return what it returned.
runVal :: Maybe Text -> Text -> (Maybe DeriveT.Val, [Text])
runVal = runValTracks "" [] 0

-- | 'run_val' with more knobs.
runValTracks :: Text -> [UiTest.TrackSpec] -> ScoreTime -> Maybe Text -> Text
    -> (Maybe DeriveT.Val, [Text])
runValTracks title tracks at transform call =
    extract $ DeriveTest.deriveTracksSetup
        (withNoteGenerator "capture" cCapture) title
        (tracks ++ [(">", [(at, 0, event)])])
    where
    event = maybe "" (<> " | ") transform <> "capture (" <> call <> ")"
    extract = first (Monad.join . Lists.head) . DeriveTest.extract
        (Env.lookup "capture" . Score.eventEnviron)
    cCapture :: Derive.Generator Derive.Note
    cCapture = Derive.generator module_ "capture" mempty "Capture env." $
        Sig.call (Sig.required "val" "Val.") $ \val _args ->
            Derive.withVal "capture" (val :: DeriveT.Val) Call.note

cShowArgs :: Derive.CallableExpr d => Derive.Generator d
cShowArgs = Derive.generator module_ "show-args" mempty "doc" $
    Sig.call (Sig.manyVals "arg" "doc") $ \_ args -> do
        Log.warn $ Text.intercalate ", " $
            map ShowVal.showVal (Derive.passedVals args)
        pure Stream.empty

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
transformer = transformerArgs . Sig.call0t

transformerArgs :: Derive.WithArgDoc (Derive.TransformerF d)
    -> Derive.Transformer d
transformerArgs = Derive.transformer module_ "test" mempty "test doc"

-- * PassedArgs

module_ :: Module.Module
module_ = "test-module"

expr :: Text -> DeriveT.Expr
expr = either (error . untxt . ("CallTest.expr: " <>)) id . Parse.parseExpr

val :: Text -> DeriveT.Val
val = either (error . untxt . ("CallTest.val: " <>)) id . Parse.parseVal
