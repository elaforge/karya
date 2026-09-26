-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate grace notes.  These are short sequences of quick notes
-- whose duration is generally independent of the tempo.
module Derive.C.Prelude.Grace (library) where
import qualified Util.Lists as Lists
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Types


library :: Library.Library
library = mconcat
    [ Library.generators -- Note
        [ ("g", cGrace)
        , ("g-", cGraceHold)
        , ("g_", cGracePitch)
        , ("grace", cBasicGrace)
        , ("roll", cRoll)
        , ("`mordent`", cMordent (Pitch.Diatonic 1))
        , ("`rmordent`", cMordent (Pitch.Diatonic (-1)))
        ]
    , Library.generators -- Pitch
        [ ("g", cGraceP)
        , ("`mordent`", cMordentP (Pitch.Diatonic 1))
        , ("`rmordent`", cMordentP (Pitch.Diatonic (-1)))
        ]
    ]


-- * note calls

cGrace :: Derive.Generator Derive.Note
cGrace = GraceUtil.makeGrace Module.prelude
    "Emit grace notes. The grace notes go through the `(` call, so they will\
    \ overlap or apply a keyswitch, or do whatever `(` does."
    id $ \args events -> Derive.withVal "legato-dyn" (1 :: Double) $
        Sub.reapplyCall (Args.context args) "(" [] [events]

cGraceHold :: Derive.Generator Derive.Note
cGraceHold = GraceUtil.makeGrace Module.prelude
    "Like `g`, but doesn't use `(`, and all notes are held to the duration of\
    \ the event."
    id $ \_args -> Sub.derive . hold
    where
    hold events = maybe events (\e -> map (setEnd e) events) end
        where end = Lists.maximum $ map SubT.end events
    setEnd end event = event { SubT._duration = end - SubT._start event }

cGracePitch :: Derive.Generator Derive.Note
cGracePitch = GraceUtil.makeGracePitch Module.prelude
    "Grace notes realized as one note with a pitch curve, rather than\
    \ separate notes." $
    \_args events -> if null events then mempty else do
        pitch <- Derive.getPitch
        let start = SubT._start (head events)
        rstarts <- mapM (Derive.real . SubT._start) events
        Derive.place start (SubT.end (last events) - start) $
            Derive.withPitch
                (mksig rstarts (map SubT._note events) <> pitch)
                Call.note
    where
    mksig starts pitches = PSignal.fromPairs $ concat
        [ (start, p) : maybe [] ((:[]) . (, p)) mbEnd
        | ((start, mbEnd), p) <- zip (pairs starts) pitches
        ]
    pairs (x1:x2:xs) = (x1, Just x2) : pairs (x2:xs)
    pairs [x] = [(x, Nothing)]
    pairs [] = []

cBasicGrace :: Derive.Generator Derive.Note
cBasicGrace = Derive.generator Module.prelude "basic-grace"
    (Tags.ornament <> Tags.ly)
    "This a grace call where all arguments are required. The idea is that this\
    \ will be used as the implementation of more specific ornaments, perhaps\
    \ defined in a definitions file."
    $ Sig.call ((,,,)
    <$> Sig.requiredEnv "pitches" Sig.None GraceUtil.gracePitchesDoc
    <*> Sig.requiredEnv "dur" Sig.None "Duration of grace notes."
    <*> Sig.requiredEnv "place" Sig.None GraceUtil.gracePlaceDoc
    <*> Sig.defaultedEnv "transformer" Sig.None (Nothing :: Maybe Sig.Dummy)
        "Apply a transformer to grace notes."
    ) $ \(pitches, graceDur, place, maybeTransform) ->
    Sub.inverting $ \args -> do
        start <- Args.realStart args
        base <- Call.getPitch start
        pitches <- GraceUtil.resolvePitches base pitches
        let apply = Eval.evalQuotedTransformers (Args.context args)
        Ly.whenLilypond (GraceUtil.lilyGrace args start pitches) $
            Sub.derive =<< GraceUtil.basicGraceTransform args pitches
                (maybe id apply maybeTransform) graceDur place


-- ** roll

cRoll :: Derive.Generator Derive.Note
cRoll = Derive.generator Module.prelude "roll" Tags.ornament
    "These are like grace notes, but they all have the same pitch.\
    \ The extra notes always fall before the main one, because `trem` covers\
    \ the afterwards case."
    $ Sig.call ((,,)
    <$> Sig.defaulted "times" (1 :: Int) "Number of grace notes."
    <*> Sig.defaulted "time" GraceUtil.defaultGraceDur
        "Time between the strokes."
    <*> Sig.defaulted "dyn" (0.5 :: Double) "Dyn scale for the grace notes."
    ) $ \(times, Typecheck.DefaultReal time, dynScale) ->
    Sub.inverting $ roll (round (times :: Double)) time dynScale

roll :: Int -> DeriveT.Duration -> Signal.Y -> Derive.PassedArgs a
    -> Derive.NoteDeriver
roll times time dynScale args = do
    start <- Args.realStart args
    pitch <- Call.getPitch start
    dyn <- Call.dynamic start
    notes <- Lists.dropEnd 1 <$> GraceUtil.repeatNotes
        (Call.withPitch pitch Call.note) (times+1) time
            (Typecheck.Normalized 0) args
    Sub.derive (map (fmap (Call.withDynamic (dyn*dynScale))) notes)
        <> Call.placedNote args

-- ** mordent

cMordent :: Pitch.Transpose -> Derive.Generator Derive.Note
cMordent defaultNeighbor = Derive.generator Module.prelude "mordent"
    Tags.ornament
    "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,,)
    <$> Sig.defaulted "neighbor" (Typecheck.DefaultDiatonic defaultNeighbor)
        "Neighbor pitch."
    <*> GraceUtil.graceDynEnv <*> GraceUtil.graceEnvs
    ) $ \(Typecheck.DefaultDiatonic neighbor, dyn, (graceDur, place)) ->
    Sub.inverting $ \args ->
        Ly.whenLilypond (lilyMordent args neighbor) $ do
            pitch <- Call.getPitch =<< Args.realStart args
            GraceUtil.legatoGrace args dyn
                [pitch, Pitches.transpose neighbor pitch] graceDur place

lilyMordent :: Derive.PassedArgs d -> Pitch.Transpose -> Derive.NoteDeriver
lilyMordent args neighbor = do
    start <- Args.realStart args
    pitch <- Call.getPitch start
    GraceUtil.lilyGrace args start [pitch, Pitches.transpose neighbor pitch]


-- * pitch calls

cMordentP :: Pitch.Transpose -> Derive.Generator Derive.Pitch
cMordentP defaultNeighbor = Derive.generator1 Module.prelude "mordent"
    Tags.ornament "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> Sig.defaulted "neighbor" (Typecheck.DefaultDiatonic defaultNeighbor)
        "Neighbor pitch."
    <*> GraceUtil.graceDurEnv
    ) $ \(pitch, Typecheck.DefaultDiatonic neighbor, graceDur) args ->
        graceP graceDur [pitch, Pitches.transpose neighbor pitch, pitch]
            (Args.rangeOrNext args)

cGraceP :: Derive.Generator Derive.Pitch
cGraceP = Derive.generator1 Module.prelude "grace" Tags.ornament
    "Generate grace note pitches.  They start on the event and have the given\
    \ duration, but are shortened if the available duration is too short.\
    \ The destination pitch is first, even though it plays last, so\
    \ `g (c) (a) (b)` produces `a b c`."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> GraceUtil.gracePitchesArg <*> GraceUtil.graceDurEnv
    ) $ \(pitch, pitches, graceDur) args -> do
        ps <- (++[pitch]) <$> GraceUtil.resolvePitches pitch pitches
        graceP graceDur ps (Args.rangeOrNext args)

graceP :: DeriveT.Duration -> [PSignal.Pitch]
    -> (ScoreTime, ScoreTime) -> Derive.Deriver PSignal.PSignal
graceP graceDur pitches (start, end) = do
    realDur <- Call.realDuration start graceDur
    realStart <- Derive.real start
    realEnd <- Derive.real end
    let starts = GraceUtil.fitAfter realStart realEnd (length pitches)
            realDur
    pure $ PSignal.fromPairs $ flatSegments $ zip starts pitches

flatSegments :: [(RealTime, y)] -> [(RealTime, y)]
flatSegments = concatMap toPairs . Lists.zipNext
    where
    toPairs ((x, y), next) = case next of
        Nothing -> [(x, y)]
        Just (x2, _) -> [(x, y), (x2, y)]
