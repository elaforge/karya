-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate grace notes.  These are short sequences of quick notes
-- whose duration is generally independent of the tempo.
module Derive.C.Prelude.Grace (library) where
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


library :: Library.Library
library = mconcat
    [ Library.generators -- Note
        [ ("g", c_grace)
        , ("g-", c_grace_hold)
        , ("grace", c_basic_grace)
        , ("roll", c_roll)
        , ("`mordent`", c_mordent (Pitch.Diatonic 1))
        , ("`rmordent`", c_mordent (Pitch.Diatonic (-1)))
        ]
    , Library.generators -- Pitch
        [ ("g", c_grace_p)
        , ("`mordent`", c_mordent_p (Pitch.Diatonic 1))
        , ("`rmordent`", c_mordent_p (Pitch.Diatonic (-1)))
        ]
    ]


-- * note calls

-- | Grace is in the prelude since it's so commonly used.  Mordent and the
-- other variations are still in 'Module.europe'.
c_grace :: Derive.Generator Derive.Note
c_grace = GraceUtil.make_grace Module.prelude
    "Emit grace notes. The grace notes go through the `(` call, so they will\
    \ overlap or apply a keyswitch, or do whatever `(` does."
    id $ \args events -> Derive.with_val "legato-dyn" (1 :: Double) $
        Sub.reapply_call (Args.context args) "(" [] [events]

c_grace_hold :: Derive.Generator Derive.Note
c_grace_hold = GraceUtil.make_grace Module.prelude
    "Like `g`, but doesn't use `(`, and all notes are held to the duration of\
    \ the event."
    id $ \_args -> Sub.derive . hold
    where
    hold events = maybe events (\e -> map (set_end e) events) end
        where end = Seq.maximum $ map Sub.event_end events
    set_end end event =
        event { Sub.event_duration = end - Sub.event_start event }

c_basic_grace :: Derive.Generator Derive.Note
c_basic_grace = Derive.generator Module.prelude "basic-grace"
    (Tags.ornament <> Tags.ly)
    "This a grace call where all arguments are required. The idea is that this\
    \ will be used as the implementation of more specific ornaments, perhaps\
    \ defined in a definitions file."
    $ Sig.call ((,,,)
    <$> Sig.required_env "pitches" Sig.None GraceUtil.grace_pitches_doc
    <*> Sig.required_env "dur" Sig.None "Duration of grace notes."
    <*> Sig.required_env "place" Sig.None GraceUtil.grace_place_doc
    <*> Sig.defaulted_env "transformer" Sig.None Nothing
        "Apply a transformer to grace notes."
    ) $ \(pitches, grace_dur, place, maybe_transform) ->
    Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Call.get_pitch start
        pitches <- GraceUtil.resolve_pitches base pitches
        let apply = Eval.eval_quoted_transformers (Args.context args)
        Ly.when_lilypond (GraceUtil.lily_grace args start pitches) $
            Sub.derive =<< GraceUtil.basic_grace args pitches
                (maybe id apply maybe_transform) grace_dur place


-- ** roll

c_roll :: Derive.Generator Derive.Note
c_roll = Derive.generator Module.europe "roll" Tags.ornament
    "These are like grace notes, but they all have the same pitch.\
    \ The extra notes always fall before the main one, because `trem` covers\
    \ the afterwards case."
    $ Sig.call ((,,)
    <$> Sig.defaulted "times" 1 "Number of grace notes."
    <*> Sig.defaulted "time" GraceUtil.default_grace_dur
        "Time between the strokes."
    <*> Sig.defaulted "dyn" 0.5 "Dyn scale for the grace notes."
    ) $ \(times, Typecheck.DefaultReal time, dyn_scale) ->
    Sub.inverting $ roll times time dyn_scale

roll :: Int -> BaseTypes.Duration -> Signal.Y -> Derive.PassedArgs a
    -> Derive.NoteDeriver
roll times time dyn_scale args = do
    start <- Args.real_start args
    pitch <- Call.get_pitch start
    dyn <- Call.dynamic start
    notes <- Seq.rdrop 1 <$> GraceUtil.repeat_notes
        (Call.with_pitch pitch Call.note) (times+1) time 0 args
    Sub.derive (map (fmap (Call.with_dynamic (dyn*dyn_scale))) notes)
        <> Call.placed_note args

-- ** mordent

c_mordent :: Pitch.Transpose -> Derive.Generator Derive.Note
c_mordent default_neighbor = Derive.generator Module.europe "mordent"
    Tags.ornament
    "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,)
    <$> Sig.defaulted "neighbor" (Typecheck.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> GraceUtil.grace_envs
    ) $ \(Typecheck.DefaultDiatonic neighbor, (grace_dur, dyn, place)) ->
    Sub.inverting $ \args ->
        Ly.when_lilypond (lily_mordent args neighbor) $ do
            pitch <- Call.get_pitch =<< Args.real_start args
            GraceUtil.legato_grace args dyn
                [pitch, Pitches.transpose neighbor pitch] grace_dur place

lily_mordent :: Derive.PassedArgs d -> Pitch.Transpose -> Derive.NoteDeriver
lily_mordent args neighbor = do
    start <- Args.real_start args
    pitch <- Call.get_pitch start
    GraceUtil.lily_grace args start [pitch, Pitches.transpose neighbor pitch]


-- * pitch calls

c_mordent_p :: Pitch.Transpose -> Derive.Generator Derive.Pitch
c_mordent_p default_neighbor = Derive.generator1 Module.europe "mordent"
    Tags.ornament "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> Sig.defaulted "neighbor" (Typecheck.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> GraceUtil.grace_dur_env
    ) $ \(pitch, Typecheck.DefaultDiatonic neighbor, grace_dur) args ->
        grace_p grace_dur [pitch, Pitches.transpose neighbor pitch, pitch]
            (Args.range_or_next args)

c_grace_p :: Derive.Generator Derive.Pitch
c_grace_p = Derive.generator1 Module.prelude "grace" Tags.ornament
    "Generate grace note pitches.  They start on the event and have the given\
    \ duration, but are shortened if the available duration is too short.\
    \ The destination pitch is first, even though it plays last, so\
    \ `g (c) (a) (b)` produces `a b c`."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> GraceUtil.grace_pitches_arg <*> GraceUtil.grace_dur_env
    ) $ \(pitch, pitches, grace_dur) args -> do
        ps <- (++[pitch]) <$> GraceUtil.resolve_pitches pitch pitches
        grace_p grace_dur ps (Args.range_or_next args)

grace_p :: BaseTypes.Duration -> [PSignal.Pitch]
    -> (ScoreTime, ScoreTime) -> Derive.Deriver PSignal.PSignal
grace_p grace_dur pitches (start, end) = do
    real_dur <- Call.real_duration start grace_dur
    real_start <- Derive.real start
    real_end <- Derive.real end
    let starts = GraceUtil.fit_after real_start real_end (length pitches)
            real_dur
    return $ PSignal.from_pairs $ flat_segments $ zip starts pitches

flat_segments :: [(RealTime, y)] -> [(RealTime, y)]
flat_segments = concatMap to_pairs . Seq.zip_next
    where
    to_pairs ((x, y), next) = case next of
        Nothing -> [(x, y)]
        Just (x2, _) -> [(x, y), (x2, y)]
