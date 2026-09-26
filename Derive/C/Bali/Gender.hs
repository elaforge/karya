-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ornaments for gender.  The unique thing about gender technique is the
-- delayed damping, so these calls deal with delayed damping.
module Derive.C.Bali.Gender (
    library, ngoretVariations
    , intervalArg, ngoret, cRealizeNgoret, realizeNgoret
    , weak, weakCall
) where
import qualified Util.Log as Log
import qualified Util.Lists as Lists
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators $
        (("weak", cWeak) : ngoretVariations genderNgoret)
    , Library.transformers
        [ ("realize-ngoret", cRealizeNgoret)
        , ("infer-damp-simple", cInferDampSimple)
        ]
    ]

ngoretVariations :: (Sig.Parser (Maybe Pitch.Transpose) -> call)
    -> [(Expr.Symbol, call)]
ngoretVariations make =
    [ ("'", make $ pure Nothing)
    , ("'n", make $ Just <$> intervalArg)
    , ("'^", make $ pure $ Just $ Pitch.Diatonic (-1))
    , ("'-", make $ pure $ Just $ Pitch.Diatonic 0)
    , ("'_", make $ pure $ Just $ Pitch.Diatonic 1)
    ]

module_ :: Module.Module
module_ = "bali" <> "gender"

-- * ngoret

genderNgoret :: Sig.Parser (Maybe Pitch.Transpose)
    -> Derive.Generator Derive.Note
genderNgoret = ngoret module_ True dampArg
    where
    dampArg = Sig.defaulted "damp" (0.5 :: RealTime)
        "Time that the grace note overlaps with this one. So the total\
        \ duration is time+damp, though it will be clipped to the\
        \ end of the current note."

intervalArg :: Sig.Parser Pitch.Transpose
intervalArg = Typecheck.defaultDiatonic <$> Sig.required "interval"
    "The grace note is this interval from the destination pitch."

-- | Other instruments also have ngoret, but without gender's special damping
-- behaviour.
ngoret :: Module.Module -> Bool
    -- ^ Extend the previous note's duration to the end of the grace note.
    -> Sig.Parser Typecheck.RealTimeFunction
    -- ^ Time grace note overlaps with this one.
    -> Sig.Parser (Maybe Pitch.Transpose)
    -> Derive.Generator Derive.Note
ngoret module_ lateDamping dampArg intervalArg =
    Derive.generator module_ "ngoret"
    (Tags.inst <> Tags.ornament <> Tags.requiresPostproc)
    ("Insert an intermediate grace note in the \"ngoret\" style.\
    \ The grace note moves up for `'^`, down for `'_`, or is based\
    \ on the previous note's pitch for `'`.\
    \\nThis requires the `realize-ngoret` postproc."
    ) $ Sig.call ((,,,,)
    <$> intervalArg
    <*> Sig.defaulted "time" (0.1 :: Double)
        "Time between the grace note start and the main note. If there isn't\
        \ enough room after the previous note, it will be halfway between\
        \ the previous note and this one."
    <*> dampArg
    <*> Sig.defaulted "dyn" (0.75 :: Double)
        "The grace note's dyn will be this multiplier of the current dyn."
    <*> Sig.environ "damp-threshold" Sig.Prefixed  (0.15 :: Double)
        "A grace note with this much time will cause the previous note to be\
        \ shortened to not overlap. Under the threshold, and the damping of\
        \ the previous note will be delayed until the end of the grace note."
    ) $ \(maybeInterval
        , Typecheck.RealTimeFunction time
        , Typecheck.RealTimeFunction damp
        , dynScale
        , dampThreshold
        ) args ->
    Sub.invertingArgs args $ \args -> do
        start <- Args.realStart args
        time <- Derive.real (time start)
        damp <- Derive.real (damp start)
        maybePitch <- case maybeInterval of
            Nothing -> pure Nothing
            Just transpose ->
                Just . Pitches.transpose transpose <$> Call.getPitch start
        dyn <- (*dynScale) <$> Call.dynamic start

        graceStart <- Derive.score (start - time)
        -- If there isn't room for the grace note, use the midpoint between the
        -- prev note and this one.
        graceStart <- pure $ case Args.prevStart args of
            Nothing -> graceStart
            Just prev -> max graceStart $ (prev + Args.start args) / 2
        realGraceStart <- Derive.real graceStart
        let withFlags
                | lateDamping && prevTouches = Call.addFlags $
                    if start - realGraceStart < dampThreshold
                        then extendPrevious else shortenPrevious
                | otherwise = id
            prevTouches = maybe False (>= Args.start args) (Args.prevEnd args)
        overlap <- Call.scoreDuration (Args.start args) damp
        let graceEnd = Args.start args + overlap
            graceNote = case maybePitch of
                Nothing -> Call.addFlags inferPitchFlag Call.note
                Just pitch -> Call.pitchedNote pitch
        Derive.place graceStart (graceEnd - graceStart)
                (withFlags $ Call.withDynamic dyn graceNote)
            <> Derive.place (Args.start args) (Args.duration args) Call.note

-- ** realize

cRealizeNgoret :: Derive.Transformer Derive.Note
cRealizeNgoret = Derive.transformer module_ "realize-ngoret"
    (Tags.inst <> Tags.postproc)
    ("Realize pitches and positions emited by the `ngoret` call.\
    \ This is necessary because it needs to know the positions and pitches\
    \ of the previous and next notes, and those aren't necessarily available\
    \ when evaluating the track. This call needs a "
    <> ShowVal.doc EnvKey.hand <> " envron to figure out which which note\
    \ follows which."
    ) $ Sig.call0t $ \_ deriver -> realizeNgoret =<< deriver

realizeNgoret :: Stream.Stream Score.Event -> Derive.NoteDeriver
realizeNgoret =
    Post.applyM $ fmap merge . mapM realize . Lists.groupSort Post.handKey
    where
    -- TODO do I want to ignore streams with irrelevant instruments?
    realize = fmap (map (uncurry realizeDamped) . Lists.zipNext)
        . apply realizeInferPitch
    apply f = mapMaybeM (apply1 f) . Lists.zipNeighbors
        where
        apply1 f (prev, event, next) = case f prev event next of
            Right event -> pure $ Just event
            Left err -> do
                Derive.withEventStack event $ Log.warn err
                pure Nothing
    merge = Lists.mergeLists Score.eventStart

realizeInferPitch :: Maybe Score.Event -> Score.Event
    -> Maybe Score.Event -> Either Text Score.Event
realizeInferPitch maybePrev event maybeNext
    | Score.hasFlags inferPitchFlag event = do
        prev <- require "no previous event" maybePrev
        next <- require "no next event" maybeNext
        pitch <- first ("can't infer pitch: "<>) $ inferPitch prev next
        -- Also make sure the grace note doesn't go past the end of the next
        -- note.
        let dur = min (Score.eventDuration event)
                (Score.eventEnd next - Score.eventStart event)
        pure $ Score.removeFlags inferPitchFlag $ Score.setDuration dur $
            Score.setPitch (PSignal.constant pitch) event
    | otherwise = pure event
    where require err = maybe (Left err) pure

realizeDamped :: Score.Event -> Maybe Score.Event -> Score.Event
realizeDamped event maybeNext =
    Score.removeFlags (extendPrevious <> shortenPrevious) $
        maybe id setDur maybeNext event
    where
    setDur next
        | Score.hasFlags extendPrevious next =
            Score.setDuration (Score.eventEnd next - start)
        | Score.hasFlags shortenPrevious next =
            Score.setDuration (Score.eventStart next - start)
        | otherwise = id
        where start = Score.eventStart event

inferPitch :: Score.Event -> Score.Event -> Either Text PSignal.Pitch
inferPitch prev next = do
    prevNn <- tryJust ("no prev nn: " <> Score.shortEvent prev) $
        Score.initialNn prev
    nextNn <- tryJust ("no next nn: " <> Score.shortEvent next) $
        Score.initialNn next
    let steps
            | prevNn == nextNn = 0
            | prevNn < nextNn = -1
            | otherwise = 1
    Pitches.transposeD steps <$>
        tryJust ("no pitch at " <> pretty (Score.eventStart next))
            (Score.pitchAt (Score.eventStart next) next)

-- | Mark events whose should have their pitch inferred from the previous and
-- next events.
inferPitchFlag :: Flags.Flags
inferPitchFlag = Flags.flag "infer-pitch"

-- | Mark grace notes that were damped late, and whose previous event should be
-- extended to be damped together.
extendPrevious :: Flags.Flags
extendPrevious = Flags.flag "extend-previous-duration"

-- | Mark grace notes that don't cause a late damp.  The previous event's
-- duration should be shortened to end where the grace note begins.
shortenPrevious :: Flags.Flags
shortenPrevious = Flags.flag "shorten-previous-duration"

-- * weak

cWeak :: Derive.Generator Derive.Note
cWeak = Derive.generator module_ "weak" Tags.inst
    "Weak notes are filler notes."
    $ Sig.call (
    Sig.defaultedEnv "strength" Derive.Unprefixed (0.5 :: Double)
        "From low strength to high, omit the note, then play it muted, and\
        \ then play it open but softly."
    ) $ \strength -> Sub.inverting (weak strength)

weak :: Signal.Y -> Derive.PassedArgs a -> Derive.NoteDeriver
weak strength args = do
    -- This biases mute values to be lower, and 0 before it unmutes.
    let mute = max 0 $ 1 - (strength + (1 - unmuteThreshold))
    if strength <= omitThreshold then mempty
        else Call.withConstant Controls.mute mute $ Call.placedNote args
    where
    omitThreshold = 0.25
    unmuteThreshold = 0.75

weakCall :: Derive.PassedArgs a -> Derive.NoteDeriver
weakCall args = do
    -- TODO This is nonstandard, because usually signals are resolved
    -- implicitly as arguments.  But DUtil.zero_duration doesn't really plug
    -- into Sig.call.  Maybe it should reapply `weak` instead of calling it
    -- directly?  In any case, I think the result should be the same.
    strength <- fmap (maybe 0.5 ScoreT.valOf) $
        Derive.controlAt "strength" =<< Args.realStart args
    weak strength (Args.setDuration dur args)
    where dur = Args.next args - Args.start args

-- * im

-- ** infer damp

{- |
    Simple version:
    - Any note immediately followed by the same pitch gets its duration
    extended to the end of the last note with the same pitch.

    Fancy version:
    - All notes ring until explicitly damped.
    - A gap between notes in the same hand adds a damp.  A pitch followed by a
    different one in the same hand adds a damp to the first.
    - The hand requires time to actually do the damp.  Adjacent pitches by 1 or
    2 can be simultaneous.  Otherwise, you need a certain amount of time when
    that hand is not busy damping.
-}
cInferDampSimple :: Derive.Transformer Derive.Note
cInferDampSimple =
    Derive.transformer (module_ <> "im") "infer-damp-simple" Tags.postproc
    ("Simple gender damping. Duration is extended if the next note on the same\
    \ hand has the same pitch and the gap is < " <> ShowVal.doc gap <> ".")
    $ Sig.callt (Sig.required "insts" "Apply damping to these instruments.")
    $ \insts _args ->
        fmap $ Post.emap1_ (infer insts) . Post.nextsBy Post.handKey
    where
    infer insts =
        Post.only fst (Post.hasInstrument insts) $ inferDampSimple gap
    -- Less than this much time before the next note of the same pitch means
    -- extend the duration.
    gap = 0.15

inferDampSimple :: RealTime -> (Score.Event, [Score.Event]) -> Score.Event
inferDampSimple gap (event, nexts)
    | Score.eventDuration event /= 0 && newEnd > Score.eventEnd event =
        Score.setDuration (newEnd - Score.eventStart event) event
    | otherwise = event
    where
    newEnd = go event nexts
    go prev (next:nexts)
        | Score.eventStart next - Score.eventEnd prev <= gap
                && Score.initialNote prev == Score.initialNote next
            = go next nexts
    go prev _ = Score.eventEnd prev
