-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Playing idioms of plucked or bowed strings.  Generally these calls expect
-- 'EnvKey.open_strings', and possibly 'EnvKey.string'.
module Derive.C.Idiom.String where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Post as Post
import qualified Derive.Call.StringUtil as StringUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [ ("gliss-a", cGlissAbsolute)
        , ("gliss", cGliss)
        , ("on", cNthHarmonic)
        , ("o", cHarmonic)
        ]
    , Library.transformers
        [ ("bent-string", cBentString)
        , ("stopped-string", cStoppedString)
        , ("mute-end", cMuteEnd)
        ]
    ]

module_ :: Module.Module
module_ = "idiom" <> "string"

cBentString :: Derive.Transformer Derive.Note
cBentString = Derive.transformer module_ "bent-string"
    (Tags.postproc <> Tags.inst)
    ("Post-process events to play in a stopped string idiom, where\
    \ strings must be bent or stopped to reach non-open pitches.\
    \ Originally it was meant to play in the style of a 古箏 or\
    \ other zither, but may also be appropriate for stopped strings\
    \ like the violin family. As a side-effect, events get a"
    <> ShowVal.doc EnvKey.string <> " variable.\
    \\nFurther documentation is in 'Derive.Call.Idiom.String'.")
    $ Sig.callt ((,,,)
    <$> Sig.defaulted "attack" (0.15 :: Double)
        "Time for a string to bend to its desired pitch. A fast attack\
        \ sounds like a stopped string."
    <*> Sig.defaulted "release" (0.1 :: Double)
        "Time for a string to return to its original pitch."
    <*> Sig.defaulted "delay" (0 :: Double)
        "If the string won't be used for the following note, it will be\
        \ released after this delay."
    <*> StringUtil.openStringsEnv
    ) $ \(attack, release, releaseDelay, openStrings) _args deriver ->
    Ly.whenLilypond deriver $ do
        -- TODO I used to use just the pitch, but the index is more useful when
        -- the pitch can change.  But I have to use pitch for harmonics below
        -- because they don't always require open_strings.
        openStrings <- StringUtil.indexedStrings openStrings
        config <- makeConfig attack releaseDelay release openStrings
        stringIdiom config =<< deriver

cStoppedString :: Derive.Transformer Derive.Note
cStoppedString = Derive.transformer module_ "stopped-string"
    (Tags.postproc <> Tags.inst)
    "A specialization of `bent-string` but for stopped strings, like the\
    \ violin family, where strings instantly jump to their pitches."
    $ Sig.callt ((,,)
    <$> Sig.defaulted "delay" (0 :: RealTime) "String release delay time."
    <*> StringUtil.openStringsEnv <*> StringUtil.stringEnv
    ) $ \(releaseDelay, openStrings, string) _args deriver -> case string of
        Just _ -> deriver -- presumably they will all get a constant string
        Nothing -> do
            openStrings <- StringUtil.indexedStrings openStrings
            config <- makeConfig
                (ScoreT.untyped (const 0)) releaseDelay
                (ScoreT.untyped (const 0)) openStrings
            stringIdiom config =<< deriver

data Config = Config
    { _openStrings :: Map Pitch.NoteNumber StringUtil.String
    -- | (curve, time)
    , _attackCurve :: (PitchUtil.Interpolate, ScoreT.TypedFunction)
    , _releaseCurve :: (PitchUtil.Interpolate, ScoreT.TypedFunction)
    , _releaseDelay :: ScoreT.TypedFunction
    }

makeConfig :: ScoreT.TypedFunction -> ScoreT.TypedFunction
    -> ScoreT.TypedFunction -> [StringUtil.String] -> Derive.Deriver Config
makeConfig attackDur releaseDelay releaseDur openStrings = do
    srate <- Call.getSrate
    let linear = PitchUtil.segment srate ControlUtil.Linear
    pure $ Config
        { _openStrings =
            Map.fromList $ Lists.keyOn StringUtil.strNn openStrings
        , _attackCurve = (linear, attackDur)
        , _releaseCurve = (linear, releaseDur)
        , _releaseDelay = releaseDelay
        }

{- | Post-process events to play them in a string-like idiom.

    This tweaks the ends of the pitch signals of notes.  When a new note is
    played, the next event is examined to determine if it will share a string
    or not.

    If the string must be used for the following note, the end of the event is
    bent up to the next pitch before the next event is triggered.  This is
    called the \"attack\".  A fast attack gives the sound of a stopped string,
    a slow one sounds like a bent one.

    If the string won't be used for a long enough time, it will be released
    after a delay.  The release time determines how long it will take to reach
    its open pitch.  Since the release happens after the note ends, only
    instruments with a bit of decay will have an audible release.

    This does't do anything fancy like simulate hand position or alternate
    fingerings.  It just selects the lowest string below or at the lowest pitch
    in the note.
-}
stringIdiom :: Config -> Stream.Stream Score.Event -> Derive.NoteDeriver
stringIdiom config = do
    Post.emap1m_ (fst . fst) event1 . Post.nextBy (StringUtil.strNn . snd)
        <=< Post.emap1m_ id assign
    where
    assign event = do
        let (nns, _warns) = Score.nnSignal event
        when (Signal.null nns) $ Derive.throw "no pitch"
        let lowest = Pitch.nn $ Signal.minimum nns
        string <- Derive.require ("below lowest string: " <> pretty lowest) $
            snd <$> Map.lookupLE lowest (_openStrings config)
        pure
            ( Score.modifyEnviron (StringUtil.insertString string) event
            , string
            )
    event1 ((event, string), next) = process config event string (fst <$> next)

-- | The next note has the same string, so bend this one to prepare.
-- If it's over a certain threshold away, release this one.
-- TODO maybe skip release and attack if the next note is over a certain
-- threshold away?  If its > the decay time, pitch curves would be inaudible...
-- unless of course changing the pitch itself causes a sound, as it might for
-- a fret slide.
process :: Config -> Score.Event -> StringUtil.String
    -> Maybe Score.Event -> Derive.Deriver Score.Event
process config event string Nothing =
    addRelease config (StringUtil.strPitch string) Nothing event
process config event string (Just nextEvent) = do
    let next = Score.eventStart nextEvent
    attackDur <- Call.realDurationAt (snd (_attackCurve config)) next
    event <- addRelease config (StringUtil.strPitch string)
        (Just (next - attackDur)) event
    pure $ addAttack config attackDur nextEvent event

-- | After releasing a note, you release your hand, which means the pitch
-- should bend down to the open string.
addRelease :: Config -> PSignal.Pitch -> Maybe RealTime -> Score.Event
    -> Derive.Deriver Score.Event
addRelease config openString maybeNext event = do
    let end = Score.eventEnd event
    delay <- Call.realDurationAt (_releaseDelay config) end
    dur <- Call.realDurationAt (snd (_releaseCurve config)) end
    -- Release if there's enough time for both delay, release curve, and
    -- following attack curve.
    let enoughTime = case maybeNext of
            Nothing -> True
            Just next -> next - end > delay + dur
    pure $ fromMaybe event $ if not enoughTime
        then Nothing else do
            fromPitch <- Score.pitchAt (end+delay) event
            pure $ mergeCurve (fst (_releaseCurve config))
                (end + delay) fromPitch
                (end + delay + dur) openString
                event

-- | Bend the event up to the next note.
--
-- If there isn't enough time, do the bend faster.
-- TODO It might be nice to make the transition spill over into the next
-- attack.
addAttack :: Config -> RealTime -> Score.Event -> Score.Event -> Score.Event
addAttack config dur nextEvent event
    | dur <= 0 = event
    | otherwise = fromMaybe event $ do
        let startX = max (Score.eventStart event) (next - dur)
        startPitch <- Score.pitchAt startX event
        nextPitch <- Score.pitchAt next nextEvent
        pure $ mergeCurve (fst (_attackCurve config)) startX startPitch
            next nextPitch event
    where next = Score.eventStart nextEvent

mergeCurve :: PitchUtil.Interpolate -> RealTime -> PSignal.Pitch
    -> RealTime -> PSignal.Pitch -> Score.Event -> Score.Event
mergeCurve interpolate x0 y0 x1 y1 event =
    Score.setPitch (Score.eventPitch event <> curve) event
    where curve = interpolate x0 y0 x1 y1


-- * mute end

cMuteEnd :: Derive.Transformer Derive.Note
cMuteEnd = Derive.transformer module_ "mute-end"
    (Tags.postproc <> Tags.inst)
    ("Put a note and then +mute note at the end of each note, unless there's\
    \ another note on the same string. The " <> ShowVal.doc ring
    <> " attr suppresses this.")
    $ Sig.callt ((,,)
    <$> Sig.required "threshold" "Mute if the string is free for this long."
    <*> Sig.required "dur" "Duration of mute note. If it's zero, its omitted\
        \ and only the +mute note is emitted."
    <*> Sig.required "dyn" "Dyn of mute note."
    ) $ \(threshold, dur, dyn) _args deriver -> Ly.whenLilypond deriver $
        Post.emap_ (muteEnd dur dyn threshold)
            . Post.nextsBy stringOf <$> deriver
    where
    stringOf :: Score.Event -> Maybe Text
    stringOf = Env.maybeVal EnvKey.string . Score.eventEnviron

muteEnd :: (RealTime -> RealTime) -> ScoreT.Function -> RealTime
    -> (Score.Event, [Score.Event]) -> [Score.Event]
muteEnd durAt dynAt threshold (event, nexts)
    | shouldMute = event
        : [muteNote end dur (dynAt end) event | dur > 0]
        ++ [setMute (end + dur) event]
    | otherwise = [event]
    where
    dur = durAt end
    end = Score.eventEnd event
    shouldMute = not (Score.hasAttribute ring event) && case nexts of
        [] -> True
        next : _ -> Score.eventStart next - end > threshold

ring :: Attrs.Attributes
ring = Attrs.attr "ring"

muteNote :: RealTime -> RealTime -> Signal.Y -> Score.Event -> Score.Event
muteNote start dur dyn event = Score.setDynamic dyn $ event
    { Score.eventStart = start
    , Score.eventDuration = dur
    }

setMute :: RealTime -> Score.Event -> Score.Event
setMute start event = Score.addAttributes Attrs.mute $
    event { Score.eventStart = start, Score.eventDuration = 0 }


-- * gliss

cGlissAbsolute, cGliss :: Derive.Generator Derive.Note
cGlissAbsolute = makeGliss "gliss-a" True
cGliss = makeGliss "gliss" False

-- | This provides two kinds of glissando: absolute takes an absolute amount
-- of time for the entire glissando, while non-absolute takes a certain amount
-- of time per string, and hence will have a different duration depending on
-- how many strings.
makeGliss :: Derive.CallName -> Bool -> Derive.Generator Derive.Note
makeGliss name isAbsolute = Derive.generator module_ name mempty
    "Glissando along the open strings. The standard version divides the `time`\
    \ among the number of notes, while the -a (absolute) version gives `time`\
    \ to each note."
    $ Sig.call ((,,,)
    <$> Sig.required "start"
        "Start this many strings above or below the destination pitch."
    <*> (if isAbsolute
        then Sig.defaulted "time" (Typecheck.real 0.25)
            "Time in which to play the glissando."
        else Sig.defaulted "time" (Typecheck.real 0.075)
            "Time between each note.")
    <*> Sig.defaulted "dyn" (Nothing :: Maybe Sig.Dummy)
        "Start at this dyn, and interpolate\
        \ to the destination dyn. If not given, the dyn is constant."
    <*> StringUtil.openStringsEnv
    ) $ \(glissStart, Typecheck.DefaultReal time, maybeStartDyn,
        openStrings) ->
    Sub.inverting $ \args -> do
        end <- Args.realStart args
        time <- Call.realDuration end time
        destPitch <- Call.getTransposed end
        destDyn <- Call.dynamic end
        let startDyn = fromMaybe destDyn maybeStartDyn
        pitches <- glissPitches openStrings destPitch glissStart
        let totalTime = if isAbsolute then time
                else time * fromIntegral (length pitches)
        Ly.whenLilypond (GraceUtil.lilyGrace args (end - time) pitches) $
            gliss pitches totalTime startDyn destDyn end
                <> Call.placedNote args

glissPitches :: [PSignal.Pitch] -> PSignal.Transposed -> Int
    -> Derive.Deriver [PSignal.Pitch]
glissPitches openStrings destPitch glissStart = do
    -- Round the NN so a slightly off note start doesn't make this string get
    -- doubled.
    destNn <- Num.roundDigits 2 <$> Pitches.pitchNn destPitch
    -- TODO shouldn't need to eval them all
    openNns <- mapM (Pitches.pitchNn . PSignal.coerce) openStrings
    let strings = Lists.sortOn snd $ zip openStrings openNns
    -- 0 2 4 6 8 10
    pure $ if glissStart >= 0
        -- 5 -> 6 8 10 -> 10 8 6 5
        then reverse $ take glissStart $ map fst $
            dropWhile ((<=destNn) . snd) strings
        -- 5 -> 0 2 4
        else Lists.takeEnd (-glissStart) $ map fst $
            takeWhile ((<destNn) . snd) strings

gliss :: [PSignal.Pitch] -> RealTime -> Signal.Y -> Signal.Y -> RealTime
    -> Derive.NoteDeriver
gliss pitches time startDyn endDyn end = do
    let dur = time / fromIntegral (length pitches)
        start = end - time
        ts = take (length pitches) (Lists.range_ start dur)
        dyns = map (Num.scale startDyn endDyn . RealTime.toSeconds
            . Num.normalize start end) ts
    scoreTs <- mapM Derive.score ts
    scoreDur <- Call.scoreDuration end dur
    let note (t, p, dyn) = Derive.place t scoreDur $ Call.withDynamic dyn $
            Call.pitchedNote p
    mconcat $ map note $ zip3 scoreTs pitches dyns


-- * harmonic

cNthHarmonic :: Derive.Generator Derive.Note
cNthHarmonic = Derive.generator module_ "harmonic" Tags.inst
    "Play a specific harmonic on a specific string."
    $ Sig.call ((,,)
    <$> (Typecheck.positive <$>
        Sig.defaulted "n" (1 :: Int) "Play this harmonic.")
    <*> fingerArg
    <*> Sig.requiredEnvironKey EnvKey.string "Play on this string."
    ) $ \(harmonic, finger, string) -> Sub.inverting $ \args -> do
        string <- StringUtil.string string
        Call.place args $
            Derive.withConstantControl Controls.finger finger $
            Call.pitchedNote $
            Twelve.nnPitch $ Pitch.modifyHz (*harmonic) $
            StringUtil.strNn string

cHarmonic :: Derive.Generator Derive.Note
cHarmonic = Derive.generator module_ "harmonic" Tags.inst
    "Play the given pitch as a harmonic, possibly restricted to a string.\
    \ Otherwise, pick the lowest harmonic where the pitch fits."
    $ Sig.call ((,,,)
        <$> fingerArg
        <*> Sig.defaulted "h1" False
            "Ok to pick an open string as the 1st harmonic?"
        <*> StringUtil.stringEnv
        <*> StringUtil.openStringsEnv
    ) $ \(finger, h1Ok, maybeString, openStrings) -> Sub.inverting $
    \args -> do
        nn <- Pitches.pitchNn =<< Call.getTransposed =<< Args.realStart args
        openStrings <- mapM StringUtil.string openStrings
        maybeString <- traverse StringUtil.string maybeString
        (string, harmonic) <- Derive.requireRight id $
            StringUtil.findHarmonic h1Ok highestHarmonic openStrings
                maybeString nn
        StringUtil.withString string $ Call.place args $
            Derive.withConstantControl Controls.finger finger $
            Call.pitchedNote $
            Twelve.nnPitch $ touchInterval harmonic (StringUtil.strNn string)

highestHarmonic :: StringUtil.Harmonic
highestHarmonic = 13

touchInterval :: Int -> Pitch.NoteNumber -> Pitch.NoteNumber
touchInterval harmonic = Pitch.modifyHz (* fromIntegral harmonic)

fingerArg :: Sig.Parser Double
fingerArg = Sig.defaulted "finger" (0.035 :: Double)
    "Weight of the finger touching the string, in newtons."
