-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
-- | Convert Lilypond Events to lilypond code.
--
-- It's a terrible name, but what else am I supposed to call it?  Render?
-- Realize?  Perform?
module Perform.Lilypond.Process (
    process, convertToRests
    , parseKey
    , Ly(..), Note(..)
    , Voices(..), Voice(..)
#ifdef TESTING
    , module Perform.Lilypond.Process
#endif
) where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.NEs as NEs
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Then as Then

import qualified Derive.Attrs as Attrs
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Types as Types
import           Perform.Lilypond.Types
    (Event(..), Time, ToLily, eventAttributes, eventEnd, toLily)
import qualified Perform.Pitch as Pitch

import           Global
import           Types


-- | Automatically add lilypond code for certain attributes.
simpleArticulations :: [(Attrs.Attributes, Code)]
simpleArticulations =
    [ (Attrs.mute, "-+")
    -- Previously pizz<>right, comment on "Attrs".
    , (Attrs.pizzRight, "-+")
    , (Attrs.marcato, "-^")
    , (Attrs.staccato, "-.")
    , (Attrs.portato, "-_")
    , (Attrs.tenuto, "--")
    , (Attrs.accent, "->")
    , (Attrs.trem, ":32")
    ]

-- | Certain attributes are modal, in that they emit one thing when they
-- start, and another when they stop.
modalArticulations :: [(Attrs.Attributes, Code, Code)]
modalArticulations =
    [ (Attrs.pizz, "^\"pizz.\"", "^\"arco\"")
    , (Attrs.nv, "^\"nv\"", "^\"vib\"")
    , (Attrs.pont, "^\"sul pont.\"", "^\"loco\"")
    ]

-- * convert_to_rests

-- | Convert a staff to all rests, keeping the key, clef, and meter changes.
-- have predicates that recognize those, and keep those Codes
convertToRests :: [Either Voices Ly] -> [Ly]
convertToRests = hush . filter wanted . concatMap flatten
    where
    flatten (Left (Voices voices)) = case voices of
        [] -> []
        (_, lys) : _ -> lys
    flatten (Right ly) = [ly]
    wanted (LyCode code) = any (`Text.isPrefixOf` code)
        ["\\time ", "\\key ", "\\bar "]
    wanted _ = True
    hasDuration (LyNote n) = Just [noteDuration n]
    hasDuration (LyRest r) =
        Just $ map Types.durToNoteDur $ Types.timeToDurs $ restTime r
    hasDuration _ = Nothing
    hush lys = -- TODO simplify durs
        map (LyRest . makeRest . HiddenRest) (concat durs) ++ case nonNotes of
            ly : rest -> ly : hush rest
            [] -> []
        where (durs, nonNotes) = Lists.spanWhile hasDuration lys

-- * process

data Chunk = ChunkNotes [Event] | ChunkVoices (VoiceMap Event)
    deriving (Show)

instance Pretty Chunk where
    format (ChunkNotes events) =
        Pretty.constructor "ChunkNotes" [Pretty.format events]
    format (ChunkVoices voices) =
        Pretty.constructor "ChunkVoices" [Pretty.format voices]

-- | This is the top level function which converts a stream of Events on one
-- staff into lilypond code.
--
-- . First pass finds voice boundaries.
-- . Then insert rests.
-- . Convert [Event]->[Ly]: keys, meter splitting, tuplet and tremolo.
-- . Merge 0 dur events 'FreeCode' in [Ly] since they have been split according
-- to the meter.
process :: Types.Config -> Time -> [Meter.Meter] -- ^ one for each measure
    -> [Event] -> Either Log.Msg [Either Voices Ly]
process config start meters events_ = first toLog $ do
    let (freeCodes, events) = Lists.partitionOn freeCode events_
    chunks <- either (Left . ConvertError Nothing) pure $
        collectChunks events
    let end = start + Num.sum (map Meter.measureTime meters)
    -- Debug.tracepM "meters" (start, end, length meters,
    --     zip [1 :: Int ..] (_meter_starts start meters))
    chunks <- pure $ mergeNoteCodeChunks $
        insertRestsChunks start end chunks
    let state = makeState config start meters defaultKey
    key <- maybe (pure defaultKey)
        (fmap fst . runConvert state . lookupKey) (Lists.head events)
    state <- pure $ state { stateKey = key }
    (lys, _) <- runConvert state $ convert chunks
    lys <- pure $ mergeFreeCode start freeCodes lys
    let meter = fromMaybe Meter.defaultMeter (Lists.head meters)
    pure $ Right (LyCode $ "\\time " <> toLily meter)
        : Right (LyCode $ toLily key)
        : lys
        ++ [Right (LyCode "\\bar \"|.\"")]

_meterStarts :: Time -> [Meter.Meter] -> [(Time, Meter.Meter)]
_meterStarts start meters = snd $ List.mapAccumL add start meters
    where add t meter = (t + Meter.measureTime meter, (t, meter))

-- | Group voice and non-voice Events into Chunks.
collectChunks :: [Event] -> Either Error [Chunk]
collectChunks = go
    where
    go [] = pure []
    go events = do
        (noVoice, events) <- withoutVoice events
        (voice, events) <- withVoice events
        let collected = filter nonempty [ChunkNotes noVoice, ChunkVoices voice]
        (collected ++) <$> go events
    nonempty (ChunkNotes []) = False
    nonempty (ChunkVoices []) = False
    nonempty _ = True
    withVoice events = do
        (voice, remain) <- collectVoices events
        let tails = mapMaybe (Lists.last . snd) voice
        whenJust (Lists.head remain) $ \e ->
            whenJust (List.find (Types.eventOverlaps e) tails) $ \over ->
                Left $ "last voice " <> pretty over
                    <> " overlaps first non-voice " <> pretty e
        pure (voice, remain)
    withoutVoice events = do
        let (without, remain) = span ((==Nothing) . eventVoice) events
        whenJust ((,) <$>
            Lists.last without <*> Lists.head remain) $ \(e1, e2) ->
                when (Types.eventOverlaps e1 e2) $
                    Left $ "last non-voice " <> pretty e1
                        <> " overlaps first voice " <> pretty e2
        pure (without, remain)

-- | Span events until they don't have a 'Constants.v_voice' val.
--
-- Previously I tried to only split voices where necessary by only spanning
-- overlapping notes, or notes with differing voices.  But even when it worked
-- as intended, joining voices this aggressively led to oddities because it
-- would turn any two voices with the same duration notes into a chord.  So now
-- I simplify voices only at the measure level, in 'simplify_voices'.
collectVoices :: [Event] -> Either Text (VoiceMap Event, [Event])
collectVoices events = do
    let (voice, remain) = Lists.spanWhile (\e -> (,e) <$> eventVoice e) events
    voice <- forM voice $ \(errOrVoice, event) -> (,event) <$> errOrVoice
    pure (Lists.groupFst voice, remain)

insertRestsChunks :: Time -> Time -> [Chunk] -> [Chunk]
insertRestsChunks start end = Then.mapAccumL insert start final
    where
    insert !t (ChunkNotes events) =
        ChunkNotes <$> insertRests Nothing t events
    insert !t (ChunkVoices voices) = case getEnd voices of
        Nothing -> (t, ChunkVoices [])
        Just end -> (end, ChunkVoices voices2)
            where
            voices2 = map (second (snd . insertRests (Just end) t)) voices
    getEnd = Lists.maximum . map eventEnd . mapMaybe (Lists.last . snd)
    final t
        | t < end = [ChunkNotes [restEvent t (end-t)]]
        | otherwise = []

-- | Fill gaps between events with explicit rests.  Zero duration note code
-- events have the effect of splitting up the rests.
insertRests :: Maybe Time -> Time -> [Event] -> (Time, [Event])
insertRests maybeEnd = go
    where
    go t [] = case maybeEnd of
        Just end | end > t -> (end, [restEvent t (end - t)])
        _ -> (t, [])
    go t (event : events) =
        ((rest ++ event : here) ++) <$>
            go (max t (maximum (map eventEnd (event : here)))) there
        where
        rest = if gap <= 0 then [] else [restEvent t gap]
        (here, there) = span ((<= eventStart event) . eventStart) events
        gap = eventStart event - t

restEvent :: Time -> Time -> Event
restEvent start dur = Event
    { eventStart = start
    , eventDuration = dur
    , eventPitch = Nothing
    , eventInstrument = ScoreT.emptyInstrument
    , eventEnviron = mempty
    , eventStack = Stack.empty
    , eventClipped = False
    }

mergeNoteCodeChunks :: [Chunk] -> [Chunk]
mergeNoteCodeChunks = map merge
    where
    merge (ChunkNotes events) = ChunkNotes (mergeNoteCode events)
    merge (ChunkVoices voices) =
        ChunkVoices (map (second mergeNoteCode) voices)

-- This has to be done after rests are present, so it can attach to rests.
mergeNoteCode :: [Event] -> [Event]
mergeNoteCode = go
    where
    go [] = []
    go (event : events) = case nonzero of
        p : ps -> addNoteCode codes p : ps ++ go there
        [] -> go there -- TODO warn about dropped zero events
        -- TODO also warn about zero dur events with no code
        where
        (here, there) = span ((<= eventStart event) . eventStart) events
        (zero, nonzero) = List.partition codeEvent (event : here)
        codes = concatMap (Constants.environCode . eventEnviron) zero
    -- Subdivision events are a special case of zero-dur events which
    -- are not code, but are directives to the meter splitting.
    -- TODO isn't there a better way?
    codeEvent e = zeroDur e && lookupSubdivision e == Nothing

addNoteCode :: [(Constants.CodePosition, Text)] -> Event -> Event
addNoteCode codes event =
    event { eventEnviron = foldr merge (eventEnviron event) codes }
    where
    merge (pos, code) env = case Env.maybeVal k env of
        Nothing -> Env.insertVal k code env
        Just old -> Env.insertVal k (old <> " " <> code) env
        where k = Constants.positionKey pos

-- * convert

-- TODO maybe return Either Voices [Ly] so I can avoid the concat and all the
-- extra Rights?
convert :: [Chunk] -> ConvertM [Either Voices Ly]
convert = concatMapM go
    where
    go :: Chunk -> ConvertM [Either Voices Ly]
    go (ChunkNotes events) =
        map Right <$> untilComplete (convertChunk True) events
    go (ChunkVoices voices) = simplifyVoices <$> voicesToLy voices

-- | Mix code events into the Lys, depending on their prepend or append attrs.
mergeFreeCode :: Time -> [FreeCode] -> [Either Voices Ly]
    -> [Either Voices Ly]
mergeFreeCode = go
    where
    go start codes (Right ly : lys) =
        Right applied : go (start + lyDuration ly) remain lys
        where (applied, remain) = mergeLy start codes ly
    go start codes (Left (Voices ((v, v1Lys) : voices)) : lys) =
        Left (Voices ((v, applied) : voices)) : go end remain lys
        where (applied, (end, remain)) = mergeLys start codes v1Lys
    go start codes (Left (Voices []) : lys) = go start codes lys
    go _ codes [] = map (Right . LyCode . snd) (concatMap snd codes)

    mergeLys start codes (ly : lys) =
        first (applied:) $ mergeLys (start + lyDuration ly) remain lys
        where (applied, remain) = mergeLy start codes ly
    mergeLys start codes [] = ([], (start, codes))

    mergeLy start codes ly = case ly of
        _ | null here -> (ly, codes)
        LyBarline {} -> (ly, codes)
        LyCode {} -> (ly, codes)
        _ -> case applyFreeCode (concatMap snd here) ly of
            -- TODO I couldn't find anything to attach to, so I should drop it
            -- and emit a warning instead of just attaching it to the next one.
            Nothing -> (ly, codes)
            Just applied -> (applied, there)
        where
        (here, there) = span ((<end) . fst) codes
        end = start + lyDuration ly

applyFreeCode :: [(Constants.FreeCodePosition, Code)] -> Ly -> Maybe Ly
applyFreeCode codes ly = case ly of
    LyNote note -> Just $ LyNote $ note
        { notePrepend = prepend ++ notePrepend note
        , noteAppend = noteAppend note ++ append
        }
    LyRest rest -> Just $ LyRest $ rest
        { restPrepend = prepend ++ restPrepend rest
        , restAppend = restAppend rest ++ append
        }
    LyNested nested -> do
        lys <- applyNested $ NonEmpty.toList (nestedContents nested)
        lys <- NonEmpty.nonEmpty lys
        pure $ LyNested $ nested { nestedContents = lys }
    _ -> Nothing
    where
    (prepend, append) = bimap (map snd) (map snd) $
        List.partition ((==Constants.FreePrepend) . fst) codes
    -- Apply to the first place that takes it, or Nothing if there was none.
    applyNested [] = Nothing
    applyNested (ly : lys) = case applyFreeCode codes ly of
        Nothing -> (ly:) <$> applyNested lys
        Just applied -> Just $ applied : lys

type FreeCode = (Time, [(Constants.FreeCodePosition, Code)])

freeCode :: Event -> Maybe FreeCode
freeCode event
    | null code = Nothing
    | otherwise = Just (eventStart event, code)
    where code = Constants.environFreeCode (eventEnviron event)

untilComplete :: Monad m => ([a] -> m ([b], [a])) -> [a] -> m [b]
untilComplete f = go
    where
    go [] = pure []
    go as = do
        (bs, as) <- f as
        (bs++) <$> go as

-- ** convert_chunk

-- | Convert the rests for the first event, and a single slice of time.
-- If notes had to be split and tied, they are put back into the remaining
-- events.
-- TODO the name should change, now that Chunk is something else
convertChunk :: Bool -> [Event] -> ConvertM ([Ly], [Event])
convertChunk _ [] = pure ([], [])
convertChunk metered (event : events) = withEvent event $ if
    | Just ((scoreDur, realDur), (_, remain))
            <- find Constants.getTuplet -> do
        scoreDur <- realToTime scoreDur
        realDur <- realToTime realDur
        convertTuplet start scoreDur realDur remain
    | Just ((), (event, remain)) <- find hasTremolo ->
        convertTremolo event remain
    | otherwise -> convertChord metered (event :| events)
    where
    hasTremolo env
        | Env.isSet Constants.vTremolo env = Just ()
        | otherwise = Nothing
    start = eventStart event
    find match = findHere start (match . eventEnviron) (event : events)

findHere :: Time -> (Event -> Maybe a) -> [Event]
    -> Maybe (a, (Event, [Event]))
findHere start match events = msum $ map find (focus here)
    where
    find (e, es) = (, (e, es ++ later)) <$> match e
    (here, later) = span ((<=start) . eventStart) events

-- | Focus on each element in turn, removing it from the list.
focus :: [a] -> [(a, [a])]
focus (x:xs) = (x, xs) : map (second (x:)) (focus xs)
focus [] = []

lookupEnv :: Typecheck.Typecheck a => Env.Key -> Event -> Maybe a
lookupEnv k = Env.maybeVal k . eventEnviron

lookupSubdivision :: Event -> Maybe Text
lookupSubdivision = lookupEnv Constants.vSubdivision

getCode :: Env.Key -> Event -> Code
getCode k = fromMaybe "" . lookupEnv k

zeroDur :: Event -> Bool
zeroDur = (==0) . eventDuration

-- ** convert_tuplet

-- | Collect the notes inside the duration, run a special 'convert_chunk' on
-- them where the meter is ok with any duration, then wrap that in \tuplet,
-- and increment time by duration * 3/2.
convertTuplet :: Time
    -> Time -- ^ score duration of notes of the tuplet
    -> Time
    -> [Event] -- ^ extract the overlapped events and render in the tuplet
    -> ConvertM ([Ly], [Event])
convertTuplet start scoreDur realDur events = do
    let (inTuplet, out) = span ((< start + scoreDur) . eventStart) events
    old <- State.get

    -- The usual convention for tuplets is that the notes have longer values
    -- but are shortened to fit, e.g. 3/8 into the time of 2/8.  But duplets
    -- are the opposite, they are shorter and are made longer, e.g. 2/8 in the
    -- time of 3/8.  Don't ask me why, that's just the convention.  I always
    -- get shorter notes so they fit under the parent tuplet event, so for
    -- triplet 1/8s, I get score_dur = 3/16, real_dur = 2/8, then double to
    -- score_dur = 3/8s.
    let isDuplet = Ratio.numerator (Types.toWhole scoreDur) == 1
    let factor = if isDuplet then 1 else 2
    scoreDur <- pure $ if isDuplet then scoreDur else scoreDur * 2
    when (realDur <= 0) $
        throw $ "tuplet with a real_dur of 0: "
            <> pretty (start, scoreDur, realDur)
    let divisor = realToFrac scoreDur / realToFrac realDur :: Rational
    -- This probably means the notes have been stretched or something and
    -- aren't on simple divisions.
    when (Ratio.numerator divisor > 15 || Ratio.denominator divisor > 15) $
        throw $ "tuplet factor is too complicated: " <> showt scoreDur
            <> "/" <> showt realDur

    -- TODO it's probably wrong to do this unmetered.  I should instead act as
    -- if the meter has changed.
    lys <- untilComplete (convertChunk False) $
        snd $ insertRests (Just (start + scoreDur)) start $
        map (stretch factor start) inTuplet
    lys <- maybe (throw "empty tuplet") pure $ NonEmpty.nonEmpty lys

    -- Rewind time back to before the tuplet.
    State.modify' $ \state -> state
        { stateTime = stateTime old
        , stateMeters = stateMeters old
        , stateMeasureStart = stateMeasureStart old
        , stateMeasureEnd = stateMeasureEnd old
        }
    barline <- withContext "converting tuplet" $
        advanceMeasure (start + realDur)
    let code = tupletCode realDur divisor
            (countNotesRests (NonEmpty.toList lys)) lys
    pure (code : maybe [] (:[]) barline, out)

-- | Convention is that the number on the tuplet is at least the number of
-- notes inside, but lilypond doesn't do that automatically.
tupletCode :: Time -> Rational -> Int -> NonEmpty Ly -> Ly
tupletCode dur ratio notes contents = LyNested $ Nested
    { nestedPrefix = "\\tuplet " <> showt num <> "/" <> showt denom <> " {"
    , nestedContents = contents
    , nestedSuffix = "}"
    , nestedDuration = dur
    }
    where
    num = Ratio.numerator ratio * factor
    denom = Ratio.denominator ratio * factor
    factor = (2^) $ max 0 $ ceiling $ logBase 2 $
        fromIntegral notes / fromIntegral (Ratio.numerator ratio)

realToTime :: RealTime -> ConvertM Time
realToTime t = do
    quarter <- State.gets $ Types.configQuarterDuration . stateConfig
    pure $ Types.realToTime quarter t

stretch :: Int -> Time -> Event -> Event
stretch factor start event
    | factor == 1 = event
    | otherwise = event
        { eventStart =
            fromIntegral factor * (eventStart event - start) + start
        , eventDuration = fromIntegral factor * eventDuration event
        }

-- ** convert_tremolo

-- | Get just pitch and code from in_tremolo notes, force them to 32nd note
-- duration, wrap in (), and wrap the whole thing in \repeat tremolo { ... }.
--
-- TODO This is sort of an ad-hoc reimplementation of 'convert_chord', which is
-- grody.  But the rules are different in a tremolo, duration-wise it's like
-- a chord, but code always has to go on the notes inside, not after the whole
-- thing.
convertTremolo :: Event -> [Event] -> ConvertM ([Ly], [Event])
convertTremolo tremoloEvent events = do
    dur <- getAllowedDur
    let (inTremolo, out) = span ((< start + dur) . eventStart) events
    inTremolo <- consumeSubdivisions inTremolo
    prevAttrs <- State.gets statePrevAttrs
    let (nextAttrs, notes) = tremoloNotes prevAttrs inTremolo
    State.modify' $ \state -> state { statePrevAttrs = nextAttrs }

    -- TODO promote this to the caller so everyone doesn't have to do it?
    barline <- advanceMeasure (start + dur)

    let (times, frac) = properFraction $ Types.toWhole dur * 16
    when (frac /= 0) $ throw $ "dur " <> pretty dur
        <> " doesn't yield an integral number of 16th notes: "
        <> pretty (Types.toWhole dur * 16)
    let (prepend, append) =
            eventsNoteCode Constants.Chord True True inTremolo
        (nprepend, nappend) =
            eventsNoteCode Constants.Note True True inTremolo

    let clipped
            | dur >= totalDur = []
            | otherwise = mapMaybe (clipEvent (start + dur))
                (tremoloEvent : inTremolo)
    notes <- maybe (throw "no notes in tremolo") pure $
        NonEmpty.nonEmpty notes
    let note = tremoloNote dur times notes
            (prepend ++ nprepend) (nappend ++ append)
    pure (LyNested note : maybe [] (:[]) barline, clipped ++ out)

    where
    start = eventStart tremoloEvent
    totalDur = eventDuration tremoloEvent
    getAllowedDur = do
        meter <- getSubdivision
        measureStart <- State.gets stateMeasureStart
        pure $ Types.noteDurToTime $ Meter.allowedDuration useDot meter
            (start - measureStart)
            (start - measureStart + eventDuration tremoloEvent)
        where useDot = True

tremoloNote :: Time -> Int -> NonEmpty Note -> [Code] -> [Code] -> Nested
tremoloNote dur times (note :| notes) prepend append = Nested
    { nestedPrefix = "\\repeat tremolo " <> showt times <> " {"
    , nestedContents = fmap LyNote (withCode :| notes)
    , nestedSuffix = "}"
    , nestedDuration = dur
    }
    where
    withCode = note
        { notePrepend = prepend ++ notePrepend note
        , noteAppend = noteAppend note ++ append
        }

tremoloNotes :: Attrs.Attributes -> [Event] -> (Attrs.Attributes, [Note])
tremoloNotes prevAttrs = List.mapAccumL make prevAttrs . zipFirstLast
    where
    make prevAttrs (isFirst, event, isLast) = (nextAttrs,) $ Note
        { notePitches = NotePitch
            { pitchPitch = pitchCode isFirst event
            , pitchTie = NoTie
            , pitchPrepend = []
            , pitchAppend = slur
            } :| []
        , noteDuration = Types.NoteDuration Types.D32 False
        , notePrepend = []
        , noteAppend = attrsCodes
        , noteStack = Lists.last $ Stack.toUi (eventStack event)
        }
        where
        slur
            | isFirst && not isLast = ["("]
            | not isFirst && isLast = [")"]
            | otherwise = []
        (attrsCodes, nextAttrs) =
            attrsToCode prevAttrs (eventAttributes event)

zipFirstLast :: [a] -> [(Bool, a, Bool)]
zipFirstLast = map toBool . Lists.zipNeighbors
    where
    toBool (prev, cur, next) =
        (Maybe.isNothing prev, cur, Maybe.isNothing next)


-- ** convert_chord

-- | This is the lowest level of conversion.  It converts a vertical slice of
-- notes starting at the first event, and returns the rest.
convertChord :: Bool -> NonEmpty Event -> ConvertM ([Ly], [Event])
convertChord metered events = do
    key <- lookupKey (NonEmpty.head events)
    state <- State.get
    config <- State.gets stateConfig
    let (here, there) = break
            ((> eventStart (NonEmpty.head events)) . eventStart)
            (NonEmpty.tail events)
        next = eventStart <$> List.find (not . zeroDur) there
    here <- consumeSubdivisions (NonEmpty.head events : here)
    meter <- if metered then Just <$> getSubdivision else pure Nothing
    case NonEmpty.nonEmpty here of
        Nothing -> pure ([], there)
        Just here -> do
            let (chordLy, end, lastAttrs, clipped) = makeNote
                    config (stateMeasureStart state) (statePrevAttrs state)
                    meter here next
            barline <- if metered then advanceMeasure end
                else advanceUnmetered end >> pure Nothing
            -- Rests don't affect the key, or emit key changes.  This is
            -- because rests are created with an empty environ, so otherwise
            -- they would be constantly unsetting the key.
            keyChange <- case chordLy of
                LyRest {} -> pure []
                _ -> do
                    State.modify' $ \state -> state
                        { stateKey = key
                        , statePrevAttrs = lastAttrs
                        }
                    pure [LyCode (toLily key) | key /= stateKey state]
            pure
                ( keyChange ++ [chordLy] ++ maybe [] (:[]) barline
                , clipped ++ there
                )

-- | Handle any Constants.v_subdivision events and filter them out.
consumeSubdivisions :: [Event] -> ConvertM [Event]
consumeSubdivisions events = mapM_ update subdivisions >> pure normal
    where
    (subdivisions, normal) = Lists.partitionOn lookupSubdivision events
    update "" = State.modify' $ \state ->
        state { stateSubdivision = Nothing }
    update m = do
        meter <- either throw pure $ first
            (("can't parse meter in " <> pretty Constants.vSubdivision
                <> ": " <> showt m <> ": ")<>)
            (Meter.parseMeter m)
        State.modify' $ \state -> state { stateSubdivision = Just meter }

makeNote :: Types.Config -> Time -> Attrs.Attributes
    -- ^ Previous note's Attributes, to track 'modal_articulations'.
    -> Maybe Meter.Meter
    -> NonEmpty Event -- ^ Events that occur at the same time.
    -> Maybe Time -- ^ start of the next note
    -> (Ly, Time, Attrs.Attributes, [Event]) -- ^ (note, note end time, clipped)
makeNote config measureStart prevAttrs maybeMeter chord next =
    (ly, allowedEnd, nextAttrs, clipped)
    where
    -- If the time is a full measure, then it uses allowed_time, not
    -- allowed_dur.
    ly = case NonEmpty.nonEmpty notePitches of
        Nothing -> LyRest $ Rest
            { restType = case maybeMeter of
                Just meter | fullMeasureRest ->
                    FullMeasure (Meter.meterDenom meter) (Meter.timeNum meter)
                _ -> NormalRest allowedDur
            , restPrepend = prependChord
            , restAppend = appendChord
            }
        Just pitches -> LyNote $ Note
            { notePitches = pitches
            , noteDuration = allowedDur
            , notePrepend = prependChord
            , noteAppend = appendChord ++ attrsCodes
            , noteStack = Lists.last $ Stack.toUi $ eventStack $
                NonEmpty.head chord
            }
    notePitches = do
        -- Sorting by pitch puts the chord notes in a predictable order.  Some
        -- lilypond notation, such as glissandoMap, refers to chord notes by
        -- index.
        event <- Lists.sortOn eventPitch $ NonEmpty.toList chord
        let pitch = pitchCode (isFirst event) event
        guard $ not (Text.null pitch)
        let tie = noteTie event
        let (prepend, append) = eventNoteCode Constants.Note
                (isFirst event) (isLast event) event
        pure $ NotePitch
            { pitchPitch = pitch
            , pitchTie = tie
            , pitchPrepend = prepend
            , pitchAppend = append
            }
    (attrsCodes, nextAttrs)
        | isRest = ([], prevAttrs)
        | otherwise = attrsToCode prevAttrs $
            mconcat $ map eventAttributes $ NonEmpty.toList chord

    (prependChord, appendChord) =
        bimap (Lists.unique . concat) (Lists.unique . concat) $ unzip
            [ eventNoteCode Constants.Chord (isFirst e) (isLast e) e
            | e <- NonEmpty.toList chord
            ]

    noteTie event
        | eventEnd event <= allowedEnd = NoTie
        | Text.null direction = TieNeutral
        | direction == "^" = TieUp
        | otherwise = TieDown
        where
        direction :: Text
        direction = getCode Constants.vTieDirection event
    isFirst = not . eventClipped
    isLast event = case noteTie event of
        NoTie -> True
        _ -> False

    clipped = mapMaybe (clipEvent allowedEnd) (NonEmpty.toList chord)
    start = eventStart (NonEmpty.head chord)

    -- FullMeasure rests are special because they can have a non-Duration
    -- time.
    allowedEnd = start + allowedTime
    (fullMeasureRest, allowedTime) = case maybeMeter of
        Just meter | isRest && start == measureStart
                && maxEnd - start >= Meter.measureTime meter ->
            (True, Meter.measureTime meter)
        _ -> (False, Types.noteDurToTime allowedDur)
    allowedDur = case maybeMeter of
        Nothing -> Types.timeToNoteDur (maxEnd - start)
        Just meter ->
            Meter.allowedDuration useDot meter inMeasure (maxEnd - start)
            where
            -- Dots are always allowed for non-binary meters.
            useDot = not isRest || Types.configDottedRests config
                || not (Meter.isBinary meter)
            inMeasure = start - measureStart
    -- Maximum end, the actual end may be shorter since it has to conform to
    -- a Duration.
    maxEnd = minIf next $ NEs.minimum (fmap eventEnd chord)
    isRest = null notePitches

eventsNoteCode :: Constants.Attach -> Bool -> Bool -> [Event]
    -> ([Text], [Text])
eventsNoteCode attach isFirst isLast =
    bimap (Lists.unique . concat) (Lists.unique . concat)
    . unzip . map (eventNoteCode attach isFirst isLast)

eventNoteCode :: Constants.Attach -> Bool -> Bool -> Event -> ([Text], [Text])
eventNoteCode attach isFirst isLast =
    extract . Constants.environCode . eventEnviron
    where
    extract codes =
        (get Constants.Prepend codes, get Constants.Append codes)
    get pos codes =
        [ code
        | (Constants.CodePosition a p d, code) <- codes
        , a == attach, p == pos, d `elem` dists
        ]
    dists = Constants.All : [Constants.First | isFirst]
        ++ [Constants.Last | isLast]

minIf :: Ord a => Maybe a -> a -> a
minIf ma b = maybe b (min b) ma

-- ** ly code env vars

tUnwords :: [Text] -> Text
tUnwords = Text.unwords . filter (not . Text.null) . map Text.strip

pitchCode :: Bool -> Event -> Code
pitchCode isFirst event =
    maybe "" toLily (eventPitch event)
        <> if isFirst then getCode Constants.vAppendPitch event else ""

attrsToCode :: Attrs.Attributes -> Attrs.Attributes
    -> ([Code], Attrs.Attributes)
    -- ^ (code to append, prev attrs for the next note)
attrsToCode prev current =
    (simple ++ starts ++ Maybe.catMaybes ends, mconcat (current : extras))
    where
    starts =
        [ start
        | (attr, start, _) <- modalArticulations
        , current `has` attr, not (prev `has` attr)
        ]
    (ends, extras) = unzip $ map (cancel prev) modalArticulations
    simple = [code | (attr, code) <- simpleArticulations, current `has` attr]
    cancel prev (attr, _, end)
        -- If prev doesn't have +nv, but this one is +staccato, then consider
        -- that this one also has +nv.  This avoids spurious vib marks on
        -- every staccato note.
        | prev `has` attr && not (current `has` attr) =
            if attr == Attrs.nv && any (current `has`) inherentlyNv
                then (Nothing, Attrs.nv)
                else (Just end, mempty)
        | otherwise = (Nothing, mempty)
    inherentlyNv = [Attrs.staccato, Attrs.harm, Attrs.pizz]
    has = Attrs.contain

-- ** voices_to_ly

-- | Like 'convert_chunk', but converts within a voice, which means no nested
-- voices are expected.
voicesToLy :: VoiceMap Event -> ConvertM (VoiceMap Ly)
voicesToLy [] = pure []
voicesToLy voices = do
    state <- State.get
    (states, voiceLys) <- unzip <$> mapM (convert state) voices
    case states of
        st : sts -> do
            unless (all ((== stateTime st) . stateTime) sts) $
                throw $ "inconsistent states after voices: " <> pretty (st:sts)
            State.put st
        [] -> pure ()
    pure voiceLys
    where
    convert state (v, events) = do
        (measures, finalState) <- tryRight $ runConvert state $
            untilComplete (convertChunk True) events
        pure (finalState, (v, measures))

-- | If a whole measure of a voice is empty, omit the voice for that measure.
--
-- Previously, I tried a more fine-grained approach, documented in
-- 'span_voices'.  This way is more complicated because it has to operate on
-- Lys since it needs to know where the measure boundaries are, but gives much
-- better results.
simplifyVoices :: VoiceMap Ly -> [Either Voices Ly]
simplifyVoices voices =
    concatMap (flatten . strip) $ splitVoicesAt restStarts voices
    where
    restStarts = Lists.dropDups id $ Lists.mergeLists id $
        concatMap (restsAt . snd) voices
    restsAt lys =
        [ [start, start + lyDuration ly]
        | (start, ly) <- withStarts 0 lys, isFullMeasureRest ly
        ]
    isFullMeasureRest (LyRest (Rest { restType = FullMeasure {} })) = True
    isFullMeasureRest _ = False

    -- Strip out voices that consist entirely of rests, keeping at least one.
    strip voices = case filter (not . restMeasure . snd) voices of
            [] -> take 1 voices
            voices -> voices
        where
        restMeasure = all $
            \ly -> isFullMeasureRest ly || lyDuration ly == 0
    flatten :: VoiceMap Ly -> [Either Voices Ly]
    flatten [] = []
    flatten [(_, lys)] = map Right lys
    flatten voices = [Left (Voices voices)]

-- | Split voices every place any of them has a full measure rest.
--
-- > [(1, xs), (2, ys)] -> [(1, x), (2, y)], [(1, x), (2, y)]
-- > [(1, xs), (2, ys)] -> [(1, [xs]), (2, [ys])]
splitVoicesAt :: [Time] -> VoiceMap Ly -> [VoiceMap Ly]
splitVoicesAt ts = rotate . map (second (splitAt ts))
    where
    -- This is really hard for me to understand, but I don't need to because
    -- the types work out :)
    -- [(1, [x, y]), (2, [a, b])] -> [[(1, x), (2, a)], [(1, y), (2, b)]]
    -- split_at should produce a [Ly] group for every split Time, but if it
    -- doesn't the rotate will drop all the other voices.
    rotate :: VoiceMap [Ly] -> [VoiceMap Ly]
    rotate voiceGroups = map (zip voices) (Lists.rotate lys)
        where (voices, lys) = unzip voiceGroups
    -- Ly times should always line up at measure boundaries, and the split
    -- times should all be at measure boundaries.  So this should return one
    -- [Ly] for each Time.
    splitAt :: [Time] -> [Ly] -> [[Ly]]
    splitAt times lys = go times $ zip (drop 1 $ lyStartTimes 0 lys) lys
        where
        go _ [] = []
        go [] rest = [map snd rest]
        go (t:ts) lys = map snd pre : go ts post
            where (pre, post) = span ((<=t) . fst) lys

withStarts :: Time -> [Ly] -> [(Time, Ly)]
withStarts start lys = zip (lyStartTimes start lys) lys

-- | drop 1 for end times.
lyStartTimes :: Time -> [Ly] -> [Time]
lyStartTimes start = scanl (+) start . map lyDuration

-- * misc

-- | Advance now to the given time, up to and including the end of the measure,
-- but it's an error to try to go past.  Return Ly with a LyBarline if this is
-- a new measure.
--
-- If I wanted to emit Barlines automatically I'd have to collect the output
-- [Ly] in the State, which I'd then need to parameterize since it can be
-- [Either Voices Ly] too.
advanceMeasure :: Time -> ConvertM (Maybe Ly)
advanceMeasure time = advance =<< State.get
    where
    advance state
        | time < stateTime state =
            throw $ "can't advance time backward: " <> pretty time
                <> " < " <> pretty (stateTime state)
        | time < stateMeasureEnd state = do
            State.put $ state { stateTime = time }
            pure Nothing
        | time == stateMeasureEnd state =
            case stateMeters state of
                prevMeter : meters -> advance1 prevMeter meters
                _ -> throw $ "out of meters, can't advance time to "
                    <> pretty time
        | otherwise =
            throw $ "can't advance time past barline: " <> pretty time
                <> " > " <> pretty (stateMeasureEnd state)
    advance1 prevMeter meters = do
        State.modify' $ \state -> state
            { stateMeters = meters
            , stateMeasureStart = stateMeasureEnd state
            , stateMeasureEnd = stateMeasureEnd state
                + Meter.measureTime (fromMaybe prevMeter (Lists.head meters))
            , stateTime = time
            }
        pure $ case Lists.head meters of
            Just meter -> Just $ LyBarline $
                if toLily prevMeter == toLily meter
                    then Nothing else Just meter
            _ -> Nothing

-- | Advance time without regard to meter or barlines.
advanceUnmetered :: Time -> ConvertM ()
advanceUnmetered time = advance =<< State.get
    where
    advance state
        | time < stateTime state =
            throw $ "can't advance unmetered time backward: " <> pretty time
                <> " < " <> pretty (stateTime state)
        | otherwise = State.put $ state { stateTime = time }

-- | Get the current subdivision and check it against the meter.  This is a way
-- to override the meter for the purposes of how durations are spelled.
getSubdivision :: ConvertM Meter.Meter
getSubdivision = do
    meters <- State.gets stateMeters
    meter <- maybe (throw "out of meters") pure $ Lists.head meters
    subdivision <- State.gets stateSubdivision
    case subdivision of
        Just sub
            | Meter.measureTime meter == Meter.measureTime sub -> pure sub
            | otherwise -> throw $ "subdivision " <> toLily sub
                <> " incompatible with meter " <> toLily meter
        Nothing -> pure meter

-- * ConvertM

type ConvertM =
    State.StateT State (Except.ExceptT ConvertError Identity.Identity)

data ConvertError = ConvertError (Maybe Stack.Stack) Error
    deriving (Eq, Show)

toLog :: ConvertError -> Log.Msg
toLog (ConvertError stack msg) = Log.msg Log.Warn stack msg

type Error = Text

runConvert :: State -> ConvertM a -> Either ConvertError (a, State)
runConvert state = Identity.runIdentity . Except.runExceptT
    . flip State.runStateT state

withEvent :: Event -> ConvertM a -> ConvertM a
withEvent event = flip Except.catchError $ \(ConvertError _ msg) ->
    Except.throwError $ ConvertError (Just (eventStack event)) msg

withContext :: Text -> ConvertM a -> ConvertM a
withContext prefix = flip Except.catchError $ \(ConvertError stack msg) ->
    Except.throwError $ ConvertError stack (prefix <> ": " <> msg)

data State = State
    -- Constant:
    { stateConfig :: Types.Config

    -- Changes on each measure:
    -- | One Meter for each expected measure in the output.
    -- The head of the list is the current meter.  It's valid for the meters to
    -- be [] as long as you don't have any more notes or rests to generate.
    , stateMeters :: [Meter.Meter]
    , stateMeasureStart :: Time
    , stateMeasureEnd :: Time

    -- Changes on each note:
    -- | Current position in time, aka the end of the previous note.
    , stateTime :: Time
        -- | Used in conjunction with 'modal_articulations'.
    , statePrevAttrs :: ~Attrs.Attributes
    , stateKey :: Key

    -- Changes on a directive.
    , stateSubdivision :: !(Maybe Meter.Meter)
    } deriving (Show)

instance Pretty State where
    format (State config meters mstart mend time prevAttrs key subdiv) =
        Pretty.record "State"
            [ ("config", Pretty.format config)
            , ("meters", Pretty.format meters)
            , ("measure_start", Pretty.format mstart)
            , ("measure_end", Pretty.format mend)
            , ("time", Pretty.format time)
            , ("prev_attrs", Pretty.format prevAttrs)
            , ("key", Pretty.format key)
            , ("subdivision", Pretty.format subdiv)
            ]

makeState :: Types.Config -> Time -> [Meter.Meter] -> Key -> State
makeState config start meters key = State
    { stateConfig = config
    , stateMeters = meters
    , stateMeasureStart = start
    , stateMeasureEnd = start + maybe 0 Meter.measureTime (Lists.head meters)
    , stateTime = start
    , statePrevAttrs = mempty
    , stateKey = key
    , stateSubdivision = Nothing
    }

-- ** util

throw :: Error -> ConvertM a
throw msg = do
    now <- State.gets stateTime
    Except.throwError $ ConvertError Nothing (pretty now <> ": " <> msg)

lookupVal :: Env.Key -> (Error -> Either Error a) -> a -> Event -> ConvertM a
lookupVal key parse deflt event = prefix $ do
    maybeVal <- Env.checkedVal key (eventEnviron event)
    maybe (Right deflt) parse maybeVal
    where
    prefix = either (throw . ((pretty key <> ": ") <>)) pure


-- * types

-- | Ultimately, the contants of Ly is just a bit of lilypond code.  They are
-- all converted to text via 'to_lily' and concatenated to form the full score.
-- But I encode a fair amount of structure into the data type, which is
-- convenient for debugging.  It could also could theoretically be further
-- modified, though I don't think I ever do that.  However, 'ly_duration' at
-- least is used to merge 'FreeCode' into a Ly stream.
data Ly
    = LyNote Note
    | LyRest Rest
    | LyNested Nested
    | LyBarline (Maybe Meter.Meter)
    | LyCode Code
    deriving (Show)

lyDuration :: Ly -> Time
lyDuration ly = case ly of
    LyNote note -> Types.noteDurToTime (noteDuration note)
    LyRest rest -> restTime rest
    LyNested n -> nestedDuration n
    _ -> 0

instance Pretty Ly where pretty = toLily

instance ToLily Ly where
    toLily ly = case ly of
        LyBarline Nothing -> "|"
        LyBarline (Just meter) -> "| " <> "\\time " <> toLily meter
        LyNote note -> toLily note
        LyRest rest -> toLily rest
        LyNested nested -> toLily nested
        LyCode code -> code

countNotesRests :: [Ly] -> Int
countNotesRests = Lists.count $ \ly -> case ly of
    LyNote {} -> True
    LyRest {} -> True
    _ -> False

-- | This represents a bit of nested lilypond code, e.g.
-- \something { contents }.
data Nested = Nested
    { nestedPrefix :: Code
    , nestedContents :: NonEmpty Ly
    , nestedSuffix :: Code
    , nestedDuration :: Time
    } deriving (Show)

instance ToLily Nested where
    toLily (Nested prefix contents suffix _) =
        tUnwords $ prefix : map toLily (NonEmpty.toList contents) ++ [suffix]

-- ** Note

data Note = Note
    -- | Greater than one pitch indicates a chord.
    { notePitches :: NonEmpty NotePitch
    , noteDuration :: Types.NoteDuration
    -- | Additional code to prepend to the note.
    , notePrepend :: [Code]
    -- | Additional code to append to the note.
    , noteAppend :: [Code]
    , noteStack :: Maybe Stack.UiFrame
    } deriving (Show)

data NotePitch = NotePitch
    { pitchPitch :: Text
    , pitchTie :: Tie
    , pitchPrepend :: [Code]
    , pitchAppend :: [Code]
    } deriving (Show)

data Tie = NoTie | TieNeutral | TieUp | TieDown deriving (Show)

-- | Arbitrary bit of lilypond code.  This type isn't used for non-arbitrary
-- chunks, like 'note_pitches'.
type Code = Text

instance ToLily Note where
    toLily (Note pitches dur prepend append _) =
        tUnwords $ prepend ++ [note] ++ append
        where
        lyDur = toLily dur
        note = case pitches of
            NotePitch pitch tie prepend append :| [] -> mconcat
                [ tUnwords prepend, " "
                , pitch, lyDur, toLily tie
                , tUnwords append
                ]
            _ -> "<" <> tUnwords (map toLily (NonEmpty.toList pitches))
                <> ">" <> lyDur

instance ToLily NotePitch where
    toLily (NotePitch pitch tie prepend append) = mconcat
        [ tUnwords prepend, " "
        , pitch, toLily tie
        , tUnwords append
        ]

instance ToLily Tie where
    toLily t = case t of
        NoTie -> ""
        TieNeutral -> "~"
        TieUp -> "^~"
        TieDown -> "_~"

-- ** Rest

data Rest = Rest
    { restType :: RestType
    , restPrepend :: [Code]
    , restAppend :: [Code]
    } deriving (Show)

makeRest :: RestType -> Rest
makeRest typ = Rest
    { restType = typ
    , restPrepend = []
    , restAppend = []
    }

data RestType
    = NormalRest Types.NoteDuration
    | HiddenRest Types.NoteDuration
    | FullMeasure Types.Duration Int
    deriving (Show)

restTime :: Rest -> Time
restTime rest = case restType rest of
    NormalRest dur -> Types.noteDurToTime dur
    HiddenRest dur -> Types.noteDurToTime dur
    FullMeasure dur mult -> Types.multiplyInt mult (Types.durToTime dur)

instance ToLily Rest where
    toLily (Rest typ prepend append) =
        tUnwords $ prepend ++ [toLily typ] ++ append

instance ToLily RestType where
    toLily r = case r of
        NormalRest dur -> "r" <> toLily dur
        HiddenRest dur -> "s" <> toLily dur
        FullMeasure dur mult -> "R" <> toLily dur <> "*" <> showt mult

-- ** Key

data Key = Key Text Mode deriving (Eq, Show)
type Mode = Text

instance Pretty Key where pretty = toLily
instance ToLily Key where
    toLily (Key tonic mode) = "\\key " <> tonic <> " \\" <> mode

parseKey :: Text -> Either Error Key
parseKey keyName = do
    key <- justErr ("unknown key: " <> keyName) $
        Twelve.lookupKey (Just (Pitch.Key keyName))
    (pc, acc) <- Types.parseDegree (Theory.keyTonic key)
    let tonic = toLily pc <> toLily acc
    mode <- justErr ("unknown mode: " <> Theory.keyName key) $
        Map.lookup (Theory.keyName key) modes
    pure $ Key tonic mode
    where
    modes = Map.fromList
        [ ("min", "minor"), ("locrian", "locrian"), ("maj", "major")
        , ("dorian", "dorian"), ("phrygian", "phrygian"), ("lydian", "lydian")
        , ("mixolydian", "mixolydian")
        ]

lookupKey :: Event -> ConvertM Key
lookupKey = lookupVal EnvKey.key parseKey defaultKey

defaultKey :: Key
defaultKey = Key "c" "major"

-- ** voice

-- | Each Ly list should be the same duration and have the same number of
-- barlines.
newtype Voices = Voices (VoiceMap Ly) deriving (Pretty, Show)

-- | Voices shouldn't be repeated, so this would be more appropriate as a
-- @Map Voice [a]@, but it turns out all the consumers work best with a list
-- so list it is.
type VoiceMap a = [(Voice, [a])]

data Voice = VoiceOne | VoiceTwo | VoiceThree | VoiceFour
    deriving (Eq, Ord, Show)

instance ToLily Voice where
    toLily v = case v of
        VoiceOne -> "\\voiceOne"; VoiceTwo -> "\\voiceTwo"
        VoiceThree -> "\\voiceThree"; VoiceFour -> "\\voiceFour"

instance Pretty Voice where pretty = showt

parseVoice :: Int -> Maybe Voice
parseVoice v = case v of
    1 -> Just VoiceOne; 2 -> Just VoiceTwo
    3 -> Just VoiceThree; 4 -> Just VoiceFour
    _ -> Nothing

eventVoice :: Event -> Maybe (Either Error Voice)
eventVoice event =
    eventContext . parse <$>
        Env.checkedVal2 EnvKey.voice (eventEnviron event)
    where
    parse (Left err) = Left err
    parse (Right voice) =
        justErr ("voice should be 1--4: " <> showt voice) $ parseVoice voice
    eventContext = first ((pretty event <> ": ")<>)

-- * Event

-- | Clip off the part of the event before the given time, or Nothing if it
-- was entirely clipped off.
clipEvent :: Time -> Event -> Maybe Event
clipEvent end e
    | left <= 0 = Nothing
    | otherwise = Just $
        e { eventStart = end, eventDuration = left, eventClipped = True }
    where left = eventEnd e - end
