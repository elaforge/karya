-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
-- | Convert Lilypond Events to lilypond code.
--
-- It's a terrible name, but what else am I supposed to call it?  Render?
-- Realize?  Perform?
module Perform.Lilypond.Process (
    process, convert_to_rests
    , parse_key
    , Ly(..), Note(..)
    , VoiceLy, Voices(..), Voice(..)
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

import qualified Util.Control as Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Derive.Attrs as Attrs
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.Stack as Stack
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types
       (Event(..), event_end, event_attributes, ToLily, to_lily, Time)
import qualified Perform.Pitch as Pitch

import Global
import Types


type Error = Text

-- | Automatically add lilypond code for certain attributes.
simple_articulations :: [(Attrs.Attributes, Code)]
simple_articulations =
    [ (Attrs.mute, "-+")
    -- Previously pizz<>right, comment on "Attrs".
    , (Attrs.pizz_right, "-+")
    , (Attrs.marcato, "-^")
    , (Attrs.staccato, "-.")
    , (Attrs.portato, "-_")
    , (Attrs.tenuto, "--")
    , (Attrs.accent, "->")
    , (Attrs.trem, ":32")
    ]

-- | Certain attributes are modal, in that they emit one thing when they
-- start, and another when they stop.
modal_articulations :: [(Attrs.Attributes, Code, Code)]
modal_articulations =
    [ (Attrs.pizz, "^\"pizz.\"", "^\"arco\"")
    , (Attrs.nv, "^\"nv\"", "^\"vib\"")
    , (Attrs.pont, "^\"sul pont.\"", "^\"loco\"")
    ]

-- * convert_to_rests

-- | Convert a staff to all rests, keeping the key, clef, and meter changes.
-- have predicates that recognize those, and keep those Codes
convert_to_rests :: [VoiceLy] -> [Ly]
convert_to_rests = hush . filter wanted . concatMap flatten
    where
    flatten (Left (Voices voices)) = case voices of
        [] -> []
        (_, lys) : _ -> lys
    flatten (Right ly) = [ly]
    wanted (LyCode code) = any (`Text.isPrefixOf` code)
        ["\\time ", "\\key ", "\\bar "]
    wanted _ = True
    has_duration (LyNote n) = Just [note_duration n]
    has_duration (LyRest r) =
        Just $ map Types.dur_to_note_dur $ Types.time_to_durs $ rest_time r
    has_duration _ = Nothing
    hush lys = -- TODO simplify durs
        map (LyRest . make_rest . HiddenRest) (concat durs) ++ case non_notes of
            ly : rest -> ly : hush rest
            [] -> []
        where (durs, non_notes) = Seq.span_while has_duration lys

-- * process

-- TODO remove
type VoiceLy = Either Voices Ly

data Chunk =
    ChunkNotes [FreeCode] [Event]
    | ChunkVoices [FreeCode] (VoiceMap Event)
    deriving (Show)

instance Pretty Chunk where
    format (ChunkNotes code events) = Pretty.constructor "ChunkNotes"
        [Pretty.format code, Pretty.format events]
    format (ChunkVoices code voices) = Pretty.constructor "ChunkVoices"
        [Pretty.format code, Pretty.format voices]

-- | This is the top level function which converts a stream of Events on one
-- staff into lilypond code.
--
-- . First pass finds voice boundaries.
-- . Then insert rests.
-- . Convert [Event]->[Ly]: keys, meter splitting, tuplet and tremolo.
-- . Merge 0 dur events 'FreeCode' in [Ly] since they have been split according
-- to the meter.
process :: Types.Config -> Time -> [Meter.Meter] -- ^ one for each measure
    -> [Event] -> Either Error [Either Voices Ly]
process config start meters events = do
    chunks <- collect_chunks events
    let end = start + sum (map Meter.measure_time meters)
    chunks <- return $ merge_note_code_chunks $
        insert_rests_chunks start end chunks
    let state = make_state config start meters default_key
    key <- maybe (return default_key)
        (fmap fst . run_convert state . lookup_key) (Seq.head events)
    state <- return $ state { state_key = key }
    (lys, _) <- run_convert state $
        error_context ("start: " <> pretty start) $ convert chunks
    let meter = fromMaybe Meter.default_meter (Seq.head meters)
    return $ Right (LyCode $ "\\time " <> to_lily meter)
        : Right (LyCode $ to_lily key) : lys

-- | Group voice and non-voice Events together into Chunks.
collect_chunks :: [Event] -> Either Error [Chunk]
collect_chunks = go
    where
    go [] = return []
    go events = do
        (no_voice, code1, events) <- without_voice events
        (voice, code2, events) <- with_voice events
        let collected
                | null no_voice = [ChunkVoices (code1++code2) voice]
                | otherwise =
                    [ChunkNotes code1 no_voice, ChunkVoices code2 voice]
        (filter nonempty collected ++) <$> go events
    nonempty (ChunkNotes [] []) = False
    nonempty (ChunkVoices [] []) = False
    nonempty _ = True
    with_voice events = do
        (voice, code, remain) <- collect_voices events
        let tails = mapMaybe (Seq.last . snd) voice
        whenJust (Seq.head remain) $ \e ->
            whenJust (List.find (Types.event_overlaps e) tails) $ \over ->
                Left $ "last voice " <> pretty over
                    <> " overlaps first non-voice " <> pretty e
        return (voice, code, remain)
    without_voice events = do
        let (without, remain) = span ((==Nothing) . event_voice) events
        whenJust ((,) <$> Seq.last without <*> Seq.head remain) $ \(e1, e2) ->
            when (Types.event_overlaps e1 e2) $
                Left $ "last non-voice " <> pretty e1
                    <> " overlaps first voice " <> pretty e2
        let (code, notes) = Seq.partition_on free_code without
        return (notes, code, remain)

insert_rests_chunks :: Time -> Time -> [Chunk] -> [Chunk]
insert_rests_chunks start end = Then.mapAccumL insert start final
    where
    insert !t (ChunkNotes code events) =
        ChunkNotes code <$> insert_rests Nothing t events
    insert !t (ChunkVoices code voices) = case get_end voices of
        Nothing -> (t, ChunkVoices code [])
        Just end -> (end, ChunkVoices code voices2)
            where
            voices2 = map (second (snd . insert_rests (Just end) t)) voices
    get_end = Seq.maximum . map event_end . mapMaybe (Seq.last . snd)
    final t
        | t < end = [ChunkNotes [] [rest_event t (end-t)]]
        | otherwise = []

-- | Fill gaps between events with explicit rests.  Zero duration note code
-- events have the effect of splitting up the rests.
insert_rests :: Maybe Time -> Time -> [Event] -> (Time, [Event])
insert_rests maybe_end = go
    where
    go t [] = case maybe_end of
        Just end | end > t -> (end, [rest_event t (end - t)])
        _ -> (t, [])
    go t (event : events) =
        ((rest ++ event : here) ++) <$>
            go (max t (maximum (map event_end (event : here)))) there
        where
        rest = if gap <= 0 then [] else [rest_event t gap]
        (here, there) = span ((<= event_start event) . event_start) events
        gap = event_start event - t

rest_event :: Time -> Time -> Event
rest_event start dur = Event
    { event_start = start
    , event_duration = dur
    , event_pitch = Nothing
    , event_instrument = ScoreTypes.empty_instrument
    , event_environ = mempty
    , event_stack = Stack.empty
    , event_clipped = False
    }

merge_note_code_chunks :: [Chunk] -> [Chunk]
merge_note_code_chunks = map merge
    where
    merge (ChunkNotes code events) = ChunkNotes code (merge_note_code events)
    merge (ChunkVoices code voices) =
        ChunkVoices code (map (second merge_note_code) voices)

-- This has to be done after rests are present, so it can attach to rests.
merge_note_code :: [Event] -> [Event]
merge_note_code = go
    where
    go [] = []
    go (event : events) = case nonzero of
        p : ps -> add_note_code codes p : ps ++ go there
        [] -> go there -- TODO warn about dropped zero events
        -- TODO also warn about zero dur events with no code
        where
        (here, there) = span ((<= event_start event) . event_start) events
        (zero, nonzero) = List.partition code_event (event : here)
        codes = concatMap (Constants.environ_code . event_environ) zero
    -- TODO subdivision events are a special case of zero-dur events which
    -- are not code, but are directives to the meter splitting.  I should
    -- handle these at the top level, like meter events themselves.
    code_event e = zero_dur e && lookup_subdivision e == Nothing

add_note_code :: [(Constants.CodePosition, Text)] -> Event -> Event
add_note_code codes event =
    event { event_environ = foldr merge (event_environ event) codes }
    where
    merge (pos, code) env = case Env.maybe_val k env of
        Nothing -> Env.insert_val k code env
        Just old -> Env.insert_val k (old <> " " <> code) env
        where k = Constants.position_key pos

-- * convert

-- TODO maybe return Either Voices [Ly] so I can avoid the concat and all the
-- extra Rights?
convert :: [Chunk] -> ConvertM [Either Voices Ly]
convert = fmap concat . Then.mapM go (return [[Right final_barline]])
    where
    final_barline = LyCode "\\bar \"|.\""
    go :: Chunk -> ConvertM [Either Voices Ly]
    go c = do
        start <- State.gets state_time
        case c of
            ChunkNotes code events ->
                map Right . merge_free_code start code <$>
                    until_complete (convert_chunk True) events
            ChunkVoices code voices ->
                merge_free_code_voices start code . simplify_voices <$>
                    voices_to_ly voices

merge_free_code_voices :: Time -> [FreeCode] -> [Either Voices Ly]
    -> [Either Voices Ly]
merge_free_code_voices = go
    where
    go _ codes [] = map (Right . LyCode . snd) (concatMap snd codes)
    go start codes (Left (Voices ((v, v1_lys) : voices)) : lys) =
        Left (Voices ((v, merged) : voices)) : go end codes_remain lys
        where
        (merged, (end, codes_remain)) = merge_free_code1 start codes v1_lys
    go start codes lys = map Right merged ++ go end codes_remain remain
        where
        (non_voice, remain) = Seq.span_while (either (const Nothing) Just) lys
        (merged, (end, codes_remain)) = merge_free_code1 start codes non_voice

-- | Mix code events into the Lys, depending on their prepend or append attrs.
merge_free_code :: Time -> [FreeCode] -> [Ly] -> [Ly]
merge_free_code start codes lys =
    merged ++ map (LyCode . snd) (concatMap snd codes_remain)
    where (merged, (_end, codes_remain)) = merge_free_code1 start codes lys

-- TODO more efficient but repetitious
-- merge_free_code start codes lys = go codes $ with_starts start lys
--     where
--     go codes [] = map (LyCode . snd) (concatMap snd codes)
--     go codes ((start, ly) : lys) = applied ++ go rest_code lys
--         where (applied, rest_code) = apply_free_code start codes ly

-- | Merge in the FreeCodes that overlap with the Ly start times.  Return the
-- unconsumed one and end time.
merge_free_code1 :: Time -> [FreeCode] -> [Ly] -> ([Ly], (Time, [FreeCode]))
merge_free_code1 start codes = go codes . with_starts start
    where
    go codes [] = ([], (start, codes))
    go codes ((start, ly) : lys) = case ly of
        _ | null here -> continue ly codes
        LyBarline {} -> continue ly codes
        LyCode {} -> continue ly codes
        _ -> case apply_free_code (concatMap snd here) ly of
            -- TODO I couldn't find anything to attach to, so I should drop it
            -- and emit a warning instead of just attaching it to the next one.
            Nothing -> continue ly codes
            Just applied -> continue applied there
        where
        (here, there) = span ((<end) . fst) codes
        end = start + ly_duration ly
        continue ly cs = first (ly:) (go cs lys)

apply_free_code :: [(Constants.FreeCodePosition, Code)] -> Ly -> Maybe Ly
apply_free_code codes ly = case ly of
    LyNote note -> Just $ LyNote $ note
        { note_prepend = prepend ++ note_prepend note
        , note_append = note_append note ++ append
        }
    LyRest rest -> Just $ LyRest $ rest
        { rest_prepend = prepend ++ rest_prepend rest
        , rest_append = rest_append rest ++ append
        }
    LyNested nested -> do
        lys <- apply_nested $ NonEmpty.toList (nested_contents nested)
        lys <- NonEmpty.nonEmpty lys
        return $ LyNested $ nested { nested_contents = lys }
    _ -> Nothing
    where
    (prepend, append) = (map snd *** map snd) $
        List.partition ((==Constants.FreePrepend) . fst) codes
    -- Apply to the first place that takes it, or Nothing if there was none.
    apply_nested [] = Nothing
    apply_nested (ly : lys) = case apply_free_code codes ly of
        Nothing -> (ly:) <$> apply_nested lys
        Just applied -> Just $ applied : lys

type FreeCode = (Time, [(Constants.FreeCodePosition, Code)])

free_code :: Event -> Maybe FreeCode
free_code event
    | null code = Nothing
    | otherwise = Just (event_start event, code)
    where code = Constants.environ_free_code (event_environ event)

until_complete :: Monad m => ([a] -> m ([b], [a])) -> [a] -> m [b]
until_complete f = go
    where
    go [] = return []
    go as = do
        (bs, as) <- f as
        (bs++) <$> go as

-- ** convert_chunk

-- | Convert the rests for the first event, and a single slice of time.
-- If notes had to be split and tied, they are put back into the remaining
-- events.
-- TODO the name should change, now that Chunk is something else
convert_chunk :: Bool -> [Event] -> ConvertM ([Ly], [Event])
convert_chunk _ [] = return ([], [])
convert_chunk metered (event : events) = error_context (pretty event) $ if
    | Just ((score_dur, real_dur), (_, remain))
            <- find Constants.get_tuplet -> do
        score_dur <- real_to_time score_dur
        real_dur <- real_to_time real_dur
        convert_tuplet start score_dur real_dur remain
    | Just ((), (event, remain)) <- find has_tremolo ->
        convert_tremolo event remain
    | otherwise -> convert_chord metered (event :| events)
    where
    has_tremolo env
        | Env.is_set Constants.v_tremolo env = Just ()
        | otherwise = Nothing
    start = event_start event
    find match = find_here start (match . event_environ) (event : events)

find_here :: Time -> (Event -> Maybe a) -> [Event]
    -> Maybe (a, (Event, [Event]))
find_here start match events = msum $ map find (focus here)
    where
    find (e, es) = (, (e, es ++ later)) <$> match e
    (here, later) = span ((<=start) . event_start) events

-- | Focus on each element in turn, removing it from the list.
focus :: [a] -> [(a, [a])]
focus (x:xs) = (x, xs) : map (second (x:)) (focus xs)
focus [] = []

lookup_env :: Typecheck.Typecheck a => Env.Key -> Event -> Maybe a
lookup_env k = Env.maybe_val k . event_environ

lookup_subdivision :: Event -> Maybe Text
lookup_subdivision = lookup_env Constants.v_subdivision

get_code :: Env.Key -> Event -> Code
get_code k = fromMaybe "" . lookup_env k

zero_dur :: Event -> Bool
zero_dur = (==0) . event_duration

-- ** convert_tuplet

-- | Collect the notes inside the duration, run a special 'convert_chunk' on
-- them where the meter is ok with any duration, then wrap that in \tuplet,
-- and increment time by duration * 3/2.
convert_tuplet :: Time
    -> Time -- ^ score duration of notes of the tuplet
    -> Time
    -> [Event] -- ^ extract the overlapped events and render in the tuplet
    -> ConvertM ([Ly], [Event])
convert_tuplet start score_dur real_dur events = do
    let (in_tuplet, out) = span ((< start + score_dur) . event_start) events
    old <- State.get

    -- The usual convention for tuplets is that the notes have longer values
    -- but are shortened to fit, e.g. 3/8 into the time of 2/8.  But duplets
    -- are the opposite, they are shorter and are made longer, e.g. 2/8 in the
    -- time of 3/8.  Don't ask me why, that's just the convention.  I always
    -- get shorter notes so they fit under the parent tuplet event, so for
    -- triplet 1/8s, I get score_dur = 3/16, real_dur = 2/8, then double to
    -- score_dur = 3/8s.
    let is_duplet = Ratio.numerator (Types.to_whole score_dur) == 1
    let factor = if is_duplet then 1 else 2
    score_dur <- return $ if is_duplet then score_dur else score_dur * 2
    when (real_dur <= 0) $
        throw $ "tuplet with a real_dur of 0: "
            <> pretty (start, score_dur, real_dur)
    let divisor = realToFrac score_dur / realToFrac real_dur :: Rational
    -- This probably means the notes have been stretched or something and
    -- aren't on simple divisions.
    when (Ratio.numerator divisor > 15 || Ratio.denominator divisor > 15) $
        throw $ "tuplet factor is too complicated: " <> showt score_dur
            <> "/" <> showt real_dur

    -- TODO it's probably wrong to do this unmetered.  I should instead act as
    -- if the meter has changed.
    lys <- until_complete (convert_chunk False) $
        snd $ insert_rests (Just (start + score_dur)) start $
        map (stretch factor start) in_tuplet

    -- Rewind time back to before the tuplet.
    State.modify' $ \state -> state
        { state_time = state_time old
        , state_meters = state_meters old
        , state_measure_start = state_measure_start old
        , state_measure_end = state_measure_end old
        }
    barline <- Control.rethrow ("converting tuplet: "<>) $
        advance_measure (start + real_dur)
    let code = tuplet_code (count_notes_rests lys) divisor
    return (code : lys ++ [LyCode "}"] ++ maybe [] (:[]) barline, out)

-- | Convention is that the number on the tuplet is at least the number of
-- notes inside, but lilypond doesn't do that automatically.
tuplet_code :: Int -> Rational -> Ly
tuplet_code notes r =
    LyCode $ "\\tuplet " <> showt num <> "/" <> showt denom <> " {"
    where
    (num, denom) = (Ratio.numerator r * factor, Ratio.denominator r * factor)
    factor = (2^) $ max 0 $ ceiling $ logBase 2 $
        fromIntegral notes / fromIntegral (Ratio.numerator r)

real_to_time :: RealTime -> ConvertM Time
real_to_time t = do
    quarter <- State.gets $ Types.config_quarter_duration . state_config
    return $ Types.real_to_time quarter t

stretch :: Int -> Time -> Event -> Event
stretch factor start event
    | factor == 1 = event
    | otherwise = event
        { event_start =
            fromIntegral factor * (event_start event - start) + start
        , event_duration = fromIntegral factor * event_duration event
        }

-- ** convert_tremolo

-- | Get just pitch and code from in_tremolo notes, force them to 32nd note
-- duration, wrap in (), and wrap the whole thing in \repeat tremolo { ... }.
--
-- TODO This is sort of an ad-hoc reimplementation of 'convert_chord', which is
-- grody.  That means I have to handle all the zero dur and code all over
-- again, and maybe get it wrong or inconsistent.  But the rules are
-- different in a tremolo, duration-wise it's like a chord, but code always has
-- to go on the notes inside, not after the whole thing.
convert_tremolo :: Event -> [Event] -> ConvertM ([Ly], [Event])
convert_tremolo tremolo_event events = do
    dur <- get_allowed_dur
    let (in_tremolo, out) = span ((< start + dur) . event_start) events
    in_tremolo <- consume_subdivisions in_tremolo
    prev_attrs <- State.gets state_prev_attrs
    let (next_attrs, notes) = tremolo_notes prev_attrs in_tremolo
    State.modify' $ \state -> state { state_prev_attrs = next_attrs }

    -- TODO promote this to the caller so everyone doesn't have to do it?
    barline <- advance_measure (start + dur)

    let (times, frac) = properFraction $ Types.to_whole dur * 16
    when (frac /= 0) $ throw $ "dur " <> pretty dur
        <> " doesn't yield an integral number of 16th notes: "
        <> pretty (Types.to_whole dur * 16)
    let (prepend, append) =
            events_note_code Constants.Chord True True in_tremolo
        (nprepend, nappend) =
            events_note_code Constants.Note True True in_tremolo

    let clipped
            | dur >= total_dur = []
            | otherwise = mapMaybe (clip_event (start + dur))
                (tremolo_event : in_tremolo)
    notes <- maybe (throw "no notes in tremolo") return $
        NonEmpty.nonEmpty notes
    let note = tremolo_note dur times notes
            (prepend ++ nprepend) (nappend ++ append)
    return (LyNested note : maybe [] (:[]) barline, clipped ++ out)

    where
    start = event_start tremolo_event
    total_dur = event_duration tremolo_event
    get_allowed_dur = do
        meter <- get_subdivision
        measure_start <- State.gets state_measure_start
        return $ Types.note_dur_to_time $ Meter.allowed_duration use_dot meter
            (start - measure_start)
            (start - measure_start + event_duration tremolo_event)
        where use_dot = True

tremolo_note :: Time -> Int -> NonEmpty Note -> [Code] -> [Code] -> Nested
tremolo_note dur times (note :| notes) prepend append = Nested
    { nested_prefix = "\\repeat tremolo " <> showt times <> " {"
    , nested_contents = fmap LyNote (with_code :| notes)
    , nested_suffix = "}"
    , nested_duration = dur
    }
    where
    with_code = note
        { note_prepend = prepend ++ note_prepend note
        , note_append = note_append note ++ append
        }

tremolo_notes :: Attrs.Attributes -> [Event] -> (Attrs.Attributes, [Note])
tremolo_notes prev_attrs = List.mapAccumL make prev_attrs . zip_first_last
    where
    make prev_attrs (is_first, event, is_last) = (next_attrs,) $ Note
        { note_pitches =
            NotePitch (pitch_code is_first event) NoTie slur :| []
        , note_duration = Types.NoteDuration Types.D32 False
        , note_prepend = []
        , note_append = attrs_codes
        , note_stack = Seq.last $ Stack.to_ui (event_stack event)
        }
        where
        slur
            | is_first && not is_last = ["("]
            | not is_first && is_last = [")"]
            | otherwise = []
        (attrs_codes, next_attrs) =
            attrs_to_code prev_attrs (event_attributes event)

zip_first_last :: [a] -> [(Bool, a, Bool)]
zip_first_last = map to_bool . Seq.zip_neighbors
    where
    to_bool (prev, cur, next) =
        (Maybe.isNothing prev, cur, Maybe.isNothing next)


-- ** convert_chord

-- | This is the lowest level of conversion.  It converts a vertical slice of
-- notes starting at the first event, and returns the rest.
convert_chord :: Bool -> NonEmpty Event -> ConvertM ([Ly], [Event])
convert_chord metered events = do
    key <- lookup_key (NonEmpty.head events)
    state <- State.get
    let key_change = [LyCode (to_lily key) | key /= state_key state]
    meter <- if metered then Just <$> get_subdivision else return Nothing
    config <- State.gets state_config
    let (here, there) = break
            ((> event_start (NonEmpty.head events)) . event_start)
            (NonEmpty.tail events)
        next = event_start <$> List.find (not . zero_dur) there
    -- TODO if I handle subdivisions at the top I can get rid of this
    here <- consume_subdivisions (NonEmpty.head events : here)
    case NonEmpty.nonEmpty here of
        Nothing -> return ([], there)
        Just here -> do
            let (chord_ly, end, last_attrs, clipped) = make_note
                    config (state_measure_start state) (state_prev_attrs state)
                    meter here next
            barline <- if metered then advance_measure end
                else advance_unmetered end >> return Nothing

            -- Lilypond will throw a barcheck error and produce broken score,
            -- and 'convert_tuplet' rewinds I'll get an extra barline, so
            -- disallow this.
            -- unless metered $ whenJust barline $ const $
            --     throw $ "An unmetered note went past a barline. This\
            --         \ probably means a tuplet tried to go over a barline: "
            --         <> pretty end
            State.modify' $ \state -> state
                { state_key = key
                , state_prev_attrs = last_attrs
                }
            return
                ( key_change <> [chord_ly] <> maybe [] (:[]) barline
                , clipped ++ there
                )

-- | Handle any Constants.v_subdivision events and filter them out.
consume_subdivisions :: [Event] -> ConvertM [Event]
consume_subdivisions events = mapM_ update subdivisions >> return normal
    where
    (subdivisions, normal) = Seq.partition_on lookup_subdivision events
    update "" = State.modify' $ \state ->
        state { state_subdivision = Nothing }
    update m = do
        meter <- tryRight $ first
            (("can't parse meter in " <> pretty Constants.v_subdivision
                <> ": " <> showt m <> ": ")<>)
            (Meter.parse_meter m)
        State.modify' $ \state -> state { state_subdivision = Just meter }

make_note :: Types.Config -> Time -> Attrs.Attributes
    -- ^ Previous note's Attributes, to track 'modal_articulations'.
    -> Maybe Meter.Meter
    -> NonEmpty Event -- ^ Events that occur at the same time.
    -> Maybe Time -- ^ start of the next note
    -> (Ly, Time, Attrs.Attributes, [Event]) -- ^ (note, note end time, clipped)
make_note config measure_start prev_attrs maybe_meter chord next =
    (ly, allowed_end, next_attrs, clipped)
    where
    -- If the time is a full measure, then it uses allowed_time, not
    -- allowed_dur.
    ly = case NonEmpty.nonEmpty note_pitches of
        Nothing -> LyRest $ Rest
            { rest_type = case maybe_meter of
                Just meter | full_measure_rest ->
                    FullMeasure (Meter.meter_denom meter) (Meter.time_num meter)
                _ -> NormalRest allowed_dur
            , rest_prepend = prepend_chord
            , rest_append = append_chord
            }
        Just pitches -> LyNote $ Note
            { note_pitches = pitches
            , note_duration = allowed_dur
            , note_prepend = prepend_chord
            , note_append = append_chord ++ attrs_codes
            , note_stack = Seq.last $ Stack.to_ui $ event_stack $
                NonEmpty.head chord
            }
    note_pitches = do
        -- Sorting by pitch puts the chord notes in a predictable order.  Some
        -- lilypond notation, such as glissandoMap, refers to chord notes by
        -- index.
        event <- Seq.sort_on event_pitch $ NonEmpty.toList chord
        let pitch = pitch_code (is_first event) event
        guard $ not (Text.null pitch)
        let tie = note_tie event
        let (prepend, append) = event_note_code Constants.Note
                (is_first event) (is_last event) event
        return $ NotePitch pitch tie append
    (attrs_codes, next_attrs) = attrs_to_code prev_attrs $
        mconcat $ map event_attributes $ NonEmpty.toList chord

    (prepend_chord, append_chord) =
        (Seq.unique . concat *** Seq.unique . concat) $ unzip
            [ event_note_code Constants.Chord (is_first e) (is_last e) e
            | e <- NonEmpty.toList chord
            ]

    note_tie event
        | event_end event <= allowed_end = NoTie
        | Text.null direction = TieNeutral
        | direction == "^" = TieUp
        | otherwise = TieDown
        where
        direction :: Text
        direction = get_code Constants.v_tie_direction event
    is_first = not . event_clipped
    is_last event = case note_tie event of
        NoTie -> True
        _ -> False

    clipped = mapMaybe (clip_event allowed_end) (NonEmpty.toList chord)
    start = event_start (NonEmpty.head chord)

    -- FullMeasure rests are special because they can have a non-Duration
    -- time.
    allowed_end = start + allowed_time
    (full_measure_rest, allowed_time) = case maybe_meter of
        Just meter | null note_pitches && start == measure_start
                && max_end - start >= Meter.measure_time meter ->
            (True, Meter.measure_time meter)
        _ -> (False, Types.note_dur_to_time allowed_dur)
    allowed_dur = case maybe_meter of
        Nothing -> Types.time_to_note_dur (max_end - start)
        Just meter ->
            Meter.allowed_duration use_dot meter in_measure (max_end - start)
            where
            is_rest = null note_pitches
            -- Dots are always allowed for non-binary meters.
            use_dot = not is_rest || Types.config_dotted_rests config
                || not (Meter.is_binary meter)
            in_measure = start - measure_start
    -- Maximum end, the actual end may be shorter since it has to conform to
    -- a Duration.
    max_end = min_if next $ Seq.ne_minimum (fmap event_end chord)

events_note_code :: Constants.Attach -> Bool -> Bool -> [Event]
    -> ([Text], [Text])
events_note_code attach is_first is_last =
    (Seq.unique . concat *** Seq.unique . concat)
    . unzip . map (event_note_code attach is_first is_last)

event_note_code :: Constants.Attach -> Bool -> Bool -> Event -> ([Text], [Text])
event_note_code attach is_first is_last =
    extract . Constants.environ_code . event_environ
    where
    extract codes =
        (get Constants.Prepend codes, get Constants.Append codes)
    get pos codes =
        [ code
        | (Constants.CodePosition a p d, code) <- codes
        , a == attach, p == pos, d `elem` dists
        ]
    dists = Constants.All : [Constants.First | is_first]
        ++ [Constants.Last | is_last]

min_if :: Ord a => Maybe a -> a -> a
min_if ma b = maybe b (min b) ma

-- ** ly code env vars

t_unwords :: [Text] -> Text
t_unwords = Text.unwords . filter (not . Text.null) . map Text.strip

pitch_code :: Bool -> Event -> Code
pitch_code is_first event =
    maybe "" to_lily (event_pitch event)
        <> if is_first then get_code Constants.v_append_pitch event else ""

attrs_to_code :: Attrs.Attributes -> Attrs.Attributes
    -> ([Code], Attrs.Attributes)
    -- ^ (code to append, prev attrs for the next note)
attrs_to_code prev current =
    (simple ++ starts ++ Maybe.catMaybes ends, mconcat (current : extras))
    where
    starts =
        [ start
        | (attr, start, _) <- modal_articulations
        , current `has` attr, not (prev `has` attr)
        ]
    (ends, extras) = unzip $ map (cancel prev) modal_articulations
    simple = [code | (attr, code) <- simple_articulations, current `has` attr]
    cancel prev (attr, _, end)
        -- If prev doesn't have +nv, but this one is +staccato, then consider
        -- that this one also has +nv.  This avoids spurious vib marks on
        -- every staccato note.
        | prev `has` attr && not (current `has` attr) =
            if attr == Attrs.nv && any (current `has`) inherently_nv
                then (Nothing, Attrs.nv)
                else (Just end, mempty)
        | otherwise = (Nothing, mempty)
    inherently_nv = [Attrs.staccato, Attrs.harm, Attrs.pizz]
    has = Attrs.contain

-- ** collect_voices

-- | Span events until they don't have a 'Constants.v_voice' val.
--
-- I used to mix the code into the first voice here, but 'simplify_voices' may
-- then get rid of it.  So return the code events and mix them in after
-- simplification.
collect_voices :: [Event] -> Either Text (VoiceMap Event, [FreeCode], [Event])
collect_voices events = do
    let (voice, code, remain) = span_voices events
    with_voice <- forM voice $ \(err_or_voice, event) ->
        (, event) <$> err_or_voice
    return $ case Seq.group_fst with_voice of
        [] -> ([], [], events)
        voices -> (voices, code, remain)

-- | Strip off events with voices.  Any FreeCode events mixed in won't break
-- the voice span, and are returned separately.
span_voices :: [Event] -> ([(Either Error Voice, Event)], [FreeCode], [Event])
span_voices events = (concat voice, concat code, remain)
    where
    ((voice, code), remain) = first unzip $
        span_xy (\e -> (, e) <$> event_voice e) free_code events
    -- Previously I tried to only split voices where necessary by only spanning
    -- overlapping notes, or notes with differing voices.  But even when it
    -- worked as intended, joining voices this aggressively led to oddities
    -- because it would turn any two voices with the same duration notes into
    -- a chord.  So now I simplify voices only at the measure level, in
    -- 'simplify_voices'.

-- | Span xs, then span ys.  Only span ys if there are subsequent xs.
-- So e.g. if xs is letters and ys is spaces, it will get words up until it hits
-- digits, but not span the final spaces.
span_xy :: (a -> Maybe x) -> (a -> Maybe y) -> [a] -> ([([x], [y])], [a])
span_xy get_x get_y = go
    where
    go as
        | null xs && null ys = ([], as)
        | null spanned && not (null remain) = ([(xs, [])], as1)
        | otherwise = ((xs, ys) : spanned, remain)
        where
        (xs, as1) = Seq.span_while get_x as
        (ys, as2) = Seq.span_while get_y as1
        (spanned, remain) = go as2

event_context :: Event -> Either Error a -> Either Error a
event_context event = first ((pretty event <> ": ")<>)

-- ** voices_to_ly

-- | Like 'convert_chunk', but converts within a voice, which means no nested
-- voices are expected.
voices_to_ly :: VoiceMap Event -> ConvertM (VoiceMap Ly)
voices_to_ly [] = return []
voices_to_ly voices = do
    state <- State.get
    (states, voice_lys) <- unzip <$> mapM (convert state) voices
    case states of
        st : sts -> do
            unless (all ((== state_time st) . state_time) sts) $
                throw $ "inconsistent states after voices: " <> pretty (st:sts)
            State.put st
        [] -> return ()
    return voice_lys
    where
    convert state (v, events) = do
        (measures, final_state) <- either throw return $ run_convert state $
            until_complete (convert_chunk True) events
        return (final_state, (v, measures))

-- | If a whole measure of a voice is empty, omit the voice for that measure.
--
-- Previously, I tried a more fine-grained approach, documented in
-- 'span_voices'.  This way is more complicated because it has to operate on
-- Lys since it needs to know where the measure boundaries are, but gives much
-- better results.
simplify_voices :: VoiceMap Ly -> [VoiceLy]
simplify_voices voices =
    concatMap (flatten . strip) $ split_voices_at rest_starts voices
    where
    rest_starts = Seq.drop_dups id $ Seq.merge_lists id $
        concatMap (rests_at . snd) voices
    rests_at lys =
        [ [start, start + ly_duration ly]
        | (start, ly) <- with_starts 0 lys, is_full_measure_rest ly
        ]
    is_full_measure_rest (LyRest (Rest { rest_type = FullMeasure {} })) = True
    is_full_measure_rest _ = False

    -- Strip out voices that consist entirely of rests, keeping at least one.
    strip voices = case filter (not . rest_measure . snd) voices of
            [] -> take 1 voices
            voices -> voices
        where
        rest_measure = all $
            \ly -> is_full_measure_rest ly || ly_duration ly == 0
    flatten :: VoiceMap Ly -> [VoiceLy]
    flatten [] = []
    flatten [(_, lys)] = map Right lys
    flatten voices = [Left (Voices voices)]

-- | Split voices every place any of them has a full measure rest.
--
-- > [(1, xs), (2, ys)] -> [(1, x), (2, y)], [(1, x), (2, y)]
-- > [(1, xs), (2, ys)] -> [(1, [xs]), (2, [ys])]
split_voices_at :: [Time] -> VoiceMap Ly -> [VoiceMap Ly]
split_voices_at ts = rotate . map (second (split_at ts))
    where
    -- This is really hard for me to understand, but I don't need to because
    -- the types work out :)
    -- [(1, [x, y]), (2, [a, b])] -> [[(1, x), (2, a)], [(1, y), (2, b)]]
    -- split_at should produce a [Ly] group for every split Time, but if it
    -- doesn't the rotate will drop all the other voices.
    rotate :: VoiceMap [Ly] -> [VoiceMap Ly]
    rotate voice_groups = map (zip voices) (Seq.rotate lys)
        where (voices, lys) = unzip voice_groups
    -- Ly times should always line up at measure boundaries, and the split
    -- times should all be at measure boundaries.  So this should return one
    -- [Ly] for each Time.
    split_at :: [Time] -> [Ly] -> [[Ly]]
    split_at times lys = go times $ zip (drop 1 $ ly_start_times 0 lys) lys
        where
        go _ [] = []
        go [] rest = [map snd rest]
        go (t:ts) lys = map snd pre : go ts post
            where (pre, post) = span ((<=t) . fst) lys

with_starts :: Time -> [Ly] -> [(Time, Ly)]
with_starts start lys = zip (ly_start_times start lys) lys

-- | drop 1 for end times.
ly_start_times :: Time -> [Ly] -> [Time]
ly_start_times start = scanl (+) start . map ly_duration

-- * misc

-- | Advance now to the given time, up to and including the end of the measure,
-- but it's an error to try to go past.  Return Ly with a LyBarline if this is
-- a new measure.
--
-- If I wanted to emit Barlines automatically I'd have to collect the output
-- [Ly] in the State, which I'd then need to parameterize since it can be
-- [VoiceLy] too.
advance_measure :: Time -> ConvertM (Maybe Ly)
advance_measure time = advance =<< State.get
    where
    advance state
        | time < state_time state =
            throw $ "can't advance time backward: " <> pretty time
                <> " < " <> pretty (state_time state)
        | time < state_measure_end state = do
            State.put $ state { state_time = time }
            return Nothing
        | time == state_measure_end state =
            case state_meters state of
                prev_meter : meters -> advance1 prev_meter meters
                _ -> throw $ "out of meters, can't advance time to "
                    <> pretty time
        | otherwise =
            throw $ "can't advance time past barline: " <> pretty time
                <> " > " <> pretty (state_measure_end state)
    advance1 prev_meter meters = do
        State.modify' $ \state -> state
            { state_meters = meters
            , state_measure_start = state_measure_end state
            , state_measure_end = state_measure_end state
                + Meter.measure_time (fromMaybe prev_meter (Seq.head meters))
            , state_time = time
            }
        return $ case Seq.head meters of
            Just meter
                | to_lily prev_meter == to_lily meter ->
                    Just (LyBarline Nothing)
                | otherwise -> Just (LyBarline (Just meter))
            _ -> Nothing

-- | Advance time without regard to meter or barlines.
advance_unmetered :: Time -> ConvertM ()
advance_unmetered time = advance =<< State.get
    where
    advance state
        | time < state_time state =
            throw $ "can't advance unmetered time backward: " <> pretty time
                <> " < " <> pretty (state_time state)
        | otherwise = State.put $ state { state_time = time }

-- | Get the current subdivision and check it against the meter.  This is a way
-- to override the meter for the purposes of how durations are spelled.
get_subdivision :: ConvertM Meter.Meter
get_subdivision = do
    meters <- State.gets state_meters
    meter <- tryJust "out of meters" $ Seq.head meters
    subdivision <- State.gets state_subdivision
    case subdivision of
        Just sub
            | Meter.measure_time meter == Meter.measure_time sub -> return sub
            | otherwise -> throw $ "subdivision " <> to_lily sub
                <> " incompatible with meter " <> to_lily meter
        Nothing -> return meter

-- * ConvertM

run_convert :: State -> ConvertM a -> Either Error (a, State)
run_convert state = Identity.runIdentity . Except.runExceptT
    . flip State.runStateT state

type ConvertM = State.StateT State (Except.ExceptT Error Identity.Identity)

error_context :: Error -> ConvertM a -> ConvertM a
error_context msg = map_error ((msg <> ": ") <>)

map_error :: (Error -> Error) -> ConvertM a -> ConvertM a
map_error f action = Except.catchError action $ \err ->
    Except.throwError (f err)

data State = State {
    -- Constant:
    state_config :: !Types.Config

    -- Changes on each measure:
    -- | One Meter for each expected measure in the output.
    -- The head of the list is the current meter.  It's valid for the meters to
    -- be [] as long as you don't have any more notes or rests to generate.
    , state_meters :: ![Meter.Meter]
    , state_measure_start :: !Time
    , state_measure_end :: !Time

    -- Changes on each note:
    -- | Current position in time, aka the end of the previous note.
    , state_time :: !Time
        -- | Used in conjunction with 'modal_articulations'.
    , state_prev_attrs :: Attrs.Attributes
    , state_key :: !Key

    -- Changes on a directive.
    , state_subdivision :: !(Maybe Meter.Meter)
    } deriving (Show)

instance Pretty State where
    format (State config meters mstart mend time prev_attrs key subdiv) =
        Pretty.record "State"
            [ ("config", Pretty.format config)
            , ("meters", Pretty.format meters)
            , ("measure_start", Pretty.format mstart)
            , ("measure_end", Pretty.format mend)
            , ("time", Pretty.format time)
            , ("prev_attrs", Pretty.format prev_attrs)
            , ("key", Pretty.format key)
            , ("subdivision", Pretty.format subdiv)
            ]

make_state :: Types.Config -> Time -> [Meter.Meter] -> Key -> State
make_state config start meters key = State
    { state_config = config
    , state_meters = meters
    , state_measure_start = start
    , state_measure_end = start + maybe 0 Meter.measure_time (Seq.head meters)
    , state_time = start
    , state_prev_attrs = mempty
    , state_key = key
    , state_subdivision = Nothing
    }

-- ** util

throw :: Error -> ConvertM a
throw msg = do
    now <- State.gets state_time
    Except.throwError $ pretty now <> ": " <> msg

lookup_val :: Env.Key -> (Error -> Either Error a) -> a -> Event -> ConvertM a
lookup_val key parse deflt event = prefix $ do
    maybe_val <- Env.checked_val key (event_environ event)
    maybe (Right deflt) parse maybe_val
    where
    prefix = either (throw . ((pretty key <> ": ") <>)) return


-- * types

-- | Ultimately, the contants of Ly is just a bit of lilypond code.  They are
-- all converted to text via 'to_lily' and concatenated to form the full score.
-- But I encode a fair amount of structure into the data type, which is
-- convenient for debugging.  It could also could theoretically be further
-- modified, though I don't think I ever do that.  However, 'ly_duration' at
-- least is used to merge 'FreeCode' into a Ly stream.
data Ly =
    LyNote !Note
    | LyRest !Rest
    | LyNested Nested
    | LyBarline !(Maybe Meter.Meter)
    | LyCode !Code
    deriving (Show)

ly_duration :: Ly -> Time
ly_duration ly = case ly of
    LyNote note -> Types.note_dur_to_time (note_duration note)
    LyRest rest -> rest_time rest
    LyNested n -> nested_duration n
    _ -> 0

instance Pretty Ly where pretty = to_lily

instance ToLily Ly where
    to_lily ly = case ly of
        LyBarline Nothing -> "|"
        LyBarline (Just meter) -> "| " <> "\\time " <> to_lily meter
        LyNote note -> to_lily note
        LyRest rest -> to_lily rest
        LyNested nested -> to_lily nested
        LyCode code -> code

count_notes_rests :: [Ly] -> Int
count_notes_rests = Seq.count $ \ly -> case ly of
    LyNote {} -> True
    LyRest {} -> True
    _ -> False

-- | This represents a bit of nested lilypond code, e.g.
-- \something { contents }.
data Nested = Nested {
    nested_prefix :: !Code
    , nested_contents :: !(NonEmpty Ly)
    , nested_suffix :: !Code
    , nested_duration :: !Time
    } deriving (Show)

instance ToLily Nested where
    to_lily (Nested prefix contents suffix _) =
        t_unwords $ prefix : map to_lily (NonEmpty.toList contents) ++ [suffix]

-- ** Note

data Note = Note {
    -- | Greater than one pitch indicates a chord.
    note_pitches :: !(NonEmpty NotePitch)
    , note_duration :: !Types.NoteDuration
    -- | Additional code to prepend to the note.
    , note_prepend :: ![Code]
    -- | Additional code to append to the note.
    , note_append :: ![Code]
    , note_stack :: !(Maybe Stack.UiFrame)
    } deriving (Show)

-- | Pitch, tie, code to append.
data NotePitch = NotePitch !Text !Tie ![Code]
    deriving (Show)

data Tie = NoTie | TieNeutral | TieUp | TieDown deriving (Show)

-- | Arbitrary bit of lilypond code.  This type isn't used for non-arbitrary
-- chunks, like 'note_pitches'.
type Code = Text

instance ToLily Note where
    to_lily (Note pitches dur prepend append _) =
        t_unwords $ prepend ++ [note] ++ append
        where
        ly_dur = to_lily dur
        note = case pitches of
            NotePitch pitch tie code :| [] ->
                mconcat [pitch, ly_dur, to_lily tie, t_unwords code]
            _ -> "<" <> t_unwords (map to_lily (NonEmpty.toList pitches))
                <> ">" <> ly_dur

instance ToLily NotePitch where
    to_lily (NotePitch pitch tie code) = pitch <> to_lily tie <> t_unwords code

instance ToLily Tie where
    to_lily t = case t of
        NoTie -> ""
        TieNeutral -> "~"
        TieUp -> "^~"
        TieDown -> "_~"

-- ** Rest

data Rest = Rest {
    rest_type :: !RestType
    , rest_prepend :: ![Code]
    , rest_append :: ![Code]
    } deriving (Show)

make_rest :: RestType -> Rest
make_rest typ = Rest
    { rest_type = typ
    , rest_prepend = []
    , rest_append = []
    }

data RestType =
    NormalRest !Types.NoteDuration
    | HiddenRest !Types.NoteDuration
    | FullMeasure !Types.Duration !Int
    deriving (Show)

rest_time :: Rest -> Time
rest_time rest = case rest_type rest of
    NormalRest dur -> Types.note_dur_to_time dur
    HiddenRest dur -> Types.note_dur_to_time dur
    FullMeasure dur mult -> Types.multiply_int mult (Types.dur_to_time dur)

instance ToLily Rest where
    to_lily (Rest typ prepend append) =
        t_unwords $ prepend ++ [to_lily typ] ++ append

instance ToLily RestType where
    to_lily r = case r of
        NormalRest dur -> "r" <> to_lily dur
        HiddenRest dur -> "s" <> to_lily dur
        FullMeasure dur mult -> "R" <> to_lily dur <> "*" <> showt mult

-- ** Key

data Key = Key !Text !Mode deriving (Eq, Show)
type Mode = Text

instance Pretty Key where pretty = to_lily
instance ToLily Key where
    to_lily (Key tonic mode) = "\\key " <> tonic <> " \\" <> mode

parse_key :: Text -> Either Error Key
parse_key key_name = do
    key <- justErr ("unknown key: " <> key_name) $
        Twelve.lookup_key (Just (Pitch.Key key_name))
    (pc, acc) <- Types.parse_degree (Theory.key_tonic key)
    let tonic = to_lily pc <> to_lily acc
    mode <- justErr ("unknown mode: " <> Theory.key_name key) $
        Map.lookup (Theory.key_name key) modes
    return $ Key tonic mode
    where
    modes = Map.fromList
        [ ("min", "minor"), ("locrian", "locrian"), ("maj", "major")
        , ("dorian", "dorian"), ("phrygian", "phrygian"), ("lydian", "lydian")
        , ("mixolydian", "mixolydian")
        ]

lookup_key :: Event -> ConvertM Key
lookup_key = lookup_val EnvKey.key parse_key default_key

default_key :: Key
default_key = Key "c" "major"

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
    to_lily v = case v of
        VoiceOne -> "\\voiceOne"; VoiceTwo -> "\\voiceTwo"
        VoiceThree -> "\\voiceThree"; VoiceFour -> "\\voiceFour"

instance Pretty Voice where pretty = showt

parse_voice :: Int -> Maybe Voice
parse_voice v = case v of
    1 -> Just VoiceOne; 2 -> Just VoiceTwo
    3 -> Just VoiceThree; 4 -> Just VoiceFour
    _ -> Nothing

event_voice :: Event -> Maybe (Either Error Voice)
event_voice event =
    event_context event . parse <$>
        Env.checked_val2 EnvKey.voice (event_environ event)
    where
    parse (Left err) = Left err
    parse (Right voice) =
        justErr ("voice should be 1--4: " <> showt voice) $ parse_voice voice

-- * Event

-- | Clip off the part of the event before the given time, or Nothing if it
-- was entirely clipped off.
clip_event :: Time -> Event -> Maybe Event
clip_event end e
    | left <= 0 = Nothing
    | otherwise = Just $
        e { event_start = end, event_duration = left, event_clipped = True }
    where left = event_end e - end
