-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
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

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import qualified Util.Control as Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Attrs as Attrs
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
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
    wanted (Code code) = any (`Text.isPrefixOf` code)
        ["\\time ", "\\key ", "\\bar "]
    wanted _ = True
    has_duration (LyNote n) = Just $ note_duration n
    has_duration (LyRest r) = Just $ rest_duration r
    has_duration _ = Nothing
    hush lys = -- TODO simplify durs
        map (LyRest . make_rest HiddenRest) durs ++ case non_notes of
            ly : rest -> ly : hush rest
            [] -> []
        where (durs, non_notes) = Seq.span_while has_duration lys

-- * process

run_process :: Monad m => m [b] -- ^ run at the end to emit final bits
    -> ([a] -> m ([b], [a])) -- ^ return results and the next input
    -> [a] -> m [b]
run_process complete chunk = go
    where
    go xs = do
        (ys, remaining) <- chunk xs
        if null remaining then (ys++) <$> complete else do
            remaining_ys <- go remaining
            return $ ys ++ remaining_ys

type VoiceLy = Either Voices Ly

-- | This figures out timing and emits a stream of lilypond code.
process :: Types.Config -> Time -> [Meter.Meter] -- ^ one for each measure
    -> [Event] -> Either Text [VoiceLy]
process config start meters events = do
    let state1 = make_state config start meters default_key
    key <- maybe (return default_key)
        (fmap fst . run_convert state1 . lookup_key) (Seq.head events)
    let state2 = state1 { state_key = key }
    (lys, _) <- run_convert state2 $
        error_context ("start: " <> pretty start) $ convert events
    let meter = fromMaybe Meter.default_meter (Seq.head meters)
    return $ Right (Code $ "\\time " <> to_lily meter)
        : Right (Code $ to_lily key) : lys

convert :: [Event] -> ConvertM [VoiceLy]
convert = run_process trailing_rests go
    where
    go :: [Event] -> ConvertM ([VoiceLy], [Event])
    go [] = return ([], [])
    go events = do
        (voices, events) <- convert_voices events
        (lys, remaining) <- convert_chunk True events
        return (voices ++ map Right lys, remaining)
    trailing_rests = do
        meters <- State.gets state_meters
        if null meters then return [Right final_barline] else do
        end <- State.gets state_measure_end
        rests <- rests_until end
        remaining <- trailing_rests
        return $ map Right rests ++ remaining
    final_barline = Code "\\bar \"|.\""

convert_voice :: Time -> [Event] -> ConvertM [Ly]
convert_voice end = run_process (rests_until end) (convert_chunk True)

-- | Convert Events to Ly, but never split notes or rests based on meter.
-- Chords of course can force ties.
convert_unmetered :: Time -> [Event] -> ConvertM [Ly]
convert_unmetered end =
    run_process (unmetered_rests_until end) (convert_chunk False)

-- ** convert_chunk

-- | Convert the rests for the first event, and a single slice of time.
-- If notes had to be split and tied, they are put back into the remaining
-- events.
convert_chunk :: Bool -> [Event] -> ConvertM ([Ly], [Event])
convert_chunk metered events = error_context current $
    case zero_dur_in_rest events of
        ([], []) -> return ([], [])
        (zeros@(event:_), []) -> do
            mapM_ update_subdivision zeros
            return (mix_in_code (event_start event) zeros [], [])
        (zeros, event : events) -> do
            mapM_ update_subdivision zeros
            start <- State.gets state_time
            rests <- mix_in_code start zeros <$> rests_until (event_start event)
            -- Debug.tracepM "convert_chunk" (zeros, event, events)
            (lys, remaining) <- convert_chunk_notes metered (event :| events)
            return (rests <> lys, remaining)
    where current = maybe "no more events" pretty (Seq.head events)

-- | Look for a magic event in the events starting here, and handle it if
-- found.
convert_chunk_notes :: Bool -> NonEmpty Event -> ConvertM ([Ly], [Event])
convert_chunk_notes metered events
    | Just ((score_dur, real_dur), (_, remaining))
            <- find Constants.get_tuplet = do
        score_dur <- real_to_time score_dur
        real_dur <- real_to_time real_dur
        convert_tuplet start score_dur real_dur remaining
    | Just ((), (event, remaining)) <- find has_tremolo =
        convert_tremolo event remaining
    | otherwise = convert_chord metered events
    where
    has_tremolo env
        | Env.is_set Constants.v_tremolo env = Just ()
        | otherwise = Nothing
    start = event_start (NonEmpty.head events)
    find match = find_here start (match . event_environ)
        (NonEmpty.toList events)

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

-- | Code events are mixed into the Lys, depending on their prepend or append
-- attrs.
--
-- This is doing the same thing as 'make_lys', but since rests aren't
-- represented explicitly by events as notes are, I have to first generate the
-- notes, and then mix in the code afterwards.
mix_in_code :: Time -> [Event] -> [Ly] -> [Ly]
mix_in_code start codes lys = go codes $ with_starts start lys
    where
    go codes [] = prepend ++ append
        where (prepend, append) = partition_code codes
    go codes ((start, ly) : lys) = applied ++ go rest_code lys
        where (applied, rest_code) = apply_code start codes ly

-- | This is the same thing as 'mix_in_code', except it has to mix code into
-- [VoiceLy], which is more complicated.  The duplicated logic is pretty
-- unfortunate but I couldn't think of a way to get rid of it.  See
-- 'collect_voices' for more information.
mix_in_code_voices :: Time -> [Event] -> [VoiceLy] -> [VoiceLy]
mix_in_code_voices start codes lys = go codes start lys
    where
    go codes _ [] = map Right $ prepend ++ append
        where (prepend, append) = partition_code codes
    go codes start (Right ly : lys) =
        map Right applied ++ go rest_code (ly_duration ly + start) lys
        where (applied, rest_code) = apply_code start codes ly
    go codes start (Left (Voices []) : rest) = go codes start rest
    go codes start (Left (Voices ((voice, lys) : voices)) : rest) =
        Left (Voices $ (voice, mixed) : voices) : go post end rest
        where
        (pre, post) = span ((<end) . event_start) codes
        end = start + sum (map ly_duration lys)
        mixed = mix_in_code start pre lys

apply_code :: Time -> [Event] -> Ly -> ([Ly], [Event])
apply_code start codes ly = (prepend ++ ly : append, post)
    where
    (prepend, append) = partition_code pre
    (pre, post) = span ((<end) . event_start) codes
    end = ly_duration ly + start

partition_code :: [Event] -> ([Ly], [Ly])
partition_code events = (map Code prepend, map Code append)
    where
    prepend = filter (not . Text.null) $
        map (get_code Constants.v_prepend) events
    append = filter (not . Text.null) $
        map (get_code Constants.v_append_all) events

get_val :: Typecheck.Typecheck a => Env.Key -> Event -> Maybe a
get_val k = Env.maybe_val k . event_environ

get_code :: Env.Key -> Event -> Code
get_code k = fromMaybe "" . get_val k

-- | Partition events which have zero dur and don't coincide with the next
-- non-zero dur event.
zero_dur_in_rest :: [Event] -> ([Event], [Event])
zero_dur_in_rest events = span (\e -> zero_dur_non_tuplet e && in_rest e) events
    where
    next_note = Seq.head (filter (not . zero_dur_non_tuplet) events)
    in_rest e = maybe True ((> event_start e) . event_start) next_note
    -- Ack, except tuplet events are zero dur but are not code events,
    -- they are interpreted by 'convert_tuplet'.
    zero_dur_non_tuplet e = zero_dur e
        && Constants.get_tuplet (event_environ e) == Nothing

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
    lys <- convert_unmetered (start + score_dur)
        (map (stretch factor start) in_tuplet)

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
    return (code : lys ++ [Code "}"] ++ maybe [] (:[]) barline, out)

-- | Convention is that the number on the tuplet is at least the number of
-- notes inside, but lilypond doesn't do that automatically.
tuplet_code :: Int -> Rational -> Ly
tuplet_code notes r =
    Code $ "\\tuplet " <> showt num <> "/" <> showt denom <> " {"
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
    let (dur0, with_dur) = List.partition zero_dur in_tremolo
    prev_attrs <- State.gets state_prev_attrs
    let (next_attrs, notes) = tremolo_make_lys prev_attrs with_dur
    State.modify' $ \state -> state { state_prev_attrs = next_attrs }

    -- TODO promote this to the caller so everyone doesn't have to do it?
    barline <- advance_measure (start + dur)

    let (times, frac) = properFraction $ Types.to_whole dur * 16
    when (frac /= 0) $ throw $ "dur " <> pretty dur
        <> " doesn't yield an integral number of 16th notes: "
        <> pretty (Types.to_whole dur * 16)
    let code = Code $ "\\repeat tremolo " <> showt times <> " {"
    -- As long as I don't support v_append_first for 0 dur events, I'll put
    -- the appends after the first note, because in practice that's what
    -- looks best.  E.g. dynamics should go on the first note of the tremolo.
    -- TODO support the append variants for 0 dur
    let (prepend, append) = partition_code dur0
    let surround = prepend ++ case map LyNote notes of
            n : ns -> n : append ++ ns
            [] -> append
    let clipped
            | dur >= total_dur = []
            | otherwise = mapMaybe (clip_event (start + dur))
                (tremolo_event : in_tremolo)
    return (code : surround ++ [Code "}"] ++ maybe [] (:[]) barline,
        clipped ++ out)

    where
    start = event_start tremolo_event
    total_dur = event_duration tremolo_event
    get_allowed_dur = do
        meter <- get_subdivision
        measure_start <- State.gets state_measure_start
        return $ Types.note_dur_to_time $
            Meter.allowed_duration use_dot meter
                (start - measure_start)
                (start - measure_start + event_duration tremolo_event)
            where use_dot = True

tremolo_make_lys :: Attrs.Attributes -> [Event] -> (Attrs.Attributes, [Note])
tremolo_make_lys prev_attrs = List.mapAccumL make prev_attrs . zip_first_last
    where
    make prev_attrs (is_first, event, is_last) = (next_attrs,) $ Note
        { note_pitches = NotePitch pitch NoTie note_code :| []
        , note_duration = Types.NoteDuration Types.D32 False
        , note_prepend = prepend
        , note_append = append ++ attrs_codes
        , note_stack = Seq.last $ Stack.to_ui (event_stack event)
        }
        where
        note_code = slur ++ append_note_code is_first event
        slur
            | is_first && not is_last = ["("]
            | not is_first && is_last = [")"]
            | otherwise = []
        pitch = pitch_code is_first event
        prepend = prepend_code is_first event
        append = append_code is_first is_last event
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
    let key_change = [Code (to_lily key) | key /= state_key state]
    mapM_ update_subdivision $
        takeWhile ((== event_start (NonEmpty.head events)) . event_start) $
        NonEmpty.toList events
    meter <- if metered then Just <$> get_subdivision else return Nothing
    let (chord_notes, end, last_attrs, remaining) = make_lys
            (state_measure_start state) (state_prev_attrs state)
            meter events
    barline <- if metered then advance_measure end
        else advance_unmetered end >> return Nothing

    -- Lilypond will throw a barcheck error and produce broken score, and
    -- 'convert_tuplet' rewinds I'll get an extra barline, so disallow this.
    -- unless metered $ whenJust barline $ const $
    --     throw $ "An unmetered note went past a barline. This probably means\
    --         \ a tuplet tried to go over a barline: " <> pretty end
    State.modify' $ \state -> state
        { state_key = key
        , state_prev_attrs = last_attrs
        }
    return (key_change <> chord_notes <> maybe [] (:[]) barline, remaining)

update_subdivision :: Event -> ConvertM ()
update_subdivision event = case get_val Constants.v_subdivision event of
    Nothing -> return ()
    Just "" -> State.modify' $ \state ->
        state { state_subdivision = Nothing }
    Just m -> do
        meter <- tryRight $ first
            (("can't parse meter in " <> pretty Constants.v_subdivision
                <> ": " <> showt m <> ": ")<>)
            (Meter.parse_meter m)
        State.modify' $ \state -> state { state_subdivision = Just meter }

-- | Convert a chunk of events all starting at the same time.  Events
-- with 0 duration or null pitch are expected to have either
-- 'Constants.v_prepend' or 'Constants.v_append_all', and turn into
-- 'Code' Notes.
--
-- The rules are documented in 'Perform.Lilypond.Convert.convert_event'.
make_lys :: Time -> Attrs.Attributes
    -- ^ Previous note's Attributes, to track 'modal_articulations'.
    -> Maybe Meter.Meter -> NonEmpty Event
    -> ([Ly], Time, Attrs.Attributes, [Event])
    -- ^ (note, note end time, last attrs, remaining events)
make_lys measure_start prev_attrs maybe_meter events =
    (notes, end, last_attrs, clipped ++ remaining)
    where
    -- As with rests, figuring out which notes to put the code events on is
    -- tricky.  The idea is the same, but rests are generated en masse for
    -- a block of time while notes are generated one at a time.

    -- Get events that all start at the same time, and make a Ly if there are
    -- any.  Otherwise they must all be zero dur code events that are
    -- "free standing", not overlapped by any previous note.
    (here, after) = NonEmpty.break
        ((> event_start (NonEmpty.head events)) . event_start) events
    (dur0, with_dur) = List.partition zero_dur here

    (maybe_note, end, last_attrs, clipped) = case NonEmpty.nonEmpty with_dur of
        Nothing -> (Nothing, start, prev_attrs, [])
            where start = event_start (NonEmpty.head events)
        Just chord -> (Just n, end, next_attrs, clipped)
            where
            next = event_start <$> List.find (not . zero_dur) after
            (n, end, next_attrs, clipped) =
                make_note measure_start prev_attrs maybe_meter chord next

    -- Now that I know the duration of the Ly (if any) I can get the zero-dur
    -- code events it overlaps.
    (overlapping, remaining) =
        span (\e -> zero_dur e && event_start e < end) after
    (prepend, append) = partition_code (dur0 ++ overlapping)
    -- Circumfix the possible real note with zero-dur code placeholders.
    notes = prepend ++ maybe [] (:[]) maybe_note ++ append

make_note :: Time -> Attrs.Attributes -> Maybe Meter.Meter
    -> NonEmpty Event -- ^ Events that occur at the same time.
    -- All these events must have >0 duration!
    -> Maybe Time -> (Ly, Time, Attrs.Attributes, [Event])
    -- ^ (note, note end time, clipped)
make_note measure_start prev_attrs maybe_meter chord next =
    (ly, allowed_end, next_attrs, clipped)
    where
    ly = case NonEmpty.nonEmpty note_pitches of
        -- TODO I think I don't actually know if this rest is the "last" one,
        -- since they're not tied.  So this may duplicate code.
        Nothing -> Code $ t_unwords $ prepend_chord ++ append_chord
        Just note_pitches -> LyNote (note note_pitches)
    first = NonEmpty.head chord
    -- If there are no pitches, then this is code with duration.
    note_pitches = do
        -- Sorting by pitch puts the chord notes in a predictable order.  Some
        -- lilypond notation, such as glissandoMap, refers to chord notes by
        -- index.
        event <- Seq.sort_on event_pitch $ NonEmpty.toList chord
        let pitch = pitch_code (is_first event) event
        guard $ not (Text.null pitch)
        let tie = note_tie event
        return $ NotePitch pitch tie (append_note_code (is_first event) event)
    note note_pitches = Note
        { note_pitches = note_pitches
        , note_duration = allowed_dur
        , note_prepend = prepend_chord
        , note_append = append_chord ++ attrs_codes
        , note_stack = Seq.last (Stack.to_ui (event_stack first))
        }
    (attrs_codes, next_attrs) = attrs_to_code prev_attrs
        (mconcat (map event_attributes (NonEmpty.toList chord)))

    -- These will wind up with "" in them, but t_unwords strips that.
    -- I take the union of all the code bits because it's confusing if code
    -- disappears depending on where it happens to fall in the chord.
    prepend_chord = Seq.unique $ concat
        [prepend_code (is_first e) e | e <- NonEmpty.toList chord]
    append_chord = Seq.unique $ concat
        [append_code (is_first e) (is_last e) e | e <- NonEmpty.toList chord]
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
    start = event_start first
    allowed_end = start + Types.note_dur_to_time allowed_dur
    allowed_dur = case maybe_meter of
        Nothing -> Types.time_to_note_dur (max_end - start)
        Just meter ->
            Meter.allowed_duration use_dot meter in_measure (max_end - start)
            where
            use_dot = True -- not use_dot is for rests.
            in_measure = start - measure_start
    -- Maximum end, the actual end may be shorter since it has to conform to
    -- a Duration.
    max_end = min_if next $ Seq.ne_minimum (fmap event_end chord)

min_if :: Ord a => Maybe a -> a -> a
min_if ma b = maybe b (min b) ma

-- ** ly code env vars

t_unwords :: [Text] -> Text
t_unwords = Text.unwords . filter (not . Text.null) . map Text.strip

append_code :: Bool -> Bool -> Event -> [Code]
append_code is_first is_last event = Seq.unique $ filter (not . Text.null) $
    concat
    [ [get_code Constants.v_append_first event | is_first]
    , [get_code Constants.v_append_last event | is_last]
    , [get_code Constants.v_append_all event]
    ]

append_note_code :: Bool -> Event -> [Code]
append_note_code is_first event = Seq.unique $ filter (not . Text.null) $ concat
    [ [get_code Constants.v_note_append_first event | is_first]
    , [get_code Constants.v_note_append_all event]
    ]

prepend_code :: Bool -> Event -> [Code]
prepend_code is_first event
    | is_first = filter (not . Text.null) [get_code Constants.v_prepend event]
    | otherwise = []

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

-- * convert voices

convert_voices :: [Event] -> ConvertM ([VoiceLy], [Event])
convert_voices [] = return ([], [])
convert_voices events@(event:_) = do
    (voices, code, events) <- either throw return $ collect_voices events
    voices <- mix_in_code_voices (event_start event) code . simplify_voices <$>
        voices_to_ly voices
    return (voices, events)

-- ** collect_voices

-- | Span events until they don't have a 'Constants.v_voice' val.
--
-- I used to mix the code into the first voice here, but 'simplify_voices' may
-- then get rid of it.  So return the code events and mix them in after
-- simplification.
collect_voices :: [Event] -> Either Text (VoiceMap Event, [Event], [Event])
collect_voices events = do
    let (spanned, rest) = span_voices events
        (code, with_voice) = Either.partitionEithers spanned
    with_voice <- mapM check_type with_voice
    return $ case Seq.group_fst with_voice of
        [] -> ([], [], events)
        voices -> (voices, code, rest)
    where
    check_type (Right num, event) = do
        voice <- justErr ("voice should be 1--4: " <> showt num) $
            parse_voice num
        return (voice, event)
    check_type (Left err, event) = Left $ pretty event <> ": " <> err

-- | Strip off events with voices.  This is complicated by the possibility of
-- intervening zero_dur code events.
span_voices :: [Event] -> ([Either Event (Either Text Int, Event)], [Event])
span_voices [] = ([], [])
span_voices events
    | [_] <- spanned2 = ([], events)
    | otherwise = (spanned2, trailing_code ++ rest)
    where
    (spanned1, rest) = Seq.span_while voice_of events
    (spanned2, trailing_code) =
        Seq.span_end_while (either Just (const Nothing)) spanned1
    voice_of event
        | zero_dur event = Just $ Left event
        | otherwise = case get event of
            Nothing -> Nothing
            Just voice -> Just $ Right (voice, event)
        where get = Env.checked_val2 EnvKey.voice . event_environ
    -- Previously I tried to only split voices where necessary by only spanning
    -- overlapping notes, or notes with differing voices.  But even when it
    -- worked as intended, joining voices this aggressively led to oddities
    -- because it would turn any two voices with the same duration notes into
    -- a chord.  So now I simplify voices only at the measure level, in
    -- 'simplify_voices'.

-- ** voices_to_ly

-- | Like 'convert', but converts within a voice, which means no nested voices
-- are expected.
voices_to_ly :: VoiceMap Event -> ConvertM (VoiceMap Ly)
voices_to_ly [] = return []
voices_to_ly voices = do
    state <- State.get
    let max_dur = fromMaybe (state_measure_start state) $ Seq.maximum $
            mapMaybe (Seq.maximum . map event_end . snd) voices
    (states, voice_lys) <- unzip <$> mapM (convert max_dur state) voices
    -- Since I pad with rests to the longest voice, I also want the State from
    -- that one.
    whenJust (Seq.maximum_on state_time states) State.put
    return voice_lys
    where
    convert max_dur state (v, events) = do
        (measures, final_state) <- either throw return $
            run_convert state (convert_voice max_dur events)
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
        | (start, ly) <- with_starts 0 lys, full_measure_rest ly
        ]
    full_measure_rest (LyRest (Rest { rest_type = FullMeasure {} })) = True
    full_measure_rest _ = False

    -- Strip out voices that consist entirely of rests, keeping at least one.
    strip voices = case filter (not . rest_measure . snd) voices of
            [] -> take 1 voices
            voices -> voices
        where
        rest_measure = all (\ly -> full_measure_rest ly || ly_duration ly == 0)
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

-- | Pad with rests until given Time, which is not necessarily on a measure
-- boundary.
rests_until :: Time -> ConvertM [Ly]
rests_until end = do
    now <- State.gets state_time
    if now >= end then return [] else do
        measure_end <- State.gets state_measure_end
        rests <- create_rests (min end measure_end)
        remaining <- rests_until end
        return $ rests <> remaining
    where
    create_rests end = do
        state <- State.get
        meter <- get_subdivision
        barline <- advance_measure end
        let rests = make_rests (state_config state) meter
                (state_time state - state_measure_start state)
                (end - state_measure_start state)
        return $ map LyRest rests <> maybe [] (:[]) barline

unmetered_rests_until :: Time -> ConvertM [Ly]
unmetered_rests_until end = do
    now <- State.gets state_time
    if now >= end then return [] else do
        let durs = Types.time_to_note_durs (end - now)
        return $ map (LyRest . make_rest NormalRest) durs

-- | Advance now to the given time, up to and including the end of the measure,
-- but it's an error to try to go past.  Return Ly with a Barline if this is
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
                | to_lily prev_meter == to_lily meter -> Just (Barline Nothing)
                | otherwise -> Just (Barline (Just meter))
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

run_convert :: State -> ConvertM a -> Either Text (a, State)
run_convert state = Identity.runIdentity . Except.runExceptT
    . flip State.runStateT state

type ConvertM = State.StateT State (Except.ExceptT Text Identity.Identity)

error_context :: Text -> ConvertM a -> ConvertM a
error_context msg = map_error ((msg <> ": ") <>)

map_error :: (Text -> Text) -> ConvertM a -> ConvertM a
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

throw :: Text -> ConvertM a
throw msg = do
    now <- State.gets state_time
    Except.throwError $ pretty now <> ": " <> msg

lookup_val :: Env.Key -> (Text -> Either Text a) -> a -> Event -> ConvertM a
lookup_val key parse deflt event = prefix $ do
    maybe_val <- Env.checked_val key (event_environ event)
    maybe (Right deflt) parse maybe_val
    where
    prefix = either (throw . ((pretty key <> ": ") <>)) return


-- * types

data Ly = Barline !(Maybe Meter.Meter) | LyNote !Note | LyRest !Rest
    | Code !Code
    deriving (Show)

ly_duration :: Ly -> Time
ly_duration ly = case ly of
    LyNote note -> Types.note_dur_to_time (note_duration note)
    LyRest rest -> rest_time rest
    _ -> 0

instance Pretty Ly where pretty = to_lily

instance ToLily Ly where
    to_lily ly = case ly of
        Barline Nothing -> "|"
        Barline (Just meter) -> "| " <> "\\time " <> to_lily meter
        LyNote note -> to_lily note
        LyRest rest -> to_lily rest
        Code code -> code

count_notes_rests :: [Ly] -> Int
count_notes_rests = Seq.count $ \ly -> case ly of
    LyNote {} -> True
    LyRest {} -> True
    _ -> False

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
    to_lily (Note pitches dur prepend append _stack) =
        -- The append codes are separated by spaces, but not the first one,
        -- so I'll get 'c4-. \xyz', instead of 'c4 -. \xyz'.  Lilypond doesn't
        -- care, but the former looks a bit nicer and besides all my tests
        -- expect it.
        t_unwords $ prepend ++ [note <> t_unwords append]
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
    rest_duration :: !Types.NoteDuration
    -- | If true, the rest is emitted as @R@ and is expected to cover a whole
    -- measure.
    , rest_type :: !RestType
    , rest_prepend :: !Code
    , rest_append :: !Code
    } deriving (Show)

make_rest :: RestType -> Types.NoteDuration -> Rest
make_rest typ dur = Rest
    { rest_duration = dur
    , rest_type = typ
    , rest_prepend = ""
    , rest_append = ""
    }

data RestType =
    NormalRest
    -- | Unlike other rests, FullMeasure rests have their duration encoded.
    -- It's redundant with rest_duration, but should always be the same.
    | FullMeasure !Types.Duration !Int
    | HiddenRest
    deriving (Show)

rest_time :: Rest -> Time
rest_time = Types.note_dur_to_time . rest_duration

instance ToLily Rest where
    to_lily (Rest dur typ prepend append) =
        (prepend<>) . (<>append) $ case typ of
            FullMeasure {} -> to_lily typ
            _ -> to_lily typ <> to_lily dur

instance ToLily RestType where
    to_lily r = case r of
        NormalRest -> "r"
        FullMeasure dur mult -> "R" <> to_lily dur <> "*" <> showt mult
        HiddenRest -> "s"

make_rests :: Types.Config -> Meter.Meter -> Time -> Time -> [Rest]
make_rests config meter start end
    | start >= end = []
    | full_measure =
        map (make_rest
                (FullMeasure (Meter.meter_denom meter) (Meter.time_num meter)))
            (take 1 dur)
    | otherwise = map (make_rest NormalRest) dur
    where
    full_measure = start `Num.fmod` measure == 0 && end - start >= measure
    measure = Meter.measure_time meter
    dur = Meter.convert_duration meter (Types.config_dotted_rests config)
        start (end - start)

-- ** Key

data Key = Key !Text !Mode deriving (Eq, Show)
type Mode = Text

instance ToLily Key where
    to_lily (Key tonic mode) = "\\key " <> tonic <> " \\" <> mode

parse_key :: Text -> Either Text Key
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

-- * Event

-- | Clip off the part of the event before the given time, or Nothing if it
-- was entirely clipped off.
clip_event :: Time -> Event -> Maybe Event
clip_event end e
    | left <= 0 = Nothing
    | otherwise = Just $
        e { event_start = end, event_duration = left, event_clipped = True }
    where left = event_end e - end
