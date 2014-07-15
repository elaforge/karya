-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for calls to cooperate with the lilypond backend.
module Derive.Call.Lily where
import qualified Data.List as List

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import qualified Perform.Lilypond.Types as Types
import qualified Perform.RealTime as RealTime

import Types


-- * utils for ly calls

when_lilypond :: Derive.Deriver a -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
when_lilypond lily = when_lilypond_config (const lily)

when_lilypond_config :: (Types.Config -> Derive.Deriver a)
    -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
when_lilypond_config lily not_lily =
    maybe not_lily lily =<< Derive.lookup_lilypond_config

-- | Only emit the deriver if I'm in lilypond mode.
only_lilypond :: Derive.NoteDeriver -> Derive.NoteDeriver
only_lilypond deriver = ifM Derive.is_lilypond_derive deriver mempty

-- | When in lilypond mode, generate a note with the given Code.
note_code :: Code -> Derive.PassedArgs d -> Derive.NoteDeriver
    -> Derive.NoteDeriver
note_code code args = when_lilypond $
    add_code code $ Util.place args Util.note

-- ** transformer

-- | Add code to the first event.
add_first :: Code -> Derive.NoteDeriver -> Derive.NoteDeriver
add_first code deriver =
    Post.map_first (return . add_event_code code) =<< deriver

-- ** note transformer

-- | Replace a note transformer with one that derives its sub-events as-is
-- and adds lilypond code to them.
notes_code :: Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_code code = notes_with (add_code code)

-- | Like 'notes_code', but only apply the code to the first event, not all of
-- them.
first_note_code :: Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
first_note_code code args = when_lilypond $
    add_first code $ place_notes args

-- | This is like 'notes_code', but the first event in each track gets the
-- start code, and the last event in each track gets the end code.
notes_around :: Code -> Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_around start end args = when_lilypond $
    mconcatMap around =<< Sub.sub_events args
    where
    around notes = first_last
        (add_event_code start) (add_event_code end) <$> Sub.place notes

-- | Like 'notes_around', but for use when you already know you're in lilypond
-- mode.
notes_around_ly :: Code -> Code -> Derive.PassedArgs d -> Derive.NoteDeriver
notes_around_ly start end = mconcatMap around <=< Sub.sub_events
    where
    around notes = first_last
        (add_event_code start) (add_event_code end) <$> Sub.place notes

-- | Like 'notes_around', but when I'm not in lilypond mode just derive the
-- sub events unchanged.
code_around :: Code -> Code -> Derive.PassedArgs d -> Derive.NoteDeriver
code_around start end args = when_lilypond
    (code0 (Args.start args) start
        <> place_notes args <> code0 (Args.end args) end)
    (place_notes args)

-- | Transform and evaluate the sub events.
notes_with :: (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_with f args = when_lilypond $
    Sub.place . Sub.map_events f . concat =<< Sub.sub_events args

place_notes :: Derive.PassedArgs d -> Derive.NoteDeriver
place_notes = Sub.place . concat <=< Sub.sub_events

-- ** events around

add_event_code :: Code -> Score.Event -> Score.Event
add_event_code (pos, code) =
    Score.modify_environ $ add (position_env pos) (<>code)
    where
    add name f env = TrackLang.insert_val name (TrackLang.to_val (f old)) env
        where old = fromMaybe "" $ TrackLang.maybe_val name env

-- | Like 'Seq.first_last', but applied to LEvents.  If the events start or end
-- with a group of events with the same start time, the start or end function
-- is applied to the entire group.  This is because the lilypond performer will
-- group them into a chord and will only take ly-prepend and ly-append from the
-- first note in the chord.  I could apply to only the first element of the
-- group, but that would rely on every sort being stable.
first_last :: (Score.Event -> Score.Event) -> (Score.Event -> Score.Event)
    -> Derive.Events -> Derive.Events
first_last start end xs =
    map LEvent.Log logs ++ map LEvent.Event (concat
        (Seq.first_last (map start) (map end) (List.groupBy cmp events)))
    where
    (events, logs) = LEvent.partition xs
    cmp x y = RealTime.eq (Score.event_start x) (Score.event_start y)

-- ** code

-- | Either prepend or append some code to a lilypond note.
type Code = (CodePosition, Ly)
data CodePosition =
    -- | Code goes before the note.
    Prefix
    -- | Code goes after each note in a tied sequence, so it could get
    -- duplicated several times.
    | SuffixAll
    -- | Code goes after only the first note in a tied sequence.
    | SuffixFirst
    -- | Code goes after the last note in a tied sequnece.
    | SuffixLast
    deriving (Bounded, Enum, Show)

-- | Fragment of Lilypond code.
type Ly = Text

-- | A lilypond \"note\", which is just a chunk of text.
type Note = Ly

position_env :: CodePosition -> TrackLang.ValName
position_env c = case c of
    Prefix -> Constants.v_ly_prepend
    SuffixFirst -> Constants.v_ly_append_first
    SuffixLast -> Constants.v_ly_append_last
    SuffixAll -> Constants.v_ly_append_all

prepend_code :: Ly -> Derive.NoteDeriver -> Derive.NoteDeriver
prepend_code = add_code . (,) Prefix

add_code :: Code -> Derive.NoteDeriver -> Derive.NoteDeriver
add_code (pos, code) = Derive.modify_val (position_env pos) $
    (<>code) . fromMaybe ""

-- | Emit a note that carries raw lilypond code.  The code is emitted
-- literally, and assumed to have the duration of the event.  The event's pitch
-- is ignored.  This can be used to emit lilypond that doesn't fit into
-- a 'Types.Event'.
code :: (ScoreTime, ScoreTime) -> Ly -> Derive.NoteDeriver
code (start, dur) code = Derive.with_val Constants.v_ly_prepend code $
    Derive.with_no_pitch $ Derive.place start dur Util.note

-- | Like 'code', but for 0 duration code fragments, and can either put them
-- before or after notes that occur at the same time.
code0 :: ScoreTime -> Code -> Derive.NoteDeriver
code0 start (pos_, code) = with (Derive.place start 0 Util.note)
    where
    -- SuffixFirst and SuffixLast are not used for 0 dur events, so make it
    -- less error-prone by getting rid of them.  Ick.
    pos = case pos_ of
        SuffixFirst -> SuffixAll
        SuffixLast -> SuffixAll
        _ -> pos_
    with = Derive.with_val (position_env pos) code

global_code0 :: ScoreTime -> Code -> Derive.NoteDeriver
global_code0 start = global . code0 start

-- | Test if an event is a 0 duration lilypond code event.
is_code0 :: Score.Event -> Bool
is_code0 event = Score.event_duration event == 0 && any has vals
    where
    vals = map position_env [minBound .. maxBound]
    has = (`TrackLang.val_set` Score.event_environ event)

-- ** convert

-- | Round the RealTime to the nearest NoteDuration.
note_duration :: Types.Config -> RealTime -> Types.NoteDuration
note_duration config = Types.time_to_note_dur . to_time config

-- | Like 'note_duration', but only succeeds if the RealTime is exactly
-- a NoteDuration.
is_note_duration :: Types.Config -> RealTime -> Maybe Types.NoteDuration
is_note_duration config = Types.is_note_dur . to_time config

is_duration :: Types.Config -> RealTime -> Maybe Types.Duration
is_duration config t = case is_note_duration config t of
    Just (Types.NoteDuration dur False) -> Just dur
    _ -> Nothing

note_pitch :: Derive.NoteDeriver -> Derive.Deriver Note
note_pitch deriver = do
    events <- deriver
    event <- require "had no event" $ Seq.head (LEvent.events_of events)
    pitch <- require "note had no pitch" $ Score.initial_pitch event
    let controls = Score.event_controls_at (Score.event_start event) event
    pitch_to_lily $ PitchSignal.apply (Score.event_environ event) controls pitch
    -- Wow, there are a lot of ways to fail.
    where
    require = Derive.require . (prefix <>)
    prefix = "Lily.note_pitch: "

pitch_to_lily :: PitchSignal.Pitch -> Derive.Deriver Note
pitch_to_lily pitch = do
    note <- right $ PitchSignal.pitch_note pitch
    pitch <- require ("unparseable note: " <> pretty note) $
        Twelve.read_absolute_pitch note
    right $ Types.show_pitch pitch
    where
    require = Derive.require . (prefix <>)
    right :: (Pretty.Pretty a) => Either a b -> Derive.Deriver b
    right = Derive.require_right ((prefix <>) . pretty)
    prefix = "Lily.pitch_to_lily: "

to_time :: Types.Config -> RealTime -> Types.Time
to_time = Types.real_to_time . Types.config_quarter_duration


-- ** eval

eval :: Types.Config -> Derive.PassedArgs d -> [Sub.Event]
    -> Derive.Deriver [Note]
eval config args notes = do
    start <- Args.real_start args
    (events, logs) <- LEvent.partition <$> Sub.place notes
    mapM_ Log.write logs
    eval_events config start events

eval_events :: Types.Config -> RealTime -> [Score.Event]
    -> Derive.Deriver [Note]
eval_events config start events = do
    meter <- maybe (return Meter.default_meter) parse_meter
        =<< Derive.lookup_val Constants.v_meter
    let (notes, logs) = eval_notes config meter start events
    mapM_ Log.write logs
    return notes
    where
    parse_meter = either (err . untxt) return . Meter.parse_meter
    err = Derive.throw . (("parse " <> pretty Constants.v_meter) <>)

eval_notes :: Types.Config -> Meter.Meter -> RealTime -> [Score.Event]
    -> ([Note], [Log.Msg])
eval_notes config meter start score_events = (map Types.to_lily notes, logs)
    where
    (events, logs) = LEvent.partition $ Convert.convert config score_events
    notes = Process.simple_convert config meter
        (Types.real_to_time quarter start)
        (Convert.quantize (Types.config_quantize config) events)
    quarter = Types.config_quarter_duration config


-- * calls

note_calls :: Derive.CallMaps Derive.Note
note_calls = Make.call_maps
    [ ("8va", c_8va)
    , ("xstaff", c_xstaff)
    , ("dyn", c_dyn)
    , ("clef", c_clef)
    , ("meter", c_meter)
    , ("movement", c_movement)
    , ("ly-!", c_reminder_accidental)
    , ("ly-?", c_cautionary_accidental)
    , ("ly-^~", c_tie_direction "^")
    , ("ly-_~", c_tie_direction "_")
    , ("ly-<", c_crescendo)
    , ("ly->", c_diminuendo)
    , ("ly^", c_ly_text_above)
    , ("ly_", c_ly_text_below)
    , ("ly-(", c_ly_begin_slur)
    , ("ly-)", c_ly_end_slur)
    , ("ly-pre", c_ly_pre)
    , ("ly-post", c_ly_post)
    , ("ly-key", c_ly_key)
    , ("ly-sus", c_ly_sus)
    ]
    [ ("if-ly", c_if_ly)
    ]
    [ ("when-ly", c_when_ly)
    , ("unless-ly", c_unless_ly)
    , ("ly-global", c_ly_global)
    , ("ly-track", c_ly_track)
    , ("not-ly-track", c_not_ly_track)
    ]

c_when_ly :: Derive.Transformer Derive.Note
c_when_ly = transformer "when-ly" mempty
    "With no arguments, evaluate the deriver only when in lilypond mode.\
    \ Unlike `ly-track`, this doesn't evaluate subtracks, so you can use it to\
    \ emit an entirely different set of tracks.\n\
    \ With arguments, evaluate them as a transformer and apply it only\
    \ when in lilypond mode.  Otherwise, the deriver is unchanged."
    $ Sig.parsed_manually "Any number of arguments of any type." (when_ly False)

c_unless_ly :: Derive.Transformer Derive.Note
c_unless_ly = transformer "unless-ly" mempty
    "The inverse of when-ly, evaluate the deriver or apply the args only when\
    \ not in lilypond mode."
    $ Sig.parsed_manually "Any number of arguments of any type." (when_ly True)

when_ly :: Bool -> Derive.PassedArgs Score.Event -> Derive.NoteDeriver
    -> Derive.NoteDeriver
when_ly inverted args deriver = case Derive.passed_vals args of
    [] -> when deriver mempty
    call : vals -> when (apply args (to_sym call) vals deriver) deriver
    where
    to_sym = TrackLang.Symbol . TrackLang.show_call_val
    when = if inverted then flip when_lilypond else when_lilypond
    apply args = Eval.reapply_transformer (Derive.passed_info args)

c_ly_global :: Derive.Transformer Derive.Note
c_ly_global = transformer "ly-global" mempty
    ("Evaluate the deriver only when in lilypond mode, like `when-ly`, but\
    \ also set the " <> ShowVal.show_val Constants.ly_global
    <> " instrument."
    ) $ Sig.call0t $ \_ deriver -> when_lilypond (global deriver) mempty

c_ly_track :: Derive.Transformer Derive.Note
c_ly_track = transformer "ly-track" mempty
    "Evaluate the deriver only when in lilypond mode, otherwise ignore this\
    \ track but evaluate its subtracks. Apply this to a track\
    \ to omit lilypond-only articulations, or to apply different articulations\
    \ to lilypond and non-lilypond output. Only use it in the track title!"
    $ Sig.call0t $ \args deriver -> when_lilypond deriver $ place_notes args

c_not_ly_track :: Derive.Transformer Derive.Note
c_not_ly_track = transformer "not-ly-track" mempty
    "The inverse of `ly-track`, evaluate the track only when not in lilypond\
    \ mode. Only use it in the track title!"
    $ Sig.call0t $ \args deriver -> flip when_lilypond deriver $
        place_notes args

c_if_ly :: Derive.Generator Derive.Note
c_if_ly = make_call "if-ly" mempty
    "Conditional for lilypond." $ Sig.call ((,)
    <$> required "is-ly" "Evaluated in lilypond mode."
    <*> required "not-ly" "Evaluated when not in lilypond mode."
    ) $ \(is_ly, not_ly) args -> when_lilypond
        (Eval.reapply_string args (TrackLang.show_call_val is_ly))
        (Eval.reapply_string args (TrackLang.show_call_val not_ly))

c_8va :: Make.Calls Derive.Note
c_8va = code0_call "ottava" "Emit lilypond ottava mark."
    (required "octave" "Transpose this many octaves up or down.") $
    \oct -> return (Prefix, ottava oct)
    where
    ottava :: Int -> Ly
    ottava n = "\\ottava #" <> showt n

c_xstaff :: Make.Calls Derive.Note
c_xstaff = code0_call "xstaff"
    "Emit lilypond to put the notes on a different staff."
    (required "staff" "Should be `up` or `down`.") $ \staff -> do
        when (staff `notElem` ["down", "up"]) $
            Derive.throw $ "expected 'up' or 'down', got "
                <> untxt (ShowVal.show_val staff)
        return (Prefix, change staff)
    where
    change staff = "\\change Staff = " <> Types.to_lily (staff :: Text)

c_dyn :: Make.Calls Derive.Note
c_dyn = code0_call "dyn"
    "Emit a lilypond dynamic. If there are notes below, they are derived\
    \ unchanged."
    (required "dynamic" "Should be `p`, `ff`, etc.")
    (return . (,) SuffixAll . ("\\"<>))

c_clef :: Make.Calls Derive.Note
c_clef = code0_call "clef" "Emit lilypond clef change."
    (required "clef" "Should be `bass`, `treble`, etc.")
    (return . (,) Prefix . ("\\clef "<>))

c_meter :: Make.Calls Derive.Note
c_meter = global_code0_call "meter"
    "Emit lilypond meter change. It will be interpreted as global no matter\
    \ where it is. Simultaneous different meters aren't supported yet."
    (required "meter" "Should be `4/4`, `3+3/8`, etc.") $
    \val -> Derive.with_val Constants.v_meter (val :: Text)

c_movement :: Make.Calls Derive.Note
c_movement = global_code0_call "movement"
    "Start a new movement with the given title."
    (required "title" "Title of this movement.") $
    \title -> Derive.with_val Constants.v_movement (title :: Text)

c_reminder_accidental :: Make.Calls Derive.Note
c_reminder_accidental = Make.environ_note Module.ly "ly-reminder-accidental"
    mempty "Force this note to display an accidental."
    Constants.v_ly_append_pitch ("!" :: Ly)

c_cautionary_accidental :: Make.Calls Derive.Note
c_cautionary_accidental = Make.environ_note Module.ly "ly-cautionary-accidental"
    mempty "Force this note to display a cautionary accidental."
    Constants.v_ly_append_pitch ("?" :: Ly)

c_tie_direction :: Ly -> Make.Calls Derive.Note
c_tie_direction code = Make.environ_note Module.ly "ly-tie-direction"
    mempty "Force the note's tie to go either up or down."
    Constants.v_ly_tie_direction code

-- I want it to either attach to the end of the first note transformed, or
-- be free-standing but suffix markup.
c_crescendo :: Make.Calls Derive.Note
c_crescendo = make_code_call "ly-crescendo"
    "Start a crescendo hairpin.  If it has non-zero duration, stop the\
    \ crescendo at the event's end, otherwise the crescendo will stop at the\
    \ next hairpin or dynamic marking." Sig.no_args $
    \() -> crescendo_diminuendo "\\<"

c_diminuendo :: Make.Calls Derive.Note
c_diminuendo = make_code_call "ly-diminuendo"
    "Start a diminuendo hairpin.  If it has non-zero duration, stop the\
    \ diminuendo at the event's end, otherwise the diminuendo will stop at the\
    \ next hairpin or dynamic marking." Sig.no_args $
    \() -> crescendo_diminuendo "\\>"

crescendo_diminuendo :: Ly -> Derive.PassedArgs d -> Derive.NoteDeriver
crescendo_diminuendo hairpin args
    -- TODO or is a transformer, I think I should set transformer duration to 0
    | Args.end args > Args.start args = start <> end
    | otherwise = start
    where
    start = code0 (Args.start args) (SuffixFirst, hairpin)
    end = code0 (Args.end args) (SuffixFirst, "\\!")

c_ly_text_above :: Make.Calls Derive.Note
c_ly_text_above = code_call "ly-text-above" "Attach text above the note."
    (required "text" "Text to attach.  Double quotes can be omitted.") $
    return . (,) SuffixFirst . ("^"<>) . lily_str

c_ly_text_below :: Make.Calls Derive.Note
c_ly_text_below = code_call "ly-text-below" "Attach text below the note."
    (required "text" "Text to attach.  Double quotes can be omitted.") $
    (return . (,) SuffixFirst . ("_"<>) . lily_str)

c_ly_begin_slur :: Make.Calls Derive.Note
c_ly_begin_slur = code_call "ly-begin-slur"
    "Begin a slur. The normal slur transformer doesn't work in some cases,\
    \ for instance inside tuplets." Sig.no_args $
    \() -> return (SuffixFirst, "(")

c_ly_end_slur :: Make.Calls Derive.Note
c_ly_end_slur = code_call "ly-end-slur"
    "End a slur. The normal slur transformer doesn't work in some cases,\
    \ for instance inside tuplets." Sig.no_args $
    \() -> return (SuffixLast, ")")

lily_str :: Text -> Ly
lily_str = Types.to_lily

c_ly_pre :: Make.Calls Derive.Note
c_ly_pre = code0_call "ly-pre"
    "Emit arbitrary lilypond code that will go before concurrent notes."
    (required "code" "A leading \\ will be prepended.") $
    \code -> return (Prefix, "\\" <> code)

c_ly_post :: Make.Calls Derive.Note
c_ly_post = code0_call "ly-post"
    "Emit arbitrary lilypond code that will go after concurrent notes."
    (required "code" "A leading \\ will be prepended.") $
    \code -> return (SuffixAll, "\\" <> code)

c_ly_key :: Make.Calls Derive.Note
c_ly_key = code0_call "ly-key"
    "Emit a key change. This only emits a lilypond key change, it doesn't\
    \ actually set the key. This means diatonic operations won't work as\
    \ expected. Also, you have to add it to every staff manually.\
    \ On the up side, it doesn't force a structural change like `=` does."
    (required "key" "You can use any of the keys from the Twelve scale.") $
    \key -> do
        key <- Derive.require_right untxt $ Process.parse_key key
        return (Prefix, Types.to_lily key)

c_ly_sus :: Make.Calls Derive.Note
c_ly_sus = code0_call "ly-sus" "Emit \\sustainOn and \\sustainOff markup."
    (required "state" "t for \\sustainOn, f for \\sustainOff,\
        \ ft for \\sustainOff\\sustainOn.") $
    \state -> case untxt state of
        "f" -> return (SuffixAll, "\\sustainOff")
        "t" -> return (SuffixAll, "\\sustainOn")
        "ft" -> return (SuffixAll, "\\sustainOff\\sustainOn")
        _ -> Derive.throw $ "should be f, t, or ft: " <> untxt state

-- * util

-- | Attach ly code to the first note in the transformed deriver.
code_call :: Text -> Text -> Sig.Parser a -> (a -> Derive.Deriver Code)
    -> Make.Calls Derive.Note
code_call name doc sig make_code = (gen, trans)
    where
    gen = make_call name mempty doc $
        Sig.call sig $ \val args -> do
            code <- make_code val
            -- Code calls mostly apply code to a single note.  It would be
            -- convenient to derive Util.note, but then I'd have to invert, and
            -- since inversion and sub-events are incompatible I would then
            -- have to ignore sub-events.  That in turn would mean I couldn't
            -- split the code calls into a separate track, which is
            -- notationally convenient.
            --
            -- The price is that if I want to put the call on the note track,
            -- I have to append |, which is easy to forget.
            require_nonempty =<< first_note_code code args (place_notes args)
    trans = transformer name mempty doc $
        Sig.callt sig $ \val _args deriver -> flip when_lilypond deriver $ do
            code <- make_code val
            add_first code deriver

require_nonempty :: Derive.Events -> Derive.Deriver Derive.Events
require_nonempty events
    | null (LEvent.events_of events) =
        Derive.throw "this call expects sub-events but none were found"
    | otherwise = return events

-- | Emit a free-standing fragment of lilypond code.
code0_call :: Text -> Text -> Sig.Parser a -> (a -> Derive.Deriver Code)
    -> Make.Calls Derive.Note
code0_call name doc sig make_code =
    make_code_call name (doc <> code0_doc) sig $
        \val args -> code0 (Args.start args) =<< make_code val
    where
    code0_doc = "\nThis either be placed in a separate track as a zero-dur\
        \ event, or it can be attached to an individual note as a transformer."

-- | Just like 'code0_call', but the code uses the 'Constants.ly_global'
-- instrument.
global_code0_call :: Text -> Text -> Sig.Parser a
    -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Make.Calls Derive.Note
global_code0_call name doc sig call =
    make_code_call name doc sig $ \val args ->
        global (call val (Derive.place (Args.start args) 0 Util.note))

-- | Emit a free-standing fragment of lilypond code.
make_code_call :: Text -> Text -> Sig.Parser a
    -> (a -> Derive.PassedArgs Score.Event -> Derive.NoteDeriver)
    -> Make.Calls Derive.Note
make_code_call name doc sig call = (gen, trans)
    where
    gen = make_call name mempty doc $
        Sig.call sig $ \val args -> only_lilypond $
            call val args <> place_notes args
    trans = transformer name mempty doc $
        Sig.callt sig $ \val args deriver ->
            when_lilypond (call val args <> deriver) deriver

global :: Derive.Deriver a -> Derive.Deriver a
global = Derive.with_val_raw Environ.instrument Constants.ly_global

make_call :: Text -> Tags.Tags -> Text -> Derive.WithArgDoc func
    -> Derive.Call func
make_call = Derive.make_call Module.ly

transformer :: Text -> Tags.Tags -> Text
    -> Derive.WithArgDoc (Derive.TransformerFunc d)
    -> Derive.Call (Derive.TransformerFunc d)
transformer = Derive.transformer Module.ly
