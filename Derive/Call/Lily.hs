-- | Utilities for calls to cooperate with the lilypond backend.
module Derive.Call.Lily where
import qualified Data.List as List

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Theory as Theory
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
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import Types


-- * utils for ly calls

when_lilypond :: (Types.Config -> Derive.Deriver a)
    -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
when_lilypond lily not_lily =
    maybe not_lily lily =<< Derive.lookup_lilypond_config

-- | Only emit the deriver if I'm in lilypond mode.
only_lilypond :: Derive.EventDeriver -> Derive.EventDeriver
only_lilypond deriver = ifM Derive.is_lilypond_derive deriver mempty

-- | When in lilypond mode, generate a note with the given Code.
note_code :: Code -> Derive.PassedArgs d -> Derive.EventDeriver
    -> Derive.EventDeriver
note_code code args = when_lilypond $
    const $ add_code code $ Util.place args Util.note

-- ** transformer

-- | Add code to the first event.
add_first :: Code -> Derive.EventDeriver -> Derive.EventDeriver
add_first code deriver =
    Util.map_first (return . add_event_code code) =<< deriver

-- ** note transformer

-- | Replace a note transformer with one that derives its children as-is,
-- but add lilypond code.
notes_code :: Code -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_code code = notes_with (add_code code)

-- | This is like 'notes_code', but the first event in each track gets the
-- start code, and the last event in each track gets the end code.
notes_around :: Code -> Code -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_around start end args = when_lilypond $ const $
    mconcat . map around =<< Note.sub_events args
    where
    around notes = first_last
        (add_event_code start) (add_event_code end) <$> Note.place notes

-- | Like 'notes_around', but for use when you already know you're in lilypond
-- mode.
notes_around_ly :: Code -> Code -> Derive.PassedArgs d -> Derive.EventDeriver
notes_around_ly start end = mconcat . map around <=< Note.sub_events
    where
    around notes = first_last
        (add_event_code start) (add_event_code end) <$> Note.place notes

-- | Like 'notes_around', but when I'm not in lilypond mode just derive the
-- sub events unchanged.
code_around :: Code -> Code -> Derive.PassedArgs d -> Derive.EventDeriver
code_around start end args = when_lilypond
    (const $ code0 (Args.start args) start
        <> place_notes args <> code0 (Args.end args) end)
    (place_notes args)

-- | Transform and evaluate the sub events.
notes_with :: (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_with f args = when_lilypond $ const $
    Note.place . Note.map_events f . concat =<< Note.sub_events args

place_notes :: Derive.PassedArgs d -> Derive.EventDeriver
place_notes = Note.place . concat <=< Note.sub_events

-- ** events around

add_event_code :: Code -> Score.Event -> Score.Event
add_event_code (pos, code) =
    Score.modify_environ $ add (position_env pos) (++code)
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
type Code = (CodePosition, String)
data CodePosition =
    -- | Code goes before the note.
    Prefix
    -- | Code goes after all the notes in a tied sequence.
    | SuffixAll
    -- | Code goes after only the first note in a tied sequence.
    | SuffixFirst
    -- | Code goes after the last note in a tied sequnece.
    | SuffixLast
    deriving (Show)

position_env :: CodePosition -> TrackLang.ValName
position_env c = case c of
    Prefix -> Constants.v_ly_prepend
    SuffixFirst -> Constants.v_ly_append_first
    SuffixLast -> Constants.v_ly_append_last
    SuffixAll -> Constants.v_ly_append_all

prepend_code :: String -> Derive.EventDeriver -> Derive.EventDeriver
prepend_code = add_code . (,) Prefix

add_code :: Code -> Derive.EventDeriver -> Derive.EventDeriver
add_code (pos, code) = Derive.modify_val (position_env pos) $
    (++code) . fromMaybe ""

-- | Emit a note that carries raw lilypond code.  The code is emitted
-- literally, and assumed to have the duration of the event.  The event's pitch
-- is ignored.  This can be used to emit lilypond that doesn't fit into
-- a 'Types.Event'.
code :: (ScoreTime, ScoreTime) -> String -> Derive.EventDeriver
code (start, dur) code = Derive.with_val Constants.v_ly_prepend code
    (Derive.d_place start dur Util.note)

-- | Like 'code', but for 0 duration code fragments, and can either put them
-- before or after notes that occur at the same time.
code0 :: ScoreTime -> Code -> Derive.EventDeriver
code0 start (pos_, code) = with (Derive.d_place start 0 Util.note)
    where
    -- SuffixFirst and SuffixLast are not used for 0 dur events, so make it
    -- less error-prone by getting rid of them.  Ick.
    pos = case pos_ of
        SuffixFirst -> SuffixAll
        SuffixLast -> SuffixAll
        _ -> pos_
    with = Derive.with_val (position_env pos) code

global_code0 :: ScoreTime -> Code -> Derive.EventDeriver
global_code0 start = global . code0 start

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

note_pitch :: Derive.EventDeriver -> Derive.Deriver String
note_pitch deriver = do
    events <- deriver
    event <- require "had no event" $ Seq.head (LEvent.events_of events)
    pitch <- require "note had no pitch" $ Score.initial_pitch event
    let controls = Score.event_controls_at (Score.event_start event) event
    pitch_to_lily $ PitchSignal.apply controls pitch
    -- Wow, there are a lot of ways to fail.
    where
    require = Derive.require . (prefix <>)
    prefix = "Lily.note_pitch: "

pitch_to_lily :: PitchSignal.Pitch -> Derive.Deriver String
pitch_to_lily pitch = do
    note <- right $ PitchSignal.pitch_note pitch
    pitch <- require ("unparseable note: " <> Pretty.pretty note) $
        Theory.parse_pitch (Pitch.note_text note)
    right $ Types.show_pitch pitch
    where
    require = Derive.require . (prefix <>)
    right :: (Pretty.Pretty a) => Either a b -> Derive.Deriver b
    right = Derive.require_right ((prefix <>) . Pretty.pretty)
    prefix = "Lily.pitch_to_lily: "

to_time :: Types.Config -> RealTime -> Types.Time
to_time = Types.real_to_time . Types.config_quarter_duration


-- ** eval

-- | A lilypond \"note\", which is just a chunk of text.
type Note = String

eval :: Types.Config -> Derive.PassedArgs d -> [Note.Event]
    -> Derive.Deriver [Note]
eval config args notes = do
    start <- Args.real_start args
    eval_events config start =<< Note.place notes

eval_events :: Types.Config -> RealTime -> Derive.Events
    -> Derive.Deriver [Note]
eval_events config start events = do
    meter <- maybe (return Meter.default_meter) parse_meter
        =<< Derive.lookup_val Constants.v_meter
    let (notes, logs) = eval_notes config meter start events
    mapM_ Log.write logs
    return notes
    where
    parse_meter = either err return . Meter.parse_meter
    err = Derive.throw . ("parse " <> Pretty.pretty Constants.v_meter <>)

eval_notes :: Types.Config -> Meter.Meter -> RealTime -> Derive.Events
    -> ([Note], [Log.Msg])
eval_notes config meter start score_events = (map Types.to_lily notes, logs)
    where
    (events, logs) = LEvent.partition $ Convert.convert quarter score_events
    notes = Process.simple_convert config meter
        (Types.real_to_time quarter start)
        (Convert.quantize (Types.config_quantize config) events)
    quarter = Types.config_quarter_duration config


-- * calls

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("when-ly", c_when_ly)
    , ("unless-ly", c_unless_ly)
    , ("ly-global", c_ly_global)
    , ("is-ly", c_is_ly)
    , ("not-ly", c_not_ly)
    , ("if-ly", c_if_ly)
    , ("8va", c_8va)
    , ("xstaff", c_xstaff)
    , ("dyn", c_dyn)
    , ("clef", c_clef)
    , ("meter", c_meter)
    , ("ly-!", c_reminder_accidental)
    , ("ly-?", c_cautionary_accidental)
    ]

c_when_ly :: Derive.NoteCall
c_when_ly = Derive.transformer "when-ly" Tags.ly_only
    "Evaluate the deriver only when in lilypond mode.  Unlike is-ly,\
    \ this doesn't evaluate subtracks, so you can use it to emit an entirely\
    \ different set of tracks."
    $ Sig.call0t $ \_ deriver -> when_lilypond (const deriver) mempty

c_unless_ly :: Derive.NoteCall
c_unless_ly = Derive.transformer "unless-ly" Tags.ly_only
    "The reverse of when-ly, evaluate the deriver only when not in lilypond\
    \ mode."
    $ Sig.call0t $ \_ deriver -> when_lilypond (const mempty) deriver

c_ly_global :: Derive.NoteCall
c_ly_global = Derive.transformer "ly-global" Tags.ly_only
    ("Evaluate the deriver only when in lilypond mode, like 'when-ly', but\
    \ also set the " <> ShowVal.show_val Constants.ly_global <> " instrument."
    ) $ Sig.call0t $ \_ deriver ->
        when_lilypond (const (global deriver)) mempty

-- | TODO it's ugly how this only works in the track title.  If applied to
-- an event, it will emit a duplicate copy of the tracks below it, which is
-- definitely not useful.  Ways around this would be:
--
-- - Always slice the subtracks, but the track title call gives the range of
-- the whole track.  This would work but would cause lots of unnecessary
-- slicing.
--
-- - Add a in_track_title flag to CallInfo, so this can switch on it.  Hacky
-- and ad-hoc.
--
-- - Provide a way for custom track-level calls, e.g. EventNode ->
-- EventDeriver.  I might want to do this eventually anyway for tracks with
-- their own little custom language.  But if I do, I also have to support
-- documentation, lookup in some namespace, and will probably want to add block
-- calls (presumably derive_tree :: ScoreTime -> TrackTree.EventsTree ->
-- Derive.EventDeriver) too, so it's a bit of work.
c_is_ly :: Derive.NoteCall
c_is_ly = Derive.transformer "is-ly" Tags.ly_only
    "Evaluate the deriver only when in lilypond mode, otherwise ignore this\
    \ track but evaluate its subtracks. Apply this to a track \
    \ to omit lilypond-only articulations, or to apply different articulations\
    \ to lilypond and non-lilypond output. Only use it in the track title!"
    $ Sig.call0t $ \args deriver ->
        when_lilypond (const deriver) (derive_subtracks args)

c_not_ly :: Derive.NoteCall
c_not_ly = Derive.transformer "not-ly" Tags.ly_only
    "The inverse of `is-ly`, evaluate the track only when not in lilypond\
    \ mode. Only use it in the track title!"
    $ Sig.call0t $ \args deriver ->
        when_lilypond (const $ derive_subtracks args) deriver

c_if_ly :: Derive.NoteCall
c_if_ly = Derive.stream_generator "if-ly" Tags.ly_only
    "Conditional for lilypond." $ Sig.call ((,)
    <$> required "is-ly" "Evaluated in lilypond mode."
    <*> required "not-ly" "Evaluated when not in lilypond mode."
    ) $ \(is_ly, not_ly) args -> when_lilypond
        (const (Call.reapply_string args is_ly))
        (Call.reapply_string args not_ly)

derive_subtracks :: Derive.PassedArgs d -> Derive.EventDeriver
derive_subtracks =
    BlockUtil.derive_tracks . Derive.info_sub_tracks . Derive.passed_info

c_8va :: Derive.NoteCall
c_8va = code0_call "ottava" "Emit lilypond ottava mark."
    (required "octave" "Transpose this many octaves up or down.") $
    \oct -> return (Prefix, ottava oct)
    where
    ottava :: Int -> String
    ottava n = "\\ottava #" ++ show n

c_xstaff :: Derive.NoteCall
c_xstaff = code0_call "xstaff"
    "Emit lilypond to put the notes on a different staff."
    (required "staff" "Should be `up` or `down`.") $ \staff -> do
        when (staff `notElem` ["down", "up"]) $
            Derive.throw $ "expected 'up' or 'down', got "
                <> ShowVal.show_val staff
        return (Prefix, change staff)
    where change staff = "\\change Staff = " <> Types.to_lily staff

c_dyn :: Derive.NoteCall
c_dyn = code0_call "dyn"
    "Emit a lilypond dynamic. If there are notes below, they are derived\
    \ unchanged."
    (required "dynamic" "Should be `p`, `ff`, etc.")
    (return . (,) SuffixAll . ('\\':))

c_clef :: Derive.NoteCall
c_clef = code0_call "clef" "Emit lilypond clef change."
    (required "clef" "Should be `bass`, `treble`, etc.")
    (return . (,) Prefix . ("\\clef "++))

c_meter :: Derive.NoteCall
c_meter = global_code0_call "meter"
    "Emit lilypond meter change. It will be interpreted as global no matter\
    \ where it is. Simultaneous different meters aren't supported yet."
    (required "meter" "Should be `4/4`, `3+3/8`, etc.") $
    \pos val -> Derive.with_val Constants.v_meter (val :: String) $
        Derive.d_place pos 0 Util.note

c_reminder_accidental :: Derive.NoteCall
c_reminder_accidental = Make.environ_note "ly-reminder-accidental"
    Tags.ly_only "Force this note to display an accidental."
    Constants.v_ly_append_pitch "!"

c_cautionary_accidental :: Derive.NoteCall
c_cautionary_accidental = Make.environ_note "ly-cautionary-accidental"
    Tags.ly_only "Force this note to display a cautionary accidental."
    Constants.v_ly_append_pitch "?"

-- * util

-- | Attach ly code to the first note in the transformed deriver.
code_call :: String -> String -> Sig.Parser a -> (a -> Derive.Deriver Code)
    -> Derive.NoteCall
code_call name doc sig make_code = Derive.transformer name Tags.ly_only doc $
    Sig.callt sig $ \val _ deriver -> flip when_lilypond deriver $ const $ do
        code <- make_code val
        add_first code deriver

-- | Emit a free-standing fragment of lilypond code.
code0_call :: String -> String -> Sig.Parser a -> (a -> Derive.Deriver Code)
    -> Derive.NoteCall
code0_call name doc sig make_code =
    make_code0_call name (doc <> code0_doc) sig $
        \pos val -> code0 pos =<< make_code val
    where
    code0_doc = "\nThis either be placed in a separate track as a zero-dur\
        \ event, or it can be attached to an individual note as a transformer."

-- | Just like 'code0_call', but the code uses the 'Constants.ly_global'
-- instrument.
global_code0_call :: String -> String -> Sig.Parser a
    -> (ScoreTime -> a -> Derive.EventDeriver) -> Derive.NoteCall
global_code0_call name doc sig call =
    make_code0_call name doc sig $ \pos val -> global (call pos val)

-- | Emit a free-standing fragment of lilypond code.
make_code0_call :: String -> String -> Sig.Parser a
    -> (ScoreTime -> a -> Derive.EventDeriver) -> Derive.NoteCall
make_code0_call name doc sig call = Derive.Call
    { Derive.call_name = name
    , Derive.call_generator = Just $
        Derive.generator_call Tags.ly_only doc generator
    , Derive.call_transformer = Just $
        Derive.transformer_call Tags.ly_only doc transformer
    }
    where
    generator = Sig.call sig $ \val args -> only_lilypond $
        call (Args.start args) val <> place_notes args
    transformer = Sig.callt sig $ \val args deriver ->
        when_lilypond (const $ call (Args.start args) val <> deriver) deriver

global :: Derive.Deriver a -> Derive.Deriver a
global = Derive.with_val_raw TrackLang.v_instrument Constants.ly_global
