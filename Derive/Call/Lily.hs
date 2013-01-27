-- | Utilities for calls to cooperate with the lilypond backend.
module Derive.Call.Lily where
import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Pitch as Pitch

import Types


when_lilypond :: (Lilypond.Config -> Derive.Deriver a)
    -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
when_lilypond lily not_lily =
    maybe not_lily lily =<< Derive.lookup_lilypond_config

append :: Derive.PassedArgs d -> String -> Derive.EventDeriver
    -> Derive.EventDeriver
append args code = when_lilypond $
    const $ append_code code $ Util.place args Util.note

-- * note transformer

-- | Replace a note transformer with one that derives its children as-is,
-- but add lilypond code.
notes_code :: Code -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_code code = notes_with (add_code code)

notes_append :: String -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_append = notes_code . Suffix

-- | This is like 'notes_code', but the first event in each track gets the
-- start code, and the last event in each track gets the end code.
notes_around :: Code -> Code -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_around start end args = when_lilypond $
    const $ Note.place $ concat $ map around $ Note.sub_events args
    where
    around = Seq.first_last (Note.map_event (add_code start))
        (Note.map_event (add_code end))

-- | Like 'notes_around', but when I'm not in lilypond mode just derive the
-- sub events unchanged.
code_around :: Code -> Code -> Derive.PassedArgs d -> Derive.EventDeriver
code_around start end args = when_lilypond
        (const $ code0 (Args.start args) start
            <> place_notes args <> code0 (Args.end args) end)
        (place_notes args)

notes_with :: (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_with f args = when_lilypond $
    const $ Note.place $ Note.map_events f $ concat (Note.sub_events args)

place_notes :: Derive.PassedArgs d -> Derive.EventDeriver
place_notes = Note.place . concat . Note.sub_events

-- * code

-- | Either prepend or append some code to a lilypond note.
data Code = Prefix String | Suffix String deriving (Show)

prepend_code :: String -> Derive.EventDeriver -> Derive.EventDeriver
prepend_code = add_code . Prefix

append_code :: String -> Derive.EventDeriver -> Derive.EventDeriver
append_code = add_code . Suffix

add_code :: Code -> Derive.EventDeriver -> Derive.EventDeriver
add_code (Prefix code) = Derive.modify_val Lilypond.v_ly_prepend $
    (code++) . fromMaybe ""
add_code (Suffix code) = Derive.modify_val Lilypond.v_ly_append $
    (++code) . fromMaybe ""

-- | Emit a note that carries raw lilypond code.  The code is emitted
-- literally, and assumed to have the duration of the event.  The event's pitch
-- is ignored.  This can be used to emit lilypond that doesn't fit into
-- a 'Lilypond.Event'.
code :: (ScoreTime, ScoreTime) -> String -> Derive.EventDeriver
code (start, dur) code = Derive.with_val Lilypond.v_ly_prepend code
    (Derive.d_place start dur Util.note)

-- | Like 'code', but for 0 duration code fragments, and can either put them
-- before or after notes that occur at the same time.
code0 :: ScoreTime -> Code -> Derive.EventDeriver
code0 start code = with (Derive.d_place start 0 Util.note)
    where
    with = case code of
        Prefix c -> Derive.with_val Lilypond.v_ly_prepend c
        Suffix c -> Derive.with_val Lilypond.v_ly_append c

-- * convert

-- | Round the RealTime to the nearest NoteDuration.
note_duration :: Lilypond.Config -> RealTime -> Lilypond.NoteDuration
note_duration config = Lilypond.time_to_note_dur . to_time config

-- | Like 'note_duration', but only succeeds if the RealTime is exactly
-- a NoteDuration.
is_note_duration :: Lilypond.Config -> RealTime
    -> Maybe Lilypond.NoteDuration
is_note_duration config = Lilypond.is_note_dur . to_time config

is_duration :: Lilypond.Config -> RealTime -> Maybe Lilypond.Duration
is_duration config t = case is_note_duration config t of
    Just (Lilypond.NoteDuration dur False) -> Just dur
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
    right $ Lilypond.show_pitch pitch
    where
    require = Derive.require . (prefix <>)
    right :: (Pretty.Pretty a) => Either a b -> Derive.Deriver b
    right = Derive.require_right ((prefix <>) . Pretty.pretty)
    prefix = "Lily.pitch_to_lily: "

to_time :: Lilypond.Config -> RealTime -> Lilypond.Time
to_time = Lilypond.real_to_time . Lilypond.config_quarter_duration


-- * eval

-- | A lilypond \"note\", which is just a chunk of text.
type Note = String

eval :: Lilypond.Config -> Derive.PassedArgs d -> [Note.Event]
    -> Derive.Deriver [Note]
eval config args notes = do
    start <- Args.real_start args
    eval_events config start =<< Note.place notes

eval_events :: Lilypond.Config -> RealTime -> Derive.Events
    -> Derive.Deriver [Note]
eval_events config start events = do
    meter <- maybe (return Meter.default_meter) parse_meter
        =<< Derive.lookup_val Lilypond.v_meter
    let (notes, logs) = eval_notes config meter start events
    mapM_ Log.write logs
    return notes
    where
    parse_meter = either err return . Meter.parse_meter
    err = Derive.throw . ("parse " <> Pretty.pretty Lilypond.v_meter <>)

eval_notes :: Lilypond.Config -> Meter.Meter -> RealTime -> Derive.Events
    -> ([Note], [Log.Msg])
eval_notes config meter start score_events =
    (map Lilypond.to_lily notes, logs)
    where
    (events, logs) = LEvent.partition $ Convert.convert quarter score_events
    notes = Lilypond.simple_convert config meter
        (Lilypond.real_to_time quarter start)
        (Convert.quantize (Lilypond.config_quantize config) events)
    quarter = Lilypond.config_quarter_duration config


-- * calls

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("when-ly", c_when_ly)
    , ("8va", c_8va)
    , ("xstaff", c_xstaff)
    , ("dyn", c_dyn)
    ]

c_when_ly :: Derive.NoteCall
c_when_ly = Derive.transformer "when-ly"
    "Evaluate the deriver only when in lilypond mode. Apply this to a track\
    \ to make non-lilypond derivation a bit more efficient."
    $ Sig.call0t $ \_args deriver -> when_lilypond (const deriver) mempty

c_8va :: Derive.NoteCall
c_8va = Derive.stream_generator "ottava"
    "Emit `lilypond \\ottava = #n` around the notes in scope."
    $ Sig.call (defaulted "octave" 1 "Transpose this many octaves up or down.")
    $ \oct args -> code_around (Prefix (ottava oct)) (Prefix (ottava 0)) args

ottava :: Int -> String
ottava n = "\\ottava #" ++ show n

c_xstaff :: Derive.NoteCall
c_xstaff = Derive.stream_generator "xstaff"
    "Emit lilypond to put the notes on a different staff."
    $ Sig.call (required "staff" "Should be `up` or `down`.") $
    \staff args -> do
        (staff1, staff2) <- case staff of
            "up" -> return ("up", "down")
            "down" -> return ("down", "up")
            _ -> Derive.throw $ "expected 'up' or 'down', got " <> show staff
        code_around (Prefix (change staff1)) (Prefix (change staff2)) args
    where change staff = "\\change Staff = " <> Lilypond.to_lily staff

c_dyn :: Derive.NoteCall
c_dyn = Derive.stream_generator "dyn"
    "Emit a lilypond dynamic. If there are notes below, they are derived\
    \ unchanged."
    $ Sig.call (required "dynamic" "Should be `p`, `ff`, etc.") $
    \dyn args -> code0 (Args.start args) (Suffix ('\\' : dyn))
        <> place_notes args
