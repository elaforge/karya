-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for calls to cooperate with the lilypond backend.
module Derive.Call.Ly where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Types as Types
import qualified Perform.RealTime as RealTime

import Global
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
only_lilypond deriver = ifM Derive.is_lilypond_mode deriver mempty

-- | When in lilypond mode, generate a note with the given Code.
note_code :: Code -> Derive.PassedArgs d -> Derive.NoteDeriver
    -> Derive.NoteDeriver
note_code code args = when_lilypond $ add_code code $ Call.placed_note args

-- ** transformer

-- | Add code to the first event.
add_first :: Code -> Derive.NoteDeriver -> Derive.NoteDeriver
add_first code deriver =
    Post.map_first (return . add_event_code code) =<< deriver

-- ** note parent

-- | Replace a note parent with one that derives its sub-events as-is
-- and adds lilypond code to them.
notes_code :: Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_code code = notes_with (add_code code)

-- | Like 'notes_code', but only apply the code to the first event, not all of
-- them.
first_note_code :: Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
first_note_code code args = when_lilypond $
    add_first code $ derive_notes args

-- | This is like 'notes_code', but the first event in each track gets the
-- start code, and the last event in each track gets the end code.
notes_around :: Code -> Code -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_around start end args = when_lilypond $
    mconcatMap around =<< Sub.sub_events args
    where
    around notes = first_last
        (add_event_code start) (add_event_code end) <$> Sub.derive notes

-- | Like 'notes_around', but for use when you already know you're in lilypond
-- mode.
notes_around_ly :: Code -> Code -> Derive.PassedArgs d -> Derive.NoteDeriver
notes_around_ly start end = mconcatMap around <=< Sub.sub_events
    where
    around notes = first_last
        (add_event_code start) (add_event_code end) <$> Sub.derive notes

-- | Like 'notes_around', but when I'm not in lilypond mode just derive the
-- sub events unchanged.
code_around :: Code -> Code -> Derive.PassedArgs d -> Derive.NoteDeriver
code_around start end args = when_lilypond
    (code0 (Args.start args) start
        <> derive_notes args <> code0 (Args.end args) end)
    (derive_notes args)

-- | Transform and evaluate the sub events.
notes_with :: (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.PassedArgs d
    -> Derive.NoteDeriver -> Derive.NoteDeriver
notes_with f args = when_lilypond $
    Sub.derive . map (fmap f) . concat =<< Sub.sub_events args

derive_notes :: Derive.PassedArgs d -> Derive.NoteDeriver
derive_notes = Sub.derive . concat <=< Sub.sub_events

-- | Like 'Seq.first_last', but applied to a Stream.  If the events start or
-- end with a group of events with the same start time, the start or end
-- function is applied to the entire group.  This is because the lilypond
-- performer will group them into a chord and will only take ly-prepend and
-- ly-append from the first note in the chord.  I could apply to only the first
-- element of the group, but that would rely on every sort being stable.  Which
-- they probably are, but it seems brittle.
first_last :: (Score.Event -> Score.Event) -> (Score.Event -> Score.Event)
    -> Stream.Stream Score.Event -> Stream.Stream Score.Event
first_last start end xs =
    Stream.merge_logs logs $ Stream.from_sorted_events $ concat $
        Seq.first_last (map start) (map end) (List.groupBy cmp events)
    where
    (events, logs) = Stream.partition xs
    cmp x y = Score.event_start x RealTime.== Score.event_start y

-- ** code

-- | Either prepend or append some code to a lilypond note.
type Code = (CodePosition, Ly)
data CodePosition =
    -- | Code goes before the note.
    Prepend
    -- | Code goes after each note in a tied sequence, so it could get
    -- duplicated several times.
    | AppendAll
    -- | Like AppendAll, but it goes after notes inside a chord, instead of
    -- once after the chord itself.
    | NoteAppendAll
    -- | Code goes after only the first note in a tied sequence.
    | AppendFirst | NoteAppendFirst
    -- | Code goes after the last note in a tied sequnece.
    | AppendLast
    -- | Create a note with the given env value set to the Ly code.  This is
    -- for directives to the lilypond performer, not to lilypond itself.
    -- E.g. 'Constants.v_subdivision'.
    | SetEnviron !Env.Key
    deriving (Eq, Show)

instance Pretty CodePosition where
    pretty (SetEnviron key) = "SetEnviron " <> pretty key
    pretty p = showt p

instance Typecheck.Typecheck CodePosition
instance Typecheck.TypecheckSymbol CodePosition where
    parse_symbol = (`Map.lookup` code_position_names) . Expr.unstr
    symbol_values _ = Just $ Map.keys code_position_names

instance Typecheck.ToVal CodePosition

instance ShowVal.ShowVal CodePosition where
    show_val = TextUtil.dropPrefix "ly-" . position_env False

code_position_names :: Map Text CodePosition
code_position_names = Map.fromList $ Seq.key_on ShowVal.show_val
    [ Prepend
    , AppendAll, NoteAppendAll, AppendFirst, NoteAppendFirst, AppendLast
    ]

-- | Fragment of Lilypond code.
type Ly = Text

-- | A lilypond \"note\", which is just a chunk of text.
type Note = Ly

position_env :: Bool -- ^ True if this is a zero-dur event created just to
    -- host some ly code.  See 'Flags.ly_code'.
    -> CodePosition -> Env.Key
position_env zero_dur p = case if zero_dur then code0 p else p of
    Prepend -> Constants.v_prepend
    AppendAll -> Constants.v_append_all
    NoteAppendAll -> Constants.v_note_append_all
    AppendFirst -> Constants.v_append_first
    NoteAppendFirst -> Constants.v_note_append_first
    AppendLast -> Constants.v_append_last
    SetEnviron key -> key
    where
    -- AppendFirst and AppendLast are not used for 0 dur events, so make it
    -- less error-prone by getting rid of them.  Ick.
    code0 pos = case pos of
        AppendFirst -> AppendAll
        AppendLast -> AppendAll
        _ -> pos

prepend_code :: Ly -> Derive.NoteDeriver -> Derive.NoteDeriver
prepend_code = add_code . (,) Prepend

-- | Emit a note that carries raw lilypond code.  The code is emitted
-- literally, and assumed to have the duration of the event.  The event's pitch
-- is ignored.  This can be used to emit lilypond that doesn't fit into
-- a 'Types.Event', but still has a duration.
code :: (ScoreTime, ScoreTime) -> Ly -> Derive.NoteDeriver
code (start, dur) code = Derive.with_val Constants.v_prepend code $
    Derive.remove_pitch $ Derive.place start dur Call.note

-- | Like 'code', but for 0 duration code fragments, and can either put them
-- before or after notes that occur at the same time.
code0 :: ScoreTime -> Code -> Derive.NoteDeriver
code0 start code = add_code code $ Derive.place start 0 Call.note

-- | Make a code0 event directly.  Inherit instrument and environ from an
-- existing note.  Otherwise, the lilypond backend doesn't know how to group
-- the code event.
code0_event :: Score.Event -> RealTime -> Code -> Score.Event
code0_event event start code = add_event_code code $ Score.empty_event
    { Score.event_start = start
    , Score.event_text = snd code
    , Score.event_stack = Score.event_stack event
    , Score.event_instrument = Score.event_instrument event
    }

global_code0 :: ScoreTime -> Ly -> Derive.NoteDeriver
global_code0 start code = global $ code0 start (Prepend, code)

-- | Derive with the 'Constants.ly_global' instrument.
global :: Derive.Deriver a -> Derive.Deriver a
global = Derive.with_val_raw EnvKey.instrument Constants.ly_global

-- | Test if an event is a 0 duration lilypond code event.
is_code0 :: Score.Event -> Bool
is_code0 event = Score.event_duration event == 0
    && Flags.has (Score.event_flags event) Flags.ly_code

-- *** low level

-- | Add lilypond code to a note.  This will skip 'is_code0' events, which
-- are supposed to be note-independent bits of lilypond code, and not actually
-- notes.  If the duration is 0, it assume's that you're making one of those
-- 0 dur events, and adds 'Flags.ly_code'.
add_event_code :: Code -> Score.Event -> Score.Event
add_event_code (pos, code) event
    | is_code0 event = event
    | otherwise = (if zero_dur then Score.add_flags Flags.ly_code else id) $
        Score.modify_environ (add (position_env zero_dur pos) (<>code)) event
    where
    zero_dur = Score.event_duration event == 0
    add name f env = Env.insert_val name (Typecheck.to_val (f old)) env
        where old = fromMaybe "" $ Env.maybe_val name env

add_code :: Code -> Derive.NoteDeriver -> Derive.NoteDeriver
add_code code = fmap $ Post.emap1_ (add_event_code code)

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
    event <- require "had no event" $ Seq.head (Stream.events_of events)
    env <- Derive.get_environ
    pitch_to_lily env
        =<< require "note had no pitch" (Score.initial_pitch event)
    -- Wow, there are a lot of ways to fail.
    where
    require = Derive.require . (prefix <>)
    prefix = "Ly.note_pitch: "

pitch_to_lily :: Env.Environ -> PSignal.Transposed -> Derive.Deriver Note
pitch_to_lily env = fmap Types.to_lily
    . Derive.require_right ("Ly.pitch_to_lily: "<>) . Convert.pitch_to_lily env

to_time :: Types.Config -> RealTime -> Types.Time
to_time = Types.real_to_time . Types.config_quarter_duration
