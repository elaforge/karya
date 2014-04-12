-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Basic calls for note tracks.
module Derive.Call.Note (
    note_calls
    , c_note, transformed_note, note_call
    , Config(..), use_attributes, no_duration_attributes
    , GenerateNote, default_note
    , adjust_duration
#ifdef TESTING
    , trimmed_controls, min_duration
#endif
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("", c_note)
    -- Since you can never call "" with arguments, I need a non-null form
    -- to handle the args version.
    , ("n", c_note)
    ]
    [ ("n", c_note_attributes "note-attributes")
    -- Called implicitly for note track titles, e.g. '>foo +bar' becomes
    -- 'note-track >foo +bar'.  It has a different name so the stack shows
    -- track calls.
    , ("note-track", c_note_attributes "note-track")
    ]

-- * note

c_note :: Derive.Generator Derive.Note
c_note = note_call "note" "" mempty (default_note use_attributes)

transformed_note :: Text -> Tags.Tags
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
transformed_note prepend_doc tags transform =
    note_call "note" prepend_doc tags $ \args ->
        transform args (default_note use_attributes args)

-- | Create a note call, configuring it with the actual note generating
-- function.  The generator is called with the usual note arguments, and
-- receives the usual instrument and attribute transform.
note_call :: Text
    -- ^ Call name.  The documentation for all calls that differ only in name
    -- can be grouped together, so it's easier to read if small modifications
    -- are reflected in the name only.  If you put invalid identifiers in the
    -- name, it can't be used to set default arguments.  That's not really
    -- a big deal for the note call, though.
    -> Text -> Tags.Tags -> GenerateNote -> Derive.Generator Derive.Note
note_call name prepend_doc tags generate =
    Derive.make_call Module.prelude name tags prepended $
        Sig.call parser (note_generate generate)
    where
    parser = Sig.many "attribute" "Change the instrument or attributes."
    note_generate generate_note vals args =
        Sub.unless_under_inversion args (with_start_controls args) $
            Sub.inverting generate args
        where generate args = transform_note vals $ generate_note args
    prepended
        | Text.null prepend_doc = generator_doc
        | otherwise = "Modified note call: " <> prepend_doc <> "\n"
            <> generator_doc
    generator_doc =
        "The note call is the main note generator, and will emit a single"
        <> " score event. It interprets `>inst` and `+attr` args by"
        <> " setting those fields of the event.  This is bound to the"
        <> " null call, \"\", but any potential arguments would wind up"
        <> " looking like a different call, so it's bound to `n` as well."

-- | Adjust the start time based on controls.
with_start_controls :: Derive.NoteArgs -> Derive.NoteDeriver
    -> Derive.NoteDeriver
with_start_controls args deriver = do
    start <- Args.real_start args
    start_s <- maybe 0 RealTime.seconds <$>
        Derive.untyped_control_at Controls.start_s start
    start_s <- Util.score_duration (Args.start args) start_s
    start_t <- maybe 0 ScoreTime.double <$>
        Derive.untyped_control_at Controls.start_t start

    let dur = Args.duration args
        min_dur = RealTime.to_score min_duration
        offset
            | dur > 0 = min (start_s + start_t) (dur - min_dur)
            | dur == 0 = start_s + start_t
            | otherwise = max (start_s + start_t) (dur + min_dur)
        stretch = case () of
            _ | dur > 0 -> max min_dur (dur - offset)
            _ | dur == 0 -> 1
            _ | otherwise -> max min_dur $ abs (dur - offset)
    if start_s + start_t == 0 then deriver else
        Derive.place (Args.start args + offset) stretch $
            normalize args deriver

normalize :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
normalize args deriver =
    Derive.stretch (if dur == 0 then 1 else 1 / abs dur) $
        Derive.at (- Args.start args) deriver
    where dur = Args.duration args

c_note_attributes :: Text -> Derive.Transformer Derive.Note
c_note_attributes name = Derive.transformer Module.prelude name mempty
    ("This is similar to to `=`, but it takes any number of `>inst` and"
        <> " `+attr` args and sets the `inst` or `attr` environ.")
    (Sig.callt parser note_transform)
    where parser = Sig.many "attribute" "Set instrument or attributes."

note_transform :: [Either Score.Instrument Score.Attributes]
    -> Derive.PassedArgs d -> Derive.NoteDeriver -> Derive.NoteDeriver
note_transform vals _ deriver = transform_note vals deriver

-- ** generate

-- | Generate a single note.  This is intended to be used as the lowest level
-- null call for some instrument.
type GenerateNote = Derive.NoteArgs -> Derive.NoteDeriver

data Config = Config {
    -- | Note duration is affected by +staccato.
    config_staccato :: !Bool
    -- | Note duration can depend on %sustain and %sustain-abs.
    , config_sustain :: !Bool
    } deriving (Show)

use_attributes :: Config
use_attributes = Config True True

-- | Don't observe any of the duration affecting attributes.
no_duration_attributes :: Config
no_duration_attributes = Config False False

-- | The actual note generator.
default_note :: Config -> GenerateNote
default_note config args = do
    start <- Args.real_start args
    end <- Args.real_end args
    real_next <- Derive.real (Args.next args)
    (end, is_arrival) <- adjust_end start end $ Seq.head (Args.next_events args)
    inst <- fromMaybe Score.empty_inst <$> Derive.lookup_val Environ.instrument
    environ <- Internal.get_environ
    let attrs = either (const Score.no_attrs) id $
            TrackLang.get_val Environ.attributes environ
    st <- Derive.gets Derive.state_dynamic

    control_vals <- Derive.controls_at start
    let controls = stash_dynamic control_vals $
            trimmed_controls start real_next (Derive.state_controls st)
        pitch = trimmed_pitch start real_next (Derive.state_pitch st)
    end <- return $ duration_attributes config control_vals attrs start end

    -- Add a attribute to get the arrival-note postproc to figure out the
    -- duration.  Details in "Derive.Call.Post.ArrivalNote".
    let add_arrival = if is_arrival
            then Score.add_attributes Attrs.arrival_note else id
    return $! LEvent.one $! LEvent.Event $! add_arrival $! Score.Event
        { Score.event_start = start
        , Score.event_duration = end - start
        , Score.event_text = Event.event_text (Args.event args)
        , Score.event_controls = controls
        , Score.event_pitch = pitch
        , Score.event_pitches = Derive.state_pitches st
        , Score.event_stack = Derive.state_stack st
        , Score.event_instrument = inst
        , Score.event_environ = environ
        }

-- | Stash the dynamic value from the ControlValMap in
-- 'Controls.dynamic_function'.  Gory details in
-- 'Perform.Midi.Convert.convert_dynamic'.
stash_dynamic :: Score.ControlValMap -> Score.ControlMap -> Score.ControlMap
stash_dynamic vals = maybe id
    (Map.insert Controls.dynamic_function . Score.untyped . Signal.constant)
    (Map.lookup Controls.dynamic vals)

-- | Adjust the end of the event if it has negative duration.  This only works
-- for when the next event start time is known.  At the end of a block the
-- duration stays negative and relies on a postproc to resolve.
--
-- But postproc has to do more work because it has to search for the next event
-- with the same instrument, while this one assumes the next event on the track
-- is the one.
adjust_end :: RealTime -> RealTime -> Maybe Event.Event
    -> Derive.Deriver (RealTime, Bool)
adjust_end start end _ | start <= end = return (end, False)
adjust_end _ end Nothing = return (end, True)
adjust_end start end (Just next) = do
    next_start <- Derive.real (Event.start next)
    next_dur <- subtract start <$> Derive.real (Event.end next)
    let end2 = start + adjust_duration start (end-start) next_start next_dur
    return (end2, False)

adjust_duration :: RealTime -> RealTime -> RealTime -> RealTime -> RealTime
adjust_duration cur_pos cur_dur next_pos next_dur
        -- Departing notes are not changed.
    | cur_dur > 0 = cur_dur
        -- Arriving followed by arriving with a rest in between extends to
        -- the arrival of the rest.
    | next_dur <= 0 && rest > 0 = rest
        -- Arriving followed by arriving with no rest, or an arriving note
        -- followed by a departing note will sound until the next note.
    | otherwise = next_pos - cur_pos
    where rest = next_pos + next_dur - cur_pos

-- | This keeps a negative sustain_abs from making note duration negative.
min_duration :: RealTime
min_duration = 1 / 64

-- | Interpret attributes and controls that effect the note's duration.
--
-- This is actually somewhat complicated.  Instead of all the
-- duration-affecting controls all applying together, notes fit into distinct
-- categories:
--
-- - Zero-duration notes ignore all this.
--
-- - Staccato notes divide their duration by 2.
--
-- - Normal notes multiply 'Controls.sustain' and add 'Controls.duration_abs',
-- which could be negative.  They clip at a minimum duration to keep from going
-- negative.
duration_attributes :: Config -> Score.ControlValMap -> Score.Attributes
    -> RealTime -> RealTime -> RealTime -- ^ new end
duration_attributes config controls attrs start end
    | start >= end = end -- don't mess with 0 dur or negative notes
    | otherwise = start + max min_duration (dur * sustain + sustain_abs)
    where
    has = Score.attrs_contain attrs
    dur = end - start
    staccato = config_staccato config && has Attrs.staccato
    sustain = if staccato then sustain_ * 0.5 else sustain_
    sustain_abs = if staccato || not (config_sustain config)
        then 0 else lookup_time 0 Controls.sustain_abs
    sustain_ = if config_sustain config
        then lookup_time 1 Controls.sustain else 1
    lookup_time deflt control = maybe deflt RealTime.seconds
        (Map.lookup control controls)

-- ** controls

-- | In a note track, the pitch signal for each note is constant as soon as
-- the next note begins.  Otherwise, it looks like each note changes pitch
-- during its decay.
trimmed_pitch :: RealTime -> RealTime -> PitchSignal.Signal
    -> PitchSignal.Signal
trimmed_pitch start end
    | end < start = maybe mempty PitchSignal.constant . PitchSignal.at start
    | start == end = PitchSignal.take 1 . PitchSignal.drop_before start
    | otherwise = PitchSignal.drop_after end . PitchSignal.drop_before start

-- | Trim control signals to the given range.
--
-- Trims will almost all be increasing in time.  Can I save indices or
-- something to make them faster?  That would only work with linear search
-- though.
trimmed_controls :: RealTime -> RealTime -> Score.ControlMap
    -> Score.ControlMap
trimmed_controls start end = Map.map (fmap trim)
    where
    trim
        -- An arrival note at the end of a block doesn't know how long it should
        -- be, so it remains negative.  Since it doesn't have its final
        -- duration, it doesn't know it can't get the proper controls.  In
        -- any case, those controls are in the next block, so they're
        -- unavailable.
        | end < start = Signal.constant . Signal.at start
        -- Otherwise 0 dur events tend to get no controls.
        -- I'm not sure exactly when this happens because 'generate_note' trims
        -- until the start of the next note, but I think it did because
        -- I added this fix for it...
        -- TODO figure it out, add a test
        | start == end = Signal.take 1 . Signal.drop_before start
        | otherwise = Signal.drop_after end . Signal.drop_before start

-- ** transform

transform_note :: [Either Score.Instrument Score.Attributes]
    -> Derive.NoteDeriver -> Derive.NoteDeriver
transform_note vals deriver =
    with_inst (Util.add_attrs (mconcat attrs) deriver)
    where
    (insts, attrs) = Seq.partition_either vals
    with_inst = maybe id Derive.with_instrument $
        Seq.last $ filter (/=Score.empty_inst) insts

