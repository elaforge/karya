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
#ifdef TESTING
    , trimmed_controls
#endif
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
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


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("", c_note)
    -- Since you can never call "" with arguments, I need a non-null form
    -- to handle the args version.
    , ("n", c_note)
    , ("=", c_equal)
    , ("note-track", c_note_track)
    ]

-- * note

c_note :: Derive.NoteCall
c_note = note_call "" "" Tags.prelude (default_note use_attributes)

transformed_note :: Text -> Tags.Tags
    -> (Derive.EventDeriver -> Derive.EventDeriver) -> Derive.NoteCall
transformed_note prepend_doc tags transform =
    note_call "" prepend_doc tags (transform . default_note use_attributes)

-- | Create a note call, configuring it with the actual note generating
-- function.  The generator is called with the usual note arguments, and
-- receives the usual instrument and attribute transform.
note_call :: Text
    -- ^ Append to the name, if non-null.  The documentation for all calls that
    -- differ only in name can be grouped together, so it's easier to read
    -- if small modifications are reflected in the name only.  But since
    -- the name is then no longer a valid identifier, it can't be used to set
    -- default arguments.  That's not really a big deal for the note call,
    -- though.
    -> Text -> Tags.Tags -> GenerateNote -> Derive.NoteCall
note_call append_name prepend_doc tags generate = Derive.Call
    { Derive.call_name = "note"
        <> (if Text.null append_name then "" else Text.cons ' ' append_name)
    , Derive.call_generator = Just $ Derive.generator_call tags prepended
        (Sig.call parser (note_generate generate))
    , Derive.call_transformer = Just $ Derive.transformer_call
        (tags <> Tags.subs) transformer_doc (Sig.callt parser note_transform)
    }
    where
    parser = Sig.many "attribute" "Change the instrument or attributes."
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
    transformer_doc =
        "This takes the same arguments as the generator and instead sets"
        <> " those values in the transformed score, similar to the `=`"
        <> " call."
    note_generate generate_note vals = Sub.inverting generate
        where generate args = transform_note vals $ generate_note args

-- | This is implicitly the call for note track titles---the \">...\" will be
-- the first argument.
c_note_track :: Derive.NoteCall
c_note_track =
    Derive.transformer "note-track" Tags.internal
        ("This is used internally as the implicit"
        <> " call for note track titles. Similar to the note transformer, it"
        <> " takes `>inst` and `+attr` args and sets them in the environment.")
    (Sig.callt parser note_transform)
    where parser = Sig.many "attribute" "Change the instrument or attributes."

note_transform :: [Either Score.Instrument Score.Attributes]
    -> Derive.PassedArgs d -> Derive.EventDeriver -> Derive.EventDeriver
note_transform vals _ deriver = transform_note vals deriver

-- ** generate

-- | Generate a single note.  This is intended to be used as the lowest level
-- null call for some instrument.
type GenerateNote = Derive.PassedArgs Score.Event -> Derive.EventDeriver

data Config = Config {
    -- | Note duration is affected by +legato.
    config_legato :: !Bool
    -- | Note duration is affected by +staccato.
    , config_staccato :: !Bool
    -- | Note duration can depend on %sustain and %sustain-abs.
    , config_sustain :: !Bool
    } deriving (Show)

use_attributes :: Config
use_attributes = Config True True True

-- | Don't observe any of the duration affecting attributes.
no_duration_attributes :: Config
no_duration_attributes = Config False False False

default_note :: Config -> GenerateNote
default_note config args = do
    start <- Args.real_start args
    end <- Args.real_end args
    real_next <- Derive.real (Args.next args)
    -- Note that due to negative durations, the end could be before the start.
    -- What this really means is that the sounding duration of the note depends
    -- on the next one, which should be sorted out later by post processing.
    inst <- fromMaybe Score.empty_inst <$>
        Derive.lookup_val Environ.instrument
    environ <- Internal.get_dynamic Derive.state_environ
    let attrs = either (const Score.no_attrs) id $
            TrackLang.get_val Environ.attributes environ
    st <- Derive.gets Derive.state_dynamic
    let controls = trimmed_controls start real_next (Derive.state_controls st)
        pitch_sig = trimmed_pitch start real_next (Derive.state_pitch st)
    (start, end) <- randomized controls start $
        duration_attributes config controls attrs start end real_next
    return $! LEvent.one $! LEvent.Event $! Score.Event
        { Score.event_start = start
        , Score.event_duration = end - start
        , Score.event_bs = Event.event_bytestring (Args.event args)
        , Score.event_controls = controls
        , Score.event_pitch = pitch_sig
        , Score.event_pitches = Derive.state_pitches st
        , Score.event_stack = Derive.state_stack st
        , Score.event_instrument = inst
        , Score.event_environ = environ
        }

-- | Interpret attributes and controls that effect the note's duration.
--
-- This is actually somewhat complicated.  Instead of all the
-- duration-affecting controls all applying together, notes fit into distinct
-- categories:
--
-- - Zero-duration notes ignore all this.
--
-- - Legato notes last until the next note plus 'Controls.legato_overlap'.
--
-- - Staccato notes divide their duration by 2.
--
-- - Normal notes multiply 'Controls.sustain' and add 'Controls.duration_abs',
-- which could be negative.  They clip at a minimum duration to keep from going
-- negative.
duration_attributes :: Config -> Score.ControlMap -> Score.Attributes
    -> RealTime -> RealTime -> RealTime -> RealTime -- ^ new end time
duration_attributes config controls attrs start end next
    | start >= end = end -- don't mess with 0 dur or negative notes
    | config_legato config && has Attrs.legato =
        next + lookup_time 0.1 Controls.legato_overlap
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
    lookup_time deflt control = maybe deflt
        (RealTime.seconds . Signal.at start . Score.typed_val)
        (Map.lookup control controls)
    -- This keeps a negative sustain_abs from making note duration negative.
    min_duration = 0.01

-- | Interpret the c_start_rnd and c_dur_rnd controls.
--
-- This only ever makes notes shorter.  Otherwise, it's very easy for
-- previously non-overlapping notes to become overlapping and MIDI doesn't
-- like that.
randomized :: Score.ControlMap -> RealTime -> RealTime
    -> Derive.Deriver (RealTime, RealTime)
randomized controls start end
    | start_r == 0 && dur_r == 0 = return (start, end)
    | start == end = do
        r1 : _ <- Util.randoms
        let offset = RealTime.seconds (Num.restrict (-start_r/2) (start_r/2) r1)
        return (start + offset, end + offset)
    | otherwise = do
        r1 : r2 : _ <- Util.randoms
        let start2 = start + RealTime.seconds (Num.restrict 0 start_r r1)
        return (start2, max start2 $
            end + RealTime.seconds (Num.restrict (-dur_r) 0 r2))
    where
    start_r = Score.typed_val $
        Score.control_at controls Controls.start_rnd start
    dur_r = Score.typed_val $
        Score.control_at controls Controls.dur_rnd start

-- | In a note track, the pitch signal for each note is constant as soon as
-- the next note begins.  Otherwise, it looks like each note changes pitch
-- during its decay.
trimmed_pitch :: RealTime -> RealTime -> PitchSignal.Signal
    -> PitchSignal.Signal
trimmed_pitch start end
    | start == end = PitchSignal.take 1 . PitchSignal.drop_before start
    | otherwise = PitchSignal.drop_after end . PitchSignal.drop_before start

-- | Trim control signals to the given range.
--
-- Trims will almost all be increasing in time.  Can I save indices or
-- something to make them faster?  That would only work with linear search
-- though.
--
-- It would be nice to strip out deriver-only controls, but without specific
-- annotation I can't know which ones those are.  Presumably laziness will
-- do its thing?
trimmed_controls :: RealTime -> RealTime -> Score.ControlMap
    -> Score.ControlMap
trimmed_controls start end = Map.map (fmap trim)
    where
    trim = if start == end
        -- Otherwise 0 dur events tend to get no controls.
        -- I'm not sure exactly when this happens because 'generate_note' trims
        -- until the start of the next note, but I think it did because
        -- I added this fix for it...
        then Signal.take 1 . Signal.drop_before start
        else Signal.drop_after end . Signal.drop_before start

-- ** transform

transform_note :: [Either Score.Instrument Score.Attributes]
    -> Derive.EventDeriver -> Derive.EventDeriver
transform_note vals deriver =
    with_inst (Util.add_attrs (mconcat attrs) deriver)
    where
    (insts, attrs) = Seq.partition_either vals
    with_inst = maybe id Derive.with_instrument $
        Seq.last $ filter (/=Score.empty_inst) insts


-- * c_equal

c_equal :: Derive.NoteCall
c_equal = Derive.Call
    { Derive.call_name = "equal"
    , Derive.call_generator = Just $ Derive.generator_call Tags.prelude
        ("Similar to the transformer, this will evaluate the notes below in"
            <> " a transformed environ.")
        (Sig.parsed_manually Util.equal_arg_doc generate)
    , Derive.call_transformer = Just $ Derive.transformer_call
        (Tags.prelude <> Tags.subs) Util.equal_doc
        (Sig.parsed_manually Util.equal_arg_doc Util.equal_transformer)
    }
    where
    generate args =
        Sub.place . map (Sub.map_event (Util.equal_transformer args))
        . concat =<< Sub.sub_events args
