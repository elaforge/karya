-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic calls for note tracks.
module Derive.Call.Prelude.Note (
    note_calls
    , c_note, c_note_attributes, transformed_note, note_call
    , Config(..), use_attributes, no_duration_attributes
    , GenerateNote, default_note, note_flags, make_event
    , adjust_duration
    , min_duration
) where
import qualified Data.Either as Either
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("", c_note)
    -- Since you can never call "" with arguments, I need a non-null form
    -- to handle the args version.
    , ("n", c_note)
    ]
    [ ("n", c_note_attributes)
    -- Called implicitly for note track titles, e.g. '>foo +bar' becomes
    -- 'note-track >foo +bar'.
    , (ParseTitle.note_track_symbol, c_note_track)
    ]

-- * note

c_note :: Derive.Generator Derive.Note
c_note = note_call "note" "" mempty (default_note use_attributes)

-- | Create a standard note call with a transformer applied.
transformed_note :: Derive.Doc -> Tags.Tags
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
transformed_note prepend_doc tags transform =
    note_call "note" prepend_doc tags $ \args ->
        transform args (default_note use_attributes args)

-- | Create a note call, configuring it with the actual note generating
-- function.  The generator is called with the usual note arguments, and
-- receives the usual instrument and attribute transform.
note_call :: Derive.CallName
    -- ^ Call name.  The documentation for all calls that differ only in name
    -- can be grouped together, so it's easier to read if small modifications
    -- are reflected in the name only.  If you put invalid identifiers in the
    -- name, it can't be used to set default arguments.  That's not really
    -- a big deal for the note call, though.
    -> Derive.Doc -> Tags.Tags -> GenerateNote -> Derive.Generator Derive.Note
note_call name prepend_doc tags generate =
    Derive.generator Module.prelude name tags prepended $
    Sig.call (Sig.many "attribute" "Change the instrument or attributes.") $
    \vals -> Sub.inverting (transform_note vals . generate)
    where
    prepended
        | prepend_doc == "" = generator_doc
        | otherwise = "Modified note call: " <> prepend_doc <> "\n"
            <> generator_doc
    generator_doc =
        "The note call is the main note generator, and will emit a single"
        <> " score event. It interprets `>inst` and `+attr` args by"
        <> " setting those fields of the event.  This is bound to the"
        <> " null call, \"\", but any potential arguments would wind up"
        <> " looking like a different call, so it's bound to `n` as well."

c_note_track :: Derive.Transformer Derive.Note
c_note_track = Derive.transformer Module.prelude "note-track" mempty
    ("This is the implicit call at the top of every note track. It expects a"
    <> " instrument as its first argument, since note tracks all start with"
    <> " `>`. If there is a note transformer of the same name as the"
    <> " instrument, starting with `>`, it will be called after setting the"
    <> " instrument. This way, you can set instrument-specific variables or"
    <> " transformations.")
    $ Sig.callt ((,)
    <$> Sig.required_env "inst" Derive.None
        ("Set this instrument, and run a transformer with the same name, if it"
        <> " exists.")
    <*> Sig.many "attribute" "Add attributes."
    ) $ \(inst, attrs) args deriver ->
        note_track (Derive.passed_ctx args) inst attrs deriver

note_track :: Derive.Context Derive.Note -> Score.Instrument
    -> [Attrs.Attributes] -> Derive.NoteDeriver -> Derive.NoteDeriver
note_track ctx inst attrs deriver = do
    let call_id = BaseTypes.Symbol $ ">" <> Score.instrument_name inst
    maybe_call <- Derive.lookup_transformer call_id
    let transform = maybe id (call_transformer ctx) maybe_call
        with_inst = if inst == Score.empty_instrument then id
            else Derive.with_instrument inst
    with_inst $ Call.add_attributes (mconcat attrs) $ transform deriver

call_transformer :: Derive.Context d -> Derive.Transformer d
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
call_transformer ctx call deriver =
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed deriver
    where
    passed = Derive.PassedArgs
        { Derive.passed_vals = []
        , Derive.passed_call_name = Derive.call_name call
        , Derive.passed_ctx = ctx
        }

c_note_attributes :: Derive.Transformer Derive.Note
c_note_attributes = Derive.transformer Module.prelude "note-attributes"
    mempty
    ("This is similar to to `=`, but it takes any number of `>inst` and"
        <> " `+attr` args and sets the `inst` or `attr` environ.")
    $ Sig.callt (Sig.many "attribute" "Set instrument or attributes.")
    $ \inst_attrs _args -> transform_note inst_attrs

-- ** generate

-- | Generate a single note.  This is intended to be used as the lowest level
-- null call for some instrument.
type GenerateNote = Derive.NoteArgs -> Derive.NoteDeriver

data Config = Config {
    -- | Note duration is affected by 'Attrs.staccato'.
    config_staccato :: !Bool
    -- | Note duration can depend on 'Controls.sustain' and
    -- 'Controls.sustain_abs'.
    , config_sustain :: !Bool
    } deriving (Show)

use_attributes :: Config
use_attributes = Config
    { config_staccato = True
    , config_sustain = True
    }

-- | Don't observe any of the duration affecting attributes.
no_duration_attributes :: Config
no_duration_attributes = Config False False

-- | The actual note generator.
default_note :: Config -> GenerateNote
default_note config args = do
    start <- Args.real_start args
    end <- Args.real_end args
    dyn <- Internal.get_dynamic id
    -- Add flags to get the arrival-note postproc to figure out the duration.
    -- Details in "Derive.Call.Post.ArrivalNote".
    let flags = note_flags (start == end) (Derive.state_stack dyn)
            (Derive.state_environ dyn)
    control_vals <- Derive.controls_at start
    let attrs = either (const mempty) id $
            Env.get_val EnvKey.attributes (Derive.state_environ dyn)
    let adjusted_end = duration_attributes config control_vals attrs start end
    Stream.from_event <$> make_event_control_vals
        control_vals args dyn start (adjusted_end - start) flags

note_flags :: Bool -> Stack.Stack -> Env.Environ -> Flags.Flags
note_flags zero_dur stack environ
    -- An event at TrackTime 0 never gets an inferred duration.
    -- Otherwise, I couldn't write single note calls for percussion.
    | infer_dur && track_start = mempty
    | infer_dur = Flags.infer_duration <> Flags.strong
    | otherwise = mempty
    where
    -- Note that I can't use Args.duration or Args.range_on_track, because
    -- this may be invoked via e.g. Call.note, which fakes up an event with
    -- range (0, 1), and sets the duration via the warp.
    infer_dur = track_end && zero_dur
    track_start = start == Just 0
    track_end = start == Env.maybe_val EnvKey.block_end environ
    start = fst <$> Seq.head (mapMaybe Stack.region_of (Stack.innermost stack))

-- | This is the canonical way to make a Score.Event.  It handles all the
-- control trimming and control function value stashing that the perform layer
-- relies on.
make_event :: Derive.PassedArgs a -> Derive.Dynamic -> RealTime -> RealTime
    -> Flags.Flags -> Derive.Deriver Score.Event
make_event args dyn start dur flags = do
    control_vals <- Derive.controls_at start
    make_event_control_vals control_vals args dyn start dur flags

-- | Specialized version of 'make_event' just so I can avoid calling
-- Derive.controls_at twice.
make_event_control_vals :: Score.ControlValMap -> Derive.PassedArgs a
    -> Derive.Dynamic -> RealTime -> RealTime -> Flags.Flags
    -> Derive.Deriver Score.Event
make_event_control_vals control_vals args dyn start dur flags = do
    offset <- get_start_offset start
    Internal.increment_event_serial
    return $! Score.Event
        { event_start = start
        , event_duration = dur
        , event_text = Event.text (Args.event args)
        , event_untransformed_controls = controls
        , event_untransformed_pitch = trim_pitch start (Derive.state_pitch dyn)
        , event_control_offset = 0
        -- I don't have to trim these because the performer doesn't use them,
        -- they're only there for any possible postproc.
        , event_untransformed_pitches = Derive.state_pitches dyn
        , event_stack = Derive.state_stack dyn
        , event_highlight = Color.NoHighlight
        , event_instrument = fromMaybe Score.empty_instrument $
            Env.maybe_val EnvKey.instrument environ
        , event_environ = stash_convert_values control_vals offset environ
        , event_flags = flags
        , event_delayed_args = mempty
        , event_logs = []
        }
    where
    controls = trim_controls start (Derive.state_controls dyn)
    environ = Derive.state_environ dyn

-- | Stash the dynamic value from the ControlValMap in
-- 'Controls.dynamic_function'.  Gory details in NOTE [EnvKey.dynamic_val].
stash_convert_values :: Score.ControlValMap -> RealTime -> Env.Environ
    -> Env.Environ
stash_convert_values vals offset =
    stash_start_offset
    . insert_if Controls.dynamic EnvKey.dynamic_val
    . insert_if Controls.attack_velocity EnvKey.attack_val
    -- Perhaps this should be sampled at the event end, but I don't want to
    -- get a whole new ControlValMap just for that.
    . insert_if Controls.release_velocity EnvKey.release_val
    where
    stash_start_offset = Env.insert_val EnvKey.start_offset_val offset
    insert_if control key = maybe id (Env.insert_val key) $
        Map.lookup control vals

-- ** adjust start and duration

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

{- | Interpret attributes and controls that effect the note's duration.

    This is actually somewhat complicated.  Instead of all the
    duration-affecting controls all applying together, notes fit into distinct
    categories:

    - Zero-duration notes ignore all this.

    - Staccato notes divide their duration by 2.

    - Normal notes multiply 'Controls.sustain' and add 'Controls.duration_abs',
    which could be negative.  They clip at a minimum duration to keep from
    going negative.
-}
duration_attributes :: Config -> Score.ControlValMap -> Attrs.Attributes
    -> RealTime -> RealTime -> RealTime -- ^ new end
duration_attributes config controls attrs start end
    | start >= end = end -- don't mess with 0 dur or negative notes
    | otherwise = start + max min_duration (dur * sustain + sustain_abs)
    where
    has = Attrs.contain attrs
    dur = end - start
    staccato = config_staccato config && has Attrs.staccato
    sustain = if staccato then sustain_ * 0.5 else sustain_
    sustain_abs = if staccato || not (config_sustain config)
        then 0 else lookup_time 0 Controls.sustain_abs
    sustain_ = if config_sustain config
        then lookup_time 1 Controls.sustain else 1
    lookup_time deflt control = maybe deflt RealTime.seconds
        (Map.lookup control controls)

get_start_offset :: RealTime -> Derive.Deriver RealTime
get_start_offset start = do
    start_s <- maybe 0 RealTime.seconds <$>
        Derive.untyped_control_at Controls.start_s start
    start_t <- maybe 0 ScoreTime.double <$>
        Derive.untyped_control_at Controls.start_t start
    start_t <- Call.real_duration start start_t
    return $ start_s + start_t

-- ** controls

-- | Trim control signals.
--
-- Previously I would also trim to the end of the note, but now I leave it
-- as-is and rely on the performer to trim the end according to the
-- instrument's decay time.  This is so that a note whose decay persists
-- outside of its block can still see control changes after its block ends.
trim_controls :: RealTime -> Score.ControlMap -> Score.ControlMap
trim_controls start = Map.map (fmap (Signal.drop_before start))

-- | For inverted tracks, this trimming should already be done by
-- 'Derive.Control.trim_signal'.
trim_pitch :: RealTime -> PSignal.PSignal -> PSignal.PSignal
trim_pitch = PSignal.drop_before

-- ** transform

transform_note :: [Either Score.Instrument Attrs.Attributes]
    -> Derive.NoteDeriver -> Derive.NoteDeriver
transform_note vals deriver =
    with_inst (Call.add_attributes (mconcat attrs) deriver)
    where
    (insts, attrs) = Either.partitionEithers vals
    with_inst = maybe id Derive.with_instrument $
        Seq.last $ filter (/=Score.empty_instrument) insts
