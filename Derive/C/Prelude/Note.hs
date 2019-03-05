-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic calls for note tracks.
module Derive.C.Prelude.Note (
    library
    , c_note, transformed_note, note_call
    , Config(..), use_attributes, no_duration_attributes
    , GenerateNote, default_note, note_flags
    , adjust_duration
    , min_duration
) where
import qualified Data.Either as Either
import qualified Data.Map as Map

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.NoteUtil as NoteUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.Symbols as Symbols

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [ (Symbols.null_note, c_note)
        , (Symbols.default_note, c_note)
        ]
    , Library.transformers [(Symbols.note_track, c_note_track)]
    , Library.both [("attr", c_with_attributes)]
    ]

-- | This is mostly useful for tests when I need some call that makes a note
-- and takes arguments.
c_with_attributes :: Library.Calls Derive.Note
c_with_attributes = Make.transform_notes Module.prelude "note" mempty
    "A note with attributes or instrument."
    (Sig.many "attr" "Set instrument or add attributes.") $
    \inst_attrs deriver -> do
        let (insts, attrs) = Either.partitionEithers inst_attrs
            inst = Seq.last insts
        maybe id Derive.with_instrument inst $
            Call.add_attributes (mconcat attrs) deriver

-- * note

c_note :: Derive.Generator Derive.Note
c_note = note_call "note" "" mempty (default_note use_attributes)

-- | Create a standard note call with a transformer applied.
transformed_note :: Doc.Doc -> Tags.Tags
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
transformed_note prepend_doc tags transform =
    note_call "note" prepend_doc tags $ \args ->
        transform args (default_note use_attributes args)

-- | Create a note call, configuring it with the actual note generating
-- function.  The generator is called with the usual note arguments, and
-- receives the usual instrument and attribute transform.
note_call :: Derive.CallName -> Doc.Doc -> Tags.Tags -> GenerateNote
    -> Derive.Generator Derive.Note
note_call name prepend_doc tags generate =
    Derive.generator Module.prelude name tags prepended $
    Sig.call0  $ Sub.inverting $ apply_instrument_controls . generate
    where
    prepended
        | prepend_doc == "" = generator_doc
        | otherwise = "Modified note call: " <> prepend_doc <> "\n"
            <> generator_doc
    generator_doc =
        "The note call is the default note generator, and will emit a single\
        \ score event. Usually this is bound to the null call, \"\", which is\
        \ therefore the most syntactically convenient note generator.\
        \\nThis should probably remain bound to "
        <> ShowVal.doc Symbols.note_track <> " symbol, which is used\
        \ internally by many other calls when they want a plain note. This is\
        \ so you can bind \"\" to one of those other calls without getting\
        \ recursion."
    -- Some history: long ago, you could call the note with >inst to set the
    -- instrument, and +a to set attributes.  The intention was an easy
    -- notation to write a single part that alternated between multiple
    -- instruments, in the way that trackers would often do it.  But eventually
    -- I generalized tracks to be able to contain arbitrary calls, and the note
    -- became the "" call, which couldn't take arguments.  Later I got +a
    -- syntax back courtesty of lookup calls, but the >inst literal syntax is
    -- gone now, and in any case I tend to have one instrument per-track due to
    -- wanting to scope transformations per-instrument.  But maybe some day
    -- I'll bring back >inst syntax as a lookup call.


-- | Apply the 'Instrument.Common.config_controls' field.  It happens in the
-- note call to make sure it happens only once per note.
apply_instrument_controls :: Derive.Deriver a -> Derive.Deriver a
apply_instrument_controls deriver = Call.lookup_instrument >>= \case
    Nothing -> deriver
    Just inst -> do
        (_inst, derive_inst) <- Derive.get_instrument inst
        let controls = ScoreT.untyped . Signal.constant <$>
                Derive.inst_controls derive_inst
        Derive.with_merged_controls (Map.toList controls) deriver

c_note_track :: Derive.Transformer Derive.Note
c_note_track = Derive.transformer Module.prelude "note-track" mempty
    ("This is the implicit call at the top of every note track. The first\
    \ argument is the instrument named after the note track's `>`.\
    \ If there is a note transformer of the same name as the\
    \ instrument, starting with `>`, it will be called after setting the\
    \ instrument. This way, you can set instrument-specific variables or\
    \ transformations.")
    $ Sig.callt (Sig.defaulted_env "inst" Derive.None Nothing
        "Set this instrument and run the transformer, if it exists."
    ) $ \inst args deriver -> note_track (Derive.passed_ctx args) inst deriver

note_track :: Derive.Context Derive.Note -> Maybe ScoreT.Instrument
    -> Derive.NoteDeriver -> Derive.NoteDeriver
note_track ctx inst deriver = do
    let sym = Expr.Symbol $ ">" <> maybe "" ScoreT.instrument_name inst
    maybe_call <- Derive.lookup_call sym
    let transform = maybe id (call_transformer ctx) maybe_call
    maybe id Derive.with_instrument inst $ transform deriver

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
    Stream.from_event <$> NoteUtil.make_event_control_vals
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
    infer_dur = block_end && zero_dur
    track_start = start == Just 0
    block_end = start == Env.maybe_val EnvKey.block_end environ
    -- This is the start time on the track, which, due to slicing, is not
    -- necessarily the same as Args.start.
    start = fst <$> Seq.head (mapMaybe Stack.region_of (Stack.innermost stack))

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
duration_attributes :: Config -> ScoreT.ControlValMap -> Attrs.Attributes
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
