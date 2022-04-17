-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the default note call.  They're separated here so internal
-- utilities can make events without importing "Derive.C.Prelude.Note".
module Derive.Call.NoteUtil (make_event, make_event_control_vals) where
import qualified Data.Map as Map

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- | This is the canonical way to make a Score.Event.  It handles all the
-- control trimming and control function value stashing that the perform layer
-- relies on.
make_event :: Derive.PassedArgs a -> Derive.Dynamic -> RealTime -> RealTime
    -> Text -> Flags.Flags -> Derive.Deriver Score.Event
make_event args dyn start dur integrate flags = do
    control_vals <- Derive.controls_at start
    make_event_control_vals control_vals args dyn start dur integrate flags

-- | Specialized version of 'make_event' just so I can avoid calling
-- Derive.controls_at twice.
make_event_control_vals :: ScoreT.ControlValMap -> Derive.PassedArgs a
    -> Derive.Dynamic -> RealTime -> RealTime -> Text -> Flags.Flags
    -> Derive.Deriver Score.Event
make_event_control_vals control_vals args dyn start dur integrate flags = do
    offset <- get_start_offset start
    Internal.increment_event_serial
    return $! Score.Event
        { event_start = start
        , event_duration = dur
        , event_text = Event.text (Args.event args)
        , event_integrate = integrate
        , event_controls = controls
        , event_pitch = trim_pitch start (Derive.state_pitch dyn)
        -- I don't have to trim these because the performer doesn't use them,
        -- they're only there for any possible postproc.
        , event_pitches = Derive.state_pitches dyn
        , event_stack = Derive.state_stack dyn
        , event_highlight = Color.NoHighlight
        , event_instrument = fromMaybe ScoreT.empty_instrument $
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
stash_convert_values :: ScoreT.ControlValMap -> RealTime -> Env.Environ
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

get_start_offset :: RealTime -> Derive.Deriver RealTime
get_start_offset start = do
    start_s <- maybe 0 RealTime.seconds <$>
        Derive.untyped_control_at Controls.start_s start
    start_t <- maybe 0 ScoreTime.from_double <$>
        Derive.untyped_control_at Controls.start_t start
    start_t <- Call.real_duration start start_t
    return $ start_s + start_t

-- | Trim control signals.
--
-- Previously I would also trim to the end of the note, but now I leave it
-- as-is and rely on the performer to trim the end according to the
-- instrument's decay time.  This is so that a note whose decay persists
-- outside of its block can still see control changes after its block ends.
trim_controls :: RealTime -> DeriveT.ControlMap -> DeriveT.ControlMap
trim_controls start = Map.map (fmap (Signal.drop_before start))

-- | For inverted tracks, this trimming should already be done by
-- 'Derive.Control.trim_signal'.
trim_pitch :: RealTime -> PSignal.PSignal -> PSignal.PSignal
trim_pitch = PSignal.drop_before
