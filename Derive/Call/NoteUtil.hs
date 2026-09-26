-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the default note call.  They're separated here so internal
-- utilities can make events without importing "Derive.C.Prelude.Note".
module Derive.Call.NoteUtil (makeEvent, makeEventControlVals) where
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
makeEvent :: Derive.PassedArgs a -> Derive.Dynamic -> RealTime -> RealTime
    -> Text -> Flags.Flags -> Derive.Deriver Score.Event
makeEvent args dyn start dur integrate flags = do
    cvmap <- Derive.controlsAt start
    makeEventControlVals cvmap args dyn start dur integrate flags

-- | Specialized version of 'make_event' just so I can avoid calling
-- Derive.controls_at twice.
makeEventControlVals :: ScoreT.ControlValMap -> Derive.PassedArgs a
    -> Derive.Dynamic -> RealTime -> RealTime -> Text -> Flags.Flags
    -> Derive.Deriver Score.Event
makeEventControlVals cvmap args dyn start dur integrate flags = do
    offset <- getStartOffset start
    Internal.incrementEventSerial
    pure $! Score.Event
        { eventStart = start
        , eventDuration = dur
        , eventText = Event.text (Args.event args)
        , eventIntegrate = integrate
        , eventPitch = trimPitch start (Derive.statePitch dyn)
        , eventStack = Derive.stateStack dyn
        , eventHighlight = Color.NoHighlight
        , eventInstrument = fromMaybe ScoreT.emptyInstrument $
            Env.maybeVal EnvKey.instrument environ
        -- Put trimmed controls back into the environ.
        , eventEnviron = Env.map (trimControlVals start) $
            stashConvertValues cvmap offset environ
        , eventFlags = flags
        , eventDelayedArgs = mempty
        , eventLogs = []
        }
    where environ = Derive.stateEnviron dyn

-- | Trim control signals.
--
-- Previously I would also trim to the end of the note, but now I leave it
-- as-is and rely on the performer to trim the end according to the
-- instrument's decay time.  This is so that a note whose decay persists
-- outside of its block can still see control changes after its block ends.
trimControlVals :: RealTime -> DeriveT.Val -> DeriveT.Val
trimControlVals start = \case
    DeriveT.VSignal sig -> DeriveT.VSignal $ Signal.dropBefore start <$> sig
    DeriveT.VPSignal sig -> DeriveT.VPSignal $ PSignal.dropBefore start sig
    val -> val

-- | Stash the dynamic value from the ControlValMap in
-- 'Controls.dynamic_function'.  Gory details in NOTE [EnvKey.dynamic_val].
stashConvertValues :: ScoreT.ControlValMap -> RealTime -> Env.Environ
    -> Env.Environ
stashConvertValues vals offset =
    stashStartOffset
    . insertIf Controls.dynamic EnvKey.dynamicVal
    . insertIf Controls.attackVelocity EnvKey.attackVal
    -- Perhaps this should be sampled at the event end, but I don't want to
    -- get a whole new ControlValMap just for that.
    . insertIf Controls.releaseVelocity EnvKey.releaseVal
    where
    stashStartOffset = Env.insertVal EnvKey.startOffsetVal offset
    insertIf control key = maybe id (Env.insertVal key) $
        Map.lookup control vals

getStartOffset :: RealTime -> Derive.Deriver RealTime
getStartOffset start = do
    startS <- maybe 0 RealTime.seconds <$>
        Derive.untypedControlAt Controls.startS start
    startT <- maybe 0 ScoreTime.fromDouble <$>
        Derive.untypedControlAt Controls.startT start
    startT <- Call.realDuration start startT
    pure $ startS + startT

-- | For inverted tracks, this trimming should already be done by
-- 'Derive.Control.trim_signal'.
trimPitch :: RealTime -> PSignal.PSignal -> PSignal.PSignal
trimPitch = PSignal.dropBefore
