-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- This module is intentionally full of imports that will be used by the REPL.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Top-level module for the REPL interpreter.  Everything in this module is
    visible to the REPL, so it imports a lot of potentially useful modules.

    It has to be interpreted, so it should just put useful things into scope
    but not actually define anything itself.  Those definitions go in
    "Cmd.Repl.Global".
-}
module Cmd.Repl.Environ where
import           Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Factor as Factor
import qualified Cmd.Info as Info
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Perf as Perf
import qualified Cmd.Play as Play
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.LAlloc as LAlloc
import qualified Cmd.Repl.LBlock as LBlock
import qualified Cmd.Repl.LCmd as LCmd
import qualified Cmd.Repl.LControl as LControl
import qualified Cmd.Repl.LDebug as LDebug
import qualified Cmd.Repl.LEvent as LEvent
import qualified Cmd.Repl.LInst as LInst
import qualified Cmd.Repl.LIntegrate as LIntegrate
import qualified Cmd.Repl.LKeycaps as LKeycaps
import qualified Cmd.Repl.LLily as LLily
import qualified Cmd.Repl.LNote as LNote
import qualified Cmd.Repl.LPerf as LPerf
import qualified Cmd.Repl.LPitch as LPitch
import qualified Cmd.Repl.LRuler as LRuler
import qualified Cmd.Repl.LSol as LSol
import qualified Cmd.Repl.LState as LState
import qualified Cmd.Repl.LSymbol as LSymbol
import qualified Cmd.Repl.LTScore as LTScore
import qualified Cmd.Repl.LTala as LTala
import qualified Cmd.Repl.LTrack as LTrack
import qualified Cmd.Repl.LTuning as LTuning
import qualified Cmd.Repl.LView as LView
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Meters as Meters
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Ruler.Tala as Tala
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.ViewConfig as ViewConfig
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Midi.Synth as Synth
import qualified Perform.Lilypond as Lilypond
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Note as Note
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Symbol as Symbol
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Update as Update
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import           Cmd.Repl.Global
import           Types
import           Global
