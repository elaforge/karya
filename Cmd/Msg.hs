-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Msg where
import qualified Control.DeepSeq as DeepSeq
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified System.IO as IO

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi
import qualified Ui.Key as Key
import qualified Ui.Track as Track
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.InputNote as InputNote
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Transport as Transport
import Types


data Msg =
    -- | Message from the UI.
    Ui UiMsg.UiMsg
    -- | Incoming midi message.
    | Midi Midi.ReadMessage
    -- | Incoming abstract note.  This is meant to enter a note and is not
    -- a MIDI NoteOn, though NoteOns may very well be translated into these.
    -- These are not generated from any subsystem, but instead translated from
    -- other msgs (such as Midi).
    | InputNote InputNote.Input
    -- | Message from the transport/play thread.
    | Transport Transport.Status
    -- | Message about the derivation status, from the background derivation
    -- threads.
    | DeriveStatus BlockId DeriveStatus
    -- | Message from the REPL socket, includes the socket handle than can be
    -- used to write a response.  Whoever responds to it should close the
    -- handle.
    | Socket IO.Handle String
    deriving (Show)

instance Pretty.Pretty Msg where
    pretty (Ui msg) = "Ui: " ++ pretty msg
    pretty (Midi msg) = "Midi: " ++ pretty msg
    pretty (InputNote msg) = "Input: " ++ show msg
    pretty (DeriveStatus bid status) = "DeriveStatus: " ++ show bid ++ ": "
        ++ pretty status
    pretty msg = show msg

data DeriveStatus =
    -- | The current derivation is out of date, but work has not yet started
    -- on a replacement.  The new Performance is already in
    -- 'Cmd.state_current_performance' but not in 'Cmd.state_performance' yet.
    OutOfDate
    | Deriving
    | DeriveComplete Performance
    deriving (Show)

instance Pretty.Pretty DeriveStatus where pretty = show

-- Performance should be in "Cmd.Cmd", but that would be a circular import.

-- | This holds the final performance for a given block.  It is used to
-- actually play music, and poked and prodded in a separate thread to control
-- its evaluation.
--
-- This is basically the same as 'Derive.Result'.  I could make them be the
-- same, but Performance wasn't always the same and may not be the same in the
-- future.
data Performance = Performance {
    perf_derive_cache :: Derive.Cache
    , perf_events :: Events
    -- | Logs from the derivation are written separately.
    , perf_logs :: [Log.Msg]
    -- | The logs are only written on the first play, to minimize error spam.
    -- So there's a flag which says whether these logs have been written or
    -- not.  I don't clear the logs, so 'Cmd.Repl.LPerf.cache_stats' can
    -- inspect them.
    , perf_logs_written :: Bool
    , perf_track_dynamic :: Derive.TrackDynamic
    , perf_integrated :: [Derive.Integrated]
    -- | ScoreDamage is normally calculated automatically from the UI diff,
    -- but Cmds can also intentionally inflict damage to cause a rederive.
    , perf_damage :: Derive.ScoreDamage
    , perf_warps :: [TrackWarp.Collection]
    , perf_track_signals :: Track.TrackSignals
    }
    -- The fields are intentionally not strict, since I need to modify
    -- 'perf_damage' without forcing any of the others.

-- | Force a Performance so that it can be used without a lag.
force_performance :: Performance -> ()
force_performance perf = perf_logs perf `DeepSeq.deepseq` perf_events perf
    `DeepSeq.deepseq` perf_warps perf `DeepSeq.deepseq` perf_track_dynamic
    `DeepSeq.deepseq` ()

-- | This is the forced result of a derivation.
type Events = Vector.Vector Score.Event

instance Show Performance where
    show perf = "((Performance " ++ pretty len ++ "))"
        where len = Derive.cache_size (perf_derive_cache perf)

instance Pretty.Pretty Performance where
    format perf = Pretty.record "Performance"
        [ ("cache", Pretty.format (Map.keys c))
        , ("events", Pretty.format (Vector.length (perf_events perf)))
        , ("integrated", Pretty.format (perf_integrated perf))
        , ("damage", Pretty.format (perf_damage perf))
        , ("warps", Pretty.format (perf_warps perf))
        ]
        where Derive.Cache c = perf_derive_cache perf

-- * views

-- To reduce dependency on the exact structure of 'Msg', and to avoid long hard
-- to read cases, use the view functions here.  They can be conveniently used
-- with the ViewPatterns feature.

mouse :: Msg -> Maybe UiMsg.MsgEvent
mouse (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent mouse@(UiMsg.Mouse {})))) =
    Just mouse
mouse _ = Nothing

mouse_down :: Msg -> Bool
mouse_down msg = case mouse msg of
    Just (UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseDown _ }) -> True
    _ -> False

key :: Msg -> Maybe (UiMsg.KbdState, Key.Key)
key (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd state _ key _)))) =
    Just (state, key)
key _ = Nothing

key_down :: Msg -> Maybe Key.Key
key_down msg = case key msg of
    Just (UiMsg.KeyDown, k) -> Just k
    _ -> Nothing

-- | The text that this keydown wants to enter, if any.
text :: Msg -> Maybe (Key.Key, Maybe Char)
text (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd UiMsg.KeyDown _ key text))))
    = Just (key, text)
text _ = Nothing

key_mods :: Msg -> Maybe [Key.Modifier]
key_mods (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd _ mods _ _)))) =
    Just mods
key_mods _ = Nothing

-- | Printable keycap down.  This is different from 'text' because it should be
-- just the keycap, not taking shift or alt or anything into account.
char :: Msg -> Maybe (UiMsg.KbdState, Char)
char msg = case key msg of
    Just (state, Key.Char c) -> Just (state, c)
    _ -> Nothing

char_down :: Msg -> Maybe Char
char_down msg = case char msg of
    Just (UiMsg.KeyDown, c) -> Just c
    _ -> Nothing

midi :: Msg -> Maybe Midi.Message
midi (Midi (Midi.ReadMessage { Midi.rmsg_msg = msg })) = Just msg
midi _ = Nothing

context :: Msg -> Maybe UiMsg.Context
context (Ui (UiMsg.UiMsg context _)) = Just context
context _ = Nothing

context_track :: Msg -> Maybe (TrackNum, UiMsg.Track)
context_track = UiMsg.ctx_track <=< context

context_track_pos :: Msg -> Maybe (TrackNum, ScoreTime)
context_track_pos msg = context_track msg >>= \(tracknum, t) -> case t of
    UiMsg.Track pos -> Just (tracknum, pos)
    _ -> Nothing
