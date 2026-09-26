-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
module Cmd.Msg where
import           Control.DeepSeq (deepseq)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified System.IO as IO

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified App.ReplProtocol as ReplProtocol
import qualified Cmd.InputNote as InputNote
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.TrackWarp as TrackWarp

import qualified Local.KeyLayout
import qualified Midi.Midi as Midi
import qualified Perform.Transport as Transport
import qualified Synth.ImGc as ImGc
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg

import           Control.Monad
import           Global
import           Types


data Msg
    -- | Message from the UI.
    = Ui UiMsg.UiMsg
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
    | Socket IO.Handle ReplProtocol.Query
    deriving (Show)

showShort :: Msg -> Text
showShort = \case
    Ui ui -> UiMsg.showShort ui
    Midi midi -> pretty midi
    InputNote note -> pretty note
    Transport status -> pretty status
    -- show_short is used for timing, and Show DeriveStatus can force stuff in
    -- Performance, so let's avoid the Show instance.
    DeriveStatus bid status -> Id.identText bid <> ":" <> case status of
        OutOfDate -> "OutOfDate"
        Deriving -> "Deriving"
        DeriveComplete {} -> "DeriveComplete"
        status@(ImStatus {}) -> pretty status
    Socket _hdl query -> pretty query

instance Pretty Msg where
    pretty = \case
        Ui msg -> "Ui: " <> pretty msg
        Midi msg -> "Midi: " <> pretty msg
        InputNote msg -> "Input: " <> showt msg
        Transport status -> pretty status
        DeriveStatus bid status -> "DeriveStatus: " <> showt bid <> ": "
            <> pretty status
        Socket _hdl query -> "Socket: " <> pretty query

data DeriveStatus
    -- | The current derivation is out of date, but work has not yet started
    -- on a replacement.  The new Performance is already in
    -- 'Cmd.state_current_performance' but not in 'Cmd.state_performance' yet.
    = OutOfDate
    | Deriving
    | DeriveComplete Performance ImStarted
    -- | The BlockId is the block to which this status applies, the BlockId
    -- in the containing DeriveStatus is the root block for the derivation.
    -- It's redundant for 'ImComplete', because only the root block gets one of
    -- those.
    | ImStatus BlockId (Set TrackId) ImStatus
    deriving (Show)

instance Pretty DeriveStatus where
    pretty = \case
        ImStatus blockId trackIds status ->
            pretty blockId <> ":" <> pretty trackIds <> ":" <> pretty status
        status -> showt status

data ImStarted = ImStarted -- ^ im subprocess in progress
    | ImUnnecessary -- ^ no im notes, so no subprocesses started
    deriving (Show)

data ImStatus
    -- | start--end currently being rendered.
    = ImRenderingRange ScoreT.Instrument RealTime RealTime
    -- | Waveforms written for these chunks.
    | ImWaveformsCompleted [Track.WaveformChunk]
    -- | True if the im subprocess had a failure.  The error will have been
    -- logged, and this flag will leave a visual indicator on the track that
    -- something went wrong.
    | ImComplete Bool (Maybe ImGc.Stats)
    deriving (Show)

instance Pretty ImStatus where
    pretty = \case
        ImRenderingRange inst start end ->
            pretty inst <> "(" <> pretty start <> "--" <> pretty end <> ")"
        ImWaveformsCompleted waves -> "ImWaveformsCompleted:"
            <> Text.intercalate "," (map (txt . Track._filename) waves)
        ImComplete failed _ -> "ImComplete" <> if failed then "(failed)" else ""

-- Performance should be in "Cmd.Cmd", but that would be a circular import.

{- | This holds the final performance for a given block.  It is used to
    actually play music, and poked and prodded in a separate thread to control
    its evaluation.

    This is basically the same as 'Derive.Result'.  I could make them be the
    same, but Performance wasn't always the same and may not be the same in the
    future.

    Unlike other records, the fields here are all lazy.  This is because I need
    to put an unevaluated Performance into Cmd.state_current_performances, and
    then force the fields in a separate thread.  Also I need to modify
    'perf_damage' without forcing any of the others.
-}
data Performance = Performance
    { perfDeriveCache :: ~Derive.Cache
    -- | This is the forced result of a derivation.
    , perfEvents :: ~(Vector.Vector Score.Event)
    -- | Logs from the derivation are written separately.
    , perfLogs :: ~[Log.Msg]
    -- | The logs are only written on the first play, to minimize error spam.
    -- So there's a flag which says whether these logs have been written or
    -- not.  I don't clear the logs, so 'Cmd.Repl.LPerf.cache_stats' can
    -- inspect them.
    , perfLogsWritten :: ~Bool
    , perfTrackDynamic :: ~Derive.TrackDynamic
    , perfIntegrated :: ~[Derive.Integrated]
    -- | ScoreDamage is normally calculated automatically from the UI diff,
    -- but Cmds can also intentionally inflict damage to cause a rederive.
    , perfDamage :: ~Derive.ScoreDamage
    , perfWarps :: ~[TrackWarp.TrackWarp]
    , perfTrackSignals :: ~Track.TrackSignals
    , perfBlockDeps :: ~Derive.BlockDeps
    -- | Map each track to the instruments on it.  This tries to remain lazy,
    -- since it's only used by muted_im_instruments.
    , perfTrackInstruments :: ~(Map TrackId (Set ScoreT.Instrument))
    -- | This is the score state at the time of the performance.  It's needed
    -- to interpret 'perf_track_signals', because at the time signals are sent
    -- (in 'Cmd.PlayC.cmd_play_msg'), the Ui.State may have unsynced changes.
    , perfUiState :: ~Ui.State
    }

-- | Force a Performance so that it can be used without a lag.
forcePerformance :: Performance -> ()
forcePerformance (Performance _cache events logs _logsWritten trackDyn
        _integrated _damage warps trackSigs blockDeps _trackInsts uiState) =
    logs `deepseq` events `deepseq` warps `deepseq` trackDyn
        `deepseq` trackSigs `deepseq` blockDeps `deepseq` uiState
        `deepseq` ()

instance Show Performance where
    show perf = "((Performance " <> show (Vector.length (perfEvents perf))
        <> " events " <> show (length (perfLogs perf)) <> " logs))"
        -- Show events and logs.  This way there's a hint to look at logs if
        -- there are mysteriously no events.

instance Pretty Performance where
    format (Performance cache events logs logsWritten trackDynamic
            integrated damage warps trackSignals blockDeps trackInsts
            _uiState) =
        Pretty.record "Performance"
        [ ("cache", Pretty.format $ Map.keys $ (\(Derive.Cache c) -> c) cache)
        , ("events", Pretty.format (Vector.length events))
        , ("logs", Pretty.format logs)
        , ("logs_written", Pretty.format logsWritten)
        , ("track_dynamic", Pretty.format trackDynamic)
        , ("integrated", Pretty.format integrated)
        , ("damage", Pretty.format damage)
        , ("warps", Pretty.format warps)
        , ("track_signals", Pretty.format trackSignals)
        , ("block_deps", Pretty.format blockDeps)
        , ("track_instruments", Pretty.format trackInsts)
        ]

-- * views

-- To reduce dependency on the exact structure of 'Msg', and to avoid long hard
-- to read cases, use the view functions here.  They can be conveniently used
-- with the ViewPatterns feature.

mouse :: Msg -> Maybe UiMsg.MouseEvent
mouse (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Mouse mouse)))) =
    Just mouse
mouse _ = Nothing

mouseDown :: Msg -> Bool
mouseDown msg = case mouse msg of
    Just (UiMsg.MouseEvent { UiMsg.mouseState = UiMsg.MouseDown _ }) -> True
    _ -> False

kbd :: Msg -> Maybe (UiMsg.KbdState, [Key.Modifier], Key.Key, Maybe Char)
kbd (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd state mods key text)))) =
    Just (state, mods, key, text)
kbd _ = Nothing

key :: Msg -> Maybe (UiMsg.KbdState, Key.Key)
key (kbd -> Just (state, _, key, _)) = Just (state, key)
key _ = Nothing

keyDown :: Msg -> Maybe Key.Key
keyDown (key -> Just (UiMsg.KeyDown, k)) = Just k
keyDown _ = Nothing

-- | The text that this keydown wants to enter, if any.
text :: Msg -> Maybe (Key.Key, Maybe Char)
text (kbd -> Just (UiMsg.KeyDown, _, key, text)) = Just (key, text)
text _ = Nothing

keyMods :: Msg -> Maybe [Key.Modifier]
keyMods (kbd -> Just (_, mods, _, _)) = Just mods
keyMods _ = Nothing

-- | A key action by keycap.  This is different from 'text' because it should
-- be just the keycap, not taking shift or alt or anything into account.
keycap :: Msg -> Maybe (UiMsg.KbdState, Char)
keycap msg = case key msg of
    Just (state, Key.Char c) -> Just (state, c)
    _ -> Nothing

-- | This is like 'keycap', but it takes shift into account.  This is because
-- it's convenient to bind to a single Char including shifted, and not have to
-- pass around a ([Key.Modifier], Char) or (Bool, Char).
char :: Msg -> Maybe (UiMsg.KbdState, Char)
char (kbd -> Just (state, mods, Key.Char c, _))
    | Key.Shift `elem` mods =
        (state,) <$> KeyLayouts.toShifted Local.KeyLayout.layout c
        -- I have to use the keylayout instead of 'text' because fltk doesn't
        -- give me text for KeyUps.
    | otherwise= Just (state, c)
char _ = Nothing

charDown :: Msg -> Maybe Char
charDown (char -> Just (UiMsg.KeyDown, c)) = Just c
charDown _ = Nothing

midi :: Msg -> Maybe Midi.Message
midi (Midi (Midi.ReadMessage { Midi.rmsgMsg = msg })) = Just msg
midi _ = Nothing

context :: Msg -> Maybe UiMsg.Context
context (Ui (UiMsg.UiMsg context _)) = Just context
context _ = Nothing

contextTrack :: Msg -> Maybe (TrackNum, UiMsg.Track)
contextTrack = UiMsg.ctxTrack <=< context

contextTrackPos :: Msg -> Maybe (TrackNum, ScoreTime)
contextTrackPos msg = contextTrack msg >>= \(tracknum, t) -> case t of
    UiMsg.Track pos -> Just (tracknum, pos)
    _ -> Nothing
