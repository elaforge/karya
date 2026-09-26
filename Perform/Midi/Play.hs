-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
-- | This module is responsible for actually scheduling MIDI messages with the
-- OS's MIDI driver.
module Perform.Midi.Play (State(..), play, cycleMessages) where
import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Cmd.Cmd as Cmd
import qualified Derive.LEvent as LEvent
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Midi.Mmc as Mmc

import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport

import           Global
import           Types


-- | Access to info that's needed by a particular run of the player.
-- This is read-only, and shouldn't need to be modified.
data State = State
    -- | Communicate into the player.
    { _playControl :: Transport.PlayControl
    -- | Communicate out from the player.
    , _players :: Transport.ActivePlayers
    , _info :: Transport.Info
    , _imEnd :: Maybe RealTime
    }

type Messages = [LEvent.LEvent Midi.WriteMessage]

-- | Start a thread to stream a list of WriteMessages.
play :: State -> Maybe Cmd.SyncConfig -> Text -> Messages -> Maybe RealTime
    -- ^ If given, loop back to the beginning when this time is reached.
    -> IO ()
play state sync name msgs repeatAt = do
    now <- Transport.infoGetCurrentTime (_info state)
    Transport.playerStarted (_players state)
    Thread.startLogged "render midi" $ thread now
        `Exception.finally` Transport.playerStopped (_players state)
    pure ()
    where
    thread now = playerThread
        -- Don't send MMC if I'm repeating, it'll just confuse the DAW.
        (if Maybe.isJust repeatAt then Nothing else sync)
        now name
        (state { _imEnd = (now+) <$> _imEnd state })
        (process now repeatAt msgs)
    -- Catch msgs up to realtime and cycle them if I'm repeating.
    process now repeatAt = shiftMessages now
        . maybe id cycleMessages repeatAt

playerThread :: Maybe Cmd.SyncConfig -> RealTime -> Text -> State
    -> Messages -> IO ()
playerThread maybeSync start name state msgs = do
    Log.debug $ "play start: " <> name
    case maybeSync of
        Just sync | not (Cmd.syncMtc sync) ->
            stateWriteMidi state $ makeMmc sync start Mmc.Play
        _ -> pure ()
    playLoop state msgs
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.warn ("player died: " <> showt exc)
    case maybeSync of
        Just sync | not (Cmd.syncMtc sync) ->
            stateWriteMidi state $ makeMmc sync start Mmc.Stop
        _ -> pure ()
    Log.debug $ "play complete: " <> name

makeMmc :: Cmd.SyncConfig -> RealTime -> Mmc.Mmc -> Midi.WriteMessage
makeMmc sync start msg = Midi.WriteMessage (Cmd.syncDevice sync) start $
    Mmc.encode (Cmd.syncDeviceId sync) msg

cycleMessages :: RealTime -> Messages -> Messages
cycleMessages _ [] = []
cycleMessages repeatAt msgs =
    go1 $ takeWhile (LEvent.logOr ((<repeatAt) . Midi.wmsgTs)) msgs
    where
    go1 msgs = msgs ++ go (shiftMessages repeatAt (strip msgs))
    -- Don't bother writing logs the second time around.
    go [] = []
    go msgs = msgs ++ go (shiftMessages repeatAt msgs)
    strip = map LEvent.Event . LEvent.eventsOf

shiftMessages :: RealTime -> Messages -> Messages
shiftMessages ts = map (fmap (Midi.addTimestamp ts))

-- * loop

-- | 'play_loop' tries to not get too far ahead of now both to avoid flooding
-- the midi driver and so a stop will happen fairly quickly.
writeAhead :: RealTime
writeAhead = RealTime.seconds 1

stateWrite :: State -> Interface.Message -> IO ()
stateWrite state = Transport.infoMidiWriter (_info state)

stateWriteMidi :: State -> Midi.WriteMessage -> IO ()
stateWriteMidi state = stateWrite state . Interface.Midi

-- | @devs@ keeps track of devices that have been seen, so I know which devices
-- to reset.
playLoop :: State -> Messages -> IO ()
playLoop state msgs = do
    -- This should make the buffer always be between write_ahead*2 and
    -- write_ahead ahead of now.
    now <- Transport.infoGetCurrentTime (_info state)
    let until = now + RealTime.mul writeAhead 2
    let (chunk, rest) =
            span (LEvent.either ((<until) . Midi.wmsgTs) (const True)) msgs
    -- Log.debug $ "play at " ++ show now ++ " chunk: " ++ show (length chunk)
    mapM_ (LEvent.either (stateWriteMidi state) Log.write) chunk

    -- Even if all the events have been scheduled, don't quit until they should
    -- have been played, so the monitor will hang on as long as there are still
    -- notes that can be cancelled.
    -- when (null rest) $
    --     putStrLn $ "MIDI end delay: " <> show (_im_end state) <> ": "
    --         <> show (get_end now chunk - now)
    let timeout = if null rest then getEnd now chunk - now else writeAhead
    stop <- Transport.pollStopPlayer (RealTime.toDiff timeout)
        (_playControl state)
    let resetMidi = stateWrite state (Interface.AllNotesOff now)
    case (stop, rest) of
        (True, _) -> do
            Transport.infoMidiAbort (_info state)
            resetMidi
        -- I used to reset when MIDI ran out, and previously was correct
        -- because it only sent specific note offs, but if I use a real
        -- AllNotesOff then it breaks im.  I could have im not use AllNotesOff,
        -- but let's just remove the reset_midi here.
        (_, []) -> pure () -- reset_midi
        _ -> playLoop state rest
    where
    getEnd now chunk = fromMaybe now $ max
        (Midi.wmsgTs <$> Lists.last (LEvent.eventsOf chunk))
        (_imEnd state)
