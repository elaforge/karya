-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Perform.Sc.Play (
    State(..)
    , play
#ifdef TESTING
    , module Perform.Sc.Play
#endif
) where
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString
import qualified Vivid.OSC as OSC
import           Vivid.OSC (OSCDatum(..))

import qualified Util.Log as Log
import qualified Util.Network as Network
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Sc.Note as Note
import qualified Perform.Transport as Transport

import           Global
import           Types


-- | Talk to sclang IDE.  Use with OSCFunc.trace(true).
lang_port :: Socket.PortNumber
lang_port = 57120

-- | sclang starts the server with this by default.
server_port :: Socket.PortNumber
server_port = 57110

data State = State {
    -- | Communicate into the player.
    _play_control :: !Transport.PlayControl
    -- | Communicate out from the player.
    , _players :: !Transport.ActivePlayers
    }

-- | Start a thread to stream a list of WriteMessages.
play :: State -> Note.PlayNotes -> Maybe RealTime
    -- ^ If given, loop back to the beginning when this time is reached.
    -> IO ()
play state pnotes repeat_at = do
    now <- Time.getCurrentTime
    -- TODO Does this spoil streaming?  Why yes it does.  But it's too annoynig
    -- to try to thread LEvent through notes_to_osc.
    let (notes, logs) = LEvent.partition $ Note.notes pnotes
    Transport.player_started (_players state)
    Thread.startLogged "render sc" $ thread now notes
        `Exception.finally` Transport.player_stopped (_players state)
    return ()
    where
    thread now = player_thread state . to_bundles now
        . map (first ((* Note.stretch pnotes) . subtract (Note.shift pnotes)))
        . notes_to_osc start_id . map modify
    modify n = n { Note.duration = Note.duration n * Note.stretch pnotes }
    start_id = SynthId 10

player_thread :: State -> [OSC.OSCBundle] -> IO ()
player_thread state bundles = do
    play_loop state bundles
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.warn ("player died: " <> showt exc)
    -- TODO finally do a /noid on any synths in scope.

loop1 :: forall state a. ((state -> a) -> state -> a) -> state -> a
loop1 go = go again
    where
    again :: state -> a
    again = go again

play_loop :: State -> [OSC.OSCBundle] -> IO ()
play_loop state = loop1 $ \loop bundles -> do
    now <- Time.getCurrentTime
    let until = Time.addUTCTime (write_ahead * 2) now
    let (chunk, rest) = span ((<= OSC.timestampFromUTC until) . time_of) bundles
    -- unless (null chunk) $ do
    --     putStrLn "osc chunk: "
    --     mapM_ print chunk
    mapM_ (send server_port . OSC.encodeOSCBundle) chunk
    let timeout = if null rest
            then maybe now (OSC.timestampToUTC . time_of) (Seq.last chunk)
                `Time.diffUTCTime` now
            else write_ahead
    stop <- Transport.poll_stop_player timeout (_play_control state)
    case (stop, rest) of
        (True, _) -> mapM_ (send server_port . OSC.encodeOSC) stop_all
        (_, []) -> return ()
        _ -> loop rest
    where
    time_of (OSC.OSCBundle ts _) = ts

write_ahead :: Time.NominalDiffTime
write_ahead = 1

send :: Socket.PortNumber -> ByteString.ByteString -> IO ()
send port bytes = Network.withConnection (Network.UDP port) $ \socket ->
    void $ Socket.ByteString.send socket bytes


-- * perform

newtype SynthId = SynthId Int.Int32
    deriving (Eq, Show)

to_bundles :: Time.UTCTime -> [(RealTime, OSC.OSC)] -> [OSC.OSCBundle]
to_bundles start = map make . Seq.keyed_group_adjacent fst
    where
    make (time, oscs) =
        bundle (Time.addUTCTime (realToFrac time) start) (map snd oscs)

-- TODO use LEvents to avoid spoiling streaming
-- -- notes_to_osc2 :: SynthId -> [LEvent.LEvent Note.Note] -> [(RealTime, OSC.OSC)]
-- notes_to_osc2 (SynthId start_id) notes =
--     -- Seq.merge_asc_lists time_of $
--     -- join $
--     -- [LEvent [(RealTime, OSC)]]
--     map (fmap (uncurry note_to_osc)) $
--         LEvent.zip synth_ids notes
--     where
--     synth_ids = map SynthId [start_id ..]
--     time_of (LEvent.Log _) = -1/0
--     time_of (LEvent.Event (t, _)) = t
--
-- join :: [LEvent.LEvent [a]] -> [LEvent.LEvent a]
-- join = concatMap $ \case
--     LEvent.Log log -> [LEvent.Log log]
--     LEvent.Event notes -> map LEvent.Event notes

notes_to_osc :: SynthId -> [Note.Note] -> [(RealTime, OSC.OSC)]
notes_to_osc (SynthId start_id) notes =
    Seq.merge_asc_lists fst $ map (uncurry note_to_osc) $ zip synth_ids notes
    where synth_ids = map SynthId [start_id ..]

note_to_osc :: SynthId -> Note.Note -> [(RealTime, OSC.OSC)]
note_to_osc synth_id (Note.Note patch start dur controls) =
    (start, s_new patch synth_id initial)
        : dropWhile ((<=start) . fst) (control_oscs synth_id controls)
    where initial = map (second (MSignal.at start)) (Map.toAscList controls)

-- | Precondition: signals have already been trimmed to the right time range.
control_oscs :: SynthId -> Map Note.ControlId MSignal.Signal
    -> [(RealTime, OSC.OSC)]
control_oscs synth_id =
    map (second (n_set synth_id)) . Seq.group_adjacent_fst
        . Seq.merge_lists fst . map extract . Map.toAscList
    where
    extract (control, sig) =
        [(x, (control, y)) | (x, y) <- MSignal.to_pairs sig]

s_new :: Note.PatchName -> SynthId -> [(Note.ControlId, Double)] -> OSC.OSC
s_new name (SynthId synth_id) controls = OSC.OSC "/s_new" $
    [ OSC_S name
    , OSC_I synth_id
    , OSC_I (fromIntegral (fromEnum Head))
    , let SynthId id = default_group in OSC_I id
    ] ++ controls_to_osc controls

-- | sclang sets up this as the default group.  I think if I launch scsynth
-- standalone I'll have to create it.
default_group :: SynthId
default_group = SynthId 1

stop_all :: [OSC.OSC]
stop_all =
    [ clear_sched
    , n_set default_group [(Note.gate_id, 0)]
    ]

n_set :: SynthId -> [(Note.ControlId, Double)] -> OSC.OSC
n_set (SynthId synth_id) controls = OSC.OSC "/n_set" $
    OSC_I synth_id : controls_to_osc controls

clear_sched :: OSC.OSC
clear_sched = OSC.OSC "/clearSched" []

force_stop_all :: OSC.OSC
force_stop_all = g_freeAll default_group

g_freeAll :: SynthId -> OSC.OSC
g_freeAll (SynthId id) = OSC.OSC "/g_freeAll" [OSC_I id]

s_noid :: SynthId -> OSC.OSC
s_noid (SynthId id) = OSC.OSC "/s_noid" [OSC_I id]

controls_to_osc :: [(Note.ControlId, Double)] -> [OSCDatum]
controls_to_osc controls =
    [ a | (Note.ControlId control, val) <- controls
    , a <- [OSC_I control, OSC_F (Num.d2f val)]
    ]

bundle :: Time.UTCTime -> [OSC.OSC] -> OSC.OSCBundle
bundle time = OSC.OSCBundle (OSC.timestampFromUTC time) . map Right

data AddAction = Head | Tail | Before | After | Replace
    deriving (Eq, Ord, Show, Enum, Bounded)

-- * patches

load_patch :: FilePath -> IO ()
load_patch = send server_port . OSC.encodeOSC . d_load

d_load :: FilePath -> OSC.OSC
d_load path = OSC.OSC "/d_load" [OSC_S (Texts.toByteString path)]
