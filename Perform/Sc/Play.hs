-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Perform.Sc.Play (
    server_port
    , State(..)
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
import qualified Vivid.OSC as OSC
import           Vivid.OSC (OSCDatum(..))

import qualified Util.Log as Log
import qualified Util.Network as Network
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.RealTime as RealTime
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
    , _monitor_control :: !Transport.PlayMonitorControl
    , _port :: !Socket.PortNumber
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
    Thread.startLogged "render sc" $ player_thread state $
        to_bundles now $
        map (first ((* Note.stretch pnotes) . subtract (Note.shift pnotes))) $
        notes_to_osc start_id $
        map (\n -> n { Note.duration = Note.duration n * Note.stretch pnotes })
        notes
    return ()
    where
    start_id = SynthId 10
    -- TODO
    -- Catch msgs up to realtime and cycle them if I'm repeating.
    -- process now repeat_at = shift_messages now . cycle_messages repeat_at

player_thread :: State -> [OSC.OSCBundle] -> IO ()
player_thread state bundles = do
    play_loop state bundles
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.warn ("player died: " <> showt exc)
    Transport.player_stopped (_monitor_control state)
    -- TODO finally do a /noid on any synths in scope.

play_loop :: State -> [OSC.OSCBundle] -> IO ()
play_loop state bundles = do
    now <- Time.getCurrentTime
    let until = Time.addUTCTime (write_ahead * 2) now
    let (chunk, rest) = span ((<= OSC.timestampFromUTC until) . time_of) bundles
    -- unless (null chunk) $ do
    --     putStrLn "osc chunk: "
    --     mapM_ print chunk
    mapM_ (send (_port state) . OSC.encodeOSCBundle) chunk
    let timeout = if null rest then 0 else write_ahead
    stop <- Transport.poll_stop_player timeout (_play_control state)
    case (stop, rest) of
        (True, _) -> send (_port state) $ OSC.encodeOSC clear_sched
            -- TODO long-duration notes will keep sounding.  Free default group?
        (_, []) -> return ()
        _ -> play_loop state rest
    where
    time_of (OSC.OSCBundle ts _) = ts

write_ahead :: Time.NominalDiffTime
write_ahead = 1

send :: Socket.PortNumber -> ByteString.ByteString -> IO ()
send port bytes = Network.withConnection (Network.UDP port) $ \hdl ->
    ByteString.hPut hdl bytes


-- * perform

newtype SynthId = SynthId Int.Int32
    deriving (Eq, Show)

to_bundles :: Time.UTCTime -> [(RealTime, OSC.OSC)] -> [OSC.OSCBundle]
to_bundles start = map make . Seq.keyed_group_adjacent fst
    where
    make (time, oscs) = bundle (Time.addUTCTime (realToFrac time) start)
        (map snd oscs)

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
note_to_osc synth_id (Note.Note patch start dur dur_control controls) =
    (start, s_new patch synth_id initial)
        : dropWhile ((<=start) . fst) (control_oscs synth_id controls)
    where
    initial = (dur_control, RealTime.to_seconds dur)
        : map (second (MSignal.at start)) (Map.toAscList controls)

control_oscs :: SynthId -> Map Note.ControlId MSignal.Signal
    -> [(RealTime, OSC.OSC)]
control_oscs synth_id controls =
    mapMaybe to_osc $ rotate_on fst $ map extract $ Map.toAscList controls
    where
    -- precondition: signal has already been trimmed to the right time range
    -- walk over all signals in parallel, and emit n_set for each time point.
    -- TODO Maybe simpler and just as efficient to just make separate lists and
    -- Seq.merge_lists them.
    extract (control, sig) =
        [(x, (control, y)) | (x, y) <- MSignal.to_pairs sig]
    to_osc :: [(MSignal.X, (Note.ControlId, MSignal.Y))]
        -> Maybe (RealTime, OSC.OSC)
    to_osc cs@((x, _) : _) = Just (x, n_set synth_id (map snd cs))
    to_osc [] = Nothing

s_new :: Note.PatchName -> SynthId -> [(Note.ControlId, Double)] -> OSC.OSC
s_new name (SynthId synth_id) controls = OSC.OSC "/s_new" $
    [ OSC_S name
    , OSC_I synth_id
    , OSC_I (fromIntegral (fromEnum Head))
    , OSC_I default_node_id
    ] ++ controls_to_osc controls
    where
    -- I think 1 to go in the default group?
    default_node_id = 1

n_set :: SynthId -> [(Note.ControlId, Double)] -> OSC.OSC
n_set (SynthId synth_id) controls = OSC.OSC "/n_set" $
    OSC_I synth_id : controls_to_osc controls

clear_sched :: OSC.OSC
clear_sched = OSC.OSC "/clearSched" []

controls_to_osc :: [(Note.ControlId, Double)] -> [OSCDatum]
controls_to_osc controls =
    [ a | (Note.ControlId control, val) <- controls
    , a <- [OSC_I control, OSC_F (Num.d2f val)]
    ]

bundle :: Time.UTCTime -> [OSC.OSC] -> OSC.OSCBundle
bundle time = OSC.OSCBundle (OSC.timestampFromUTC time) . map Right

data AddAction = Head | Tail | Before | After | Replace
    deriving (Eq, Ord, Show, Enum, Bounded)


-- * util

rotate_on :: Eq k => (a -> k) -> [[a]] -> [[a]]
rotate_on key = go
    where
    go xss = case break1 key xss of
        ([], []) -> []
        (xs, xss) -> xs : go xss

-- | Break off the first element of each of the lists that compare equal
-- by the key function.
break1 :: Eq k => (a -> k) -> [[a]] -> ([a], [[a]])
break1 key = go
    where
    go [] = ([], [])
    go ([] : xs) = go xs
    go ((x0 : xs0) : xss) = bimap (x0:) (xs0:) $ get (key x0) xss
    get _ [] = ([], [])
    get v ([] : xss) = get v xss
    get v (xs@(x0 : xs0) : xss)
        | v == key x0 = bimap (x0:) (xs0:) $ get v xss
        | otherwise = second (xs:) $ get v xss
