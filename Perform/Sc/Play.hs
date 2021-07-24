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
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString
import qualified Vivid.OSC as OSC
import           Vivid.OSC (OSCDatum(..))

import qualified Util.Control as Control
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

newtype NodeId = NodeId Int.Int32
    deriving (Eq, Show)

node_ids_from :: NodeId -> [NodeId]
node_ids_from (NodeId start) =
    NodeId start : node_ids_from (NodeId next)
    where next = if start == maxBound then min_node_id else start + 1

-- I can't choose 1 because that's the default group.  I don't know if
-- there's some cutoff for
min_node_id :: Int.Int32
min_node_id = 10

-- | Start a thread to stream a list of WriteMessages.
play :: State -> Note.PlayNotes -> Maybe RealTime
    -- ^ If given, loop back to the beginning when this time is reached.
    -> IO ()
play state pnotes repeat_at = do
    now <- Time.getCurrentTime
    -- TODO Does this spoil streaming?  Why yes it does.  But it's too annoynig
    -- to try to thread LEvent through notes_to_osc.
    let (notes, logs) = LEvent.partition $ Note.notes pnotes
    mapM_ Log.write logs
    Transport.player_started (_players state)
    let start_id = time_to_id now
    Thread.startLogged "render sc" $ do
        thread now start_id notes
            `Exception.finally` Transport.player_stopped (_players state)
    return ()
    where
    thread now start_id = player_thread state . to_bundles now
        . map (first ((* Note.stretch pnotes) . subtract (Note.shift pnotes)))
        . notes_to_osc start_id

-- | This is a silly solution to a silly problem.  NodeIds are 31-bit (must be
-- positive) numbers to uniquely identify a note.  In an imperative language
-- it's easy to get a unique supply by incrementing, but to do that here I'd
-- have to involve streams, or unsafePerformIO, or give up the nice lazy list
-- structure.  So I'll use the lower bits of milliseconds to pick IDs that are
-- unlikely to collide with whatever note happens to be decaying since the last
-- play.  /noid is supposed to be used for this, but it doesn't work on groups,
-- so without knowing everyone's decay time, I can't know how many IDs to
-- remember to cancel.
time_to_id :: Time.UTCTime -> NodeId
time_to_id now = NodeId $ min_node_id + fromIntegral (ms Bits..&. 0xffffff)
    where ms = Time.diffTimeToPicoseconds (Time.utctDayTime now) `div` 1000000

player_thread :: State -> [OSC.OSCBundle] -> IO ()
player_thread state bundles = do
    play_loop state bundles
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.warn ("player died: " <> showt exc) -- game over

play_loop :: State -> [OSC.OSCBundle] -> IO ()
play_loop state = flip Control.loop1 $ \loop bundles -> do
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

to_bundles :: Time.UTCTime -> [(RealTime, OSC.OSC)] -> [OSC.OSCBundle]
to_bundles start = map make . Seq.keyed_group_adjacent fst
    where
    make (time, oscs) =
        bundle (Time.addUTCTime (realToFrac time) start) (map snd oscs)

-- -- TODO use LEvents to avoid spoiling streaming
-- -- notes_to_osc2 :: NodeId -> [LEvent.LEvent Note.Note] -> [(RealTime, OSC.OSC)]
-- notes_to_osc2 (NodeId start_id) notes =
--     -- Seq.merge_asc_lists time_of $
--     -- join $
--     -- [LEvent [(RealTime, OSC)]]
--     map (fmap (uncurry note_to_osc)) $
--         LEvent.zip node_ids notes
--     where
--     node_ids = map NodeId [start_id ..]
--     time_of (LEvent.Log _) = -1/0
--     time_of (LEvent.Event (t, _)) = t
--
-- join :: [LEvent.LEvent [a]] -> [LEvent.LEvent a]
-- join = concatMap $ \case
--     LEvent.Log log -> [LEvent.Log log]
--     LEvent.Event notes -> map LEvent.Event notes

notes_to_osc :: NodeId -> [Note.Note] -> [(RealTime, OSC.OSC)]
notes_to_osc start_id notes =
    Seq.merge_asc_lists fst $ map (uncurry note_to_osc) $ zip node_ids notes
    where node_ids = node_ids_from start_id

note_to_osc :: NodeId -> Note.Note -> [(RealTime, OSC.OSC)]
note_to_osc node_id (Note.Note patch start controls) =
    (start, s_new patch node_id initial)
        : dropWhile ((<=start) . fst) (control_oscs node_id controls)
    where
    initial = map (second (MSignal.at start)) (Map.toAscList controls)

-- | Precondition: signals have already been trimmed to the right time range.
control_oscs :: NodeId -> Map Note.ControlId MSignal.Signal
    -> [(RealTime, OSC.OSC)]
control_oscs node_id =
    map (second (n_set node_id)) . Seq.group_adjacent_fst
        . Seq.merge_lists fst . map extract . Map.toAscList
    where
    extract (control, sig) =
        [(x, (control, y)) | (x, y) <- MSignal.to_pairs sig]

s_new :: Note.PatchName -> NodeId -> [(Note.ControlId, Double)] -> OSC.OSC
s_new name (NodeId node_id) controls = OSC.OSC "/s_new" $
    [ OSC_S name
    , OSC_I node_id
    , OSC_I (fromIntegral (fromEnum Head))
    , let NodeId id = default_group in OSC_I id
    ] ++ controls_to_osc controls

-- | sclang sets up this as the default group.  I think if I launch scsynth
-- standalone I'll have to create it.
default_group :: NodeId
default_group = NodeId 1

stop_all :: [OSC.OSC]
stop_all =
    [ clear_sched
    , n_set default_group [(Note.gate_id, 0)]
    ]

n_set :: NodeId -> [(Note.ControlId, Double)] -> OSC.OSC
n_set (NodeId node_id) controls = OSC.OSC "/n_set" $
    OSC_I node_id : controls_to_osc controls

clear_sched :: OSC.OSC
clear_sched = OSC.OSC "/clearSched" []

force_stop_all :: OSC.OSC
force_stop_all = g_freeAll default_group

g_freeAll :: NodeId -> OSC.OSC
g_freeAll (NodeId id) = OSC.OSC "/g_freeAll" [OSC_I id]

s_noid :: NodeId -> OSC.OSC
s_noid (NodeId id) = OSC.OSC "/s_noid" [OSC_I id]

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
