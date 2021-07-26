-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Perform.Sc.Play (
    State(..)
    , play
    -- * stop
    , force_stop
    -- * initialize
    , version
    , initialize_patch
    , sync
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
import qualified Util.Exceptions as Exceptions
import qualified Util.Log as Log
import qualified Util.Network as Network
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Sc.Note as Note
import qualified Perform.Sc.Patch as Patch
import qualified Perform.Transport as Transport

import           Global
import           Types


-- | Talk to sclang IDE.  Use with OSCFunc.trace(true).
lang_port :: Socket.PortNumber
lang_port = 57120

-- | sclang starts the server with this port by default.  This is hardcoded for
-- now, but could be configured if necessary.
server_port :: Socket.PortNumber
server_port = 57110

data State = State {
    -- | Communicate into the player.
    _play_control :: !Transport.PlayControl
    -- | Communicate out from the player.
    , _players :: !Transport.ActivePlayers
    }

-- * NodeId

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

-- * play

-- | Start a thread to stream a list of WriteMessages.
play :: State -> Note.PlayNotes -> Maybe RealTime
    -- ^ If given, loop back to the beginning when this time is reached.
    -> IO ()
play state pnotes repeat_at_ = do
    now <- Time.getCurrentTime
    Transport.player_started (_players state)
    let start_id = time_to_id now
    -- repeat_at_ has already had the shift and stretch applied, which is
    -- confusing.  TODO stop doing that.  But on the other hand, all the other
    -- stuff in PlayArgs had shift and stretch applied...
    let repeat_at = (+ Note.shift pnotes) . (/ Note.stretch pnotes) <$>
            repeat_at_
    Thread.startLogged "render sc" $
        player_thread state (convert tweak now start_id pnotes repeat_at)
            `Exception.finally` Transport.player_stopped (_players state)
    return ()

convert :: RealTime -> Time.UTCTime -> NodeId -> Note.PlayNotes
    -> Maybe RealTime -> [LEvent.LEvent OSC.OSCBundle]
convert tweak now start_id pnotes repeat_at =
    to_bundles now $ map (fmap (first place)) $
    notes_to_osc start_id $
    maybe (map (fmap (0,))) (cycle_messages (Note.shift pnotes)) repeat_at $
    Note.notes pnotes
    where
    place = (* Note.stretch pnotes) . subtract (Note.shift pnotes - tweak)

cycle_messages :: RealTime -> RealTime -> [LEvent.LEvent Note.Note]
    -> [LEvent.LEvent (RealTime, Note.Note)]
cycle_messages start repeat_at notes
    | null chunk = []
    | otherwise = concat $ chunk : chunks
    where
    chunk = map (fmap ((0,) . trim_note repeat_at)) $
        takeWhile (LEvent.log_or ((<repeat_at) . Note.start)) notes
    chunks = tail $ iterate shift stripped
    shift = map (fmap (first (+ (repeat_at - start))))
    stripped = map LEvent.Event (LEvent.events_of chunk)

trim_note :: RealTime -> Note.Note -> Note.Note
trim_note end note =
    note { Note.controls = Map.mapWithKey trim (Note.controls note) }
    where
    trim control sig
        | maybe True ((<=end) . fst) $ MSignal.last sig = sig
        | control == Note.gate_id =
            MSignal.drop_at_after end sig <> MSignal.from_pairs [(end, 0)]
        | otherwise = MSignal.drop_at_after end sig

-- | For some reason, sc sound winds up slightly before MIDI, but I can't be
-- bothered to track down why.  This delays the OSC by enough to sound close
-- enough to MIDI attacks.
tweak :: RealTime
tweak = 0.03

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

player_thread :: State -> [LEvent.LEvent OSC.OSCBundle] -> IO ()
player_thread state bundles = do
    play_loop state bundles
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.warn ("player died: " <> showt exc) -- game over

play_loop :: State -> [LEvent.LEvent OSC.OSCBundle] -> IO ()
play_loop state = flip Control.loop1 $ \loop bundles -> do
    now <- Time.getCurrentTime
    let until = Time.addUTCTime (write_ahead * 2) now
    let ((chunk, logs), rest) = first LEvent.partition $
            span ((<= OSC.timestampFromUTC until) . ltime_of) bundles
    mapM_ Log.write logs
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
    ltime_of (LEvent.Log _) = OSC.Timestamp 0
    ltime_of (LEvent.Event e) = time_of e
    time_of (OSC.OSCBundle ts _) = ts

write_ahead :: Time.NominalDiffTime
write_ahead = 1


-- * perform

to_bundles :: Time.UTCTime -> [LEvent.LEvent (RealTime, OSC.OSC)]
    -> [LEvent.LEvent OSC.OSCBundle]
to_bundles start = concatMap make . Seq.keyed_group_adjacent time_of
    where
    make (time, events) =
        map LEvent.Log logs ++ if null oscs then []
            else [LEvent.Event $ bundle t $ map snd oscs]
        where
        t = Time.addUTCTime (realToFrac time) start
        (oscs, logs) = LEvent.partition events

notes_to_osc :: NodeId -> [LEvent.LEvent (RealTime, Note.Note)]
    -> [LEvent.LEvent (RealTime, OSC.OSC)]
notes_to_osc start_id =
    Seq.merge_asc_lists time_of . map (apply (uncurry note_to_osc))
        . LEvent.zip node_ids
    where
    node_ids = node_ids_from start_id
    apply f = \case
        LEvent.Event e -> map LEvent.Event (f e)
        LEvent.Log a -> [LEvent.Log a]

time_of :: LEvent.LEvent (RealTime, x) -> RealTime
time_of (LEvent.Log _) = -1/0
time_of (LEvent.Event (t, _)) = t

note_to_osc :: NodeId -> (RealTime, Note.Note) -> [(RealTime, OSC.OSC)]
note_to_osc node_id (offset, Note.Note patch start controls) =
    map (first (offset+)) $ (start, s_new patch node_id initial)
        : dropWhile ((<=start) . fst) (control_oscs node_id controls)
    where initial = map (second (MSignal.at start)) (Map.toAscList controls)

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

n_set :: NodeId -> [(Note.ControlId, Double)] -> OSC.OSC
n_set (NodeId node_id) controls = OSC.OSC "/n_set" $
    OSC_I node_id : controls_to_osc controls

clear_sched :: OSC.OSC
clear_sched = OSC.OSC "/clearSched" []

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

-- * stop

stop_all :: [OSC.OSC]
stop_all =
    [ clear_sched
    , n_set default_group [(Note.gate_id, 0)]
    ]

force_stop :: IO ()
force_stop = mapM_ (send server_port . OSC.encodeOSC)
    [ clear_sched
    , g_freeAll default_group
    ]

g_freeAll :: NodeId -> OSC.OSC
g_freeAll (NodeId id) = OSC.OSC "/g_freeAll" [OSC_I id]

-- * initialize

-- | Left error if a valid server wasn't detected, Right msg if it was.
version :: IO (Either Text Text)
version = query server_port (OSC.OSC "/version" []) >>= return . \case
    Nothing -> Left $ "scsynth not running on " <> showt server_port
    Just (Left err) -> Left $
        "unparseable response from scsynth on " <> showt server_port <> ": "
        <> txt err
    Just (Right (OSC.OSC "/version.reply"
            (OSC_S name : OSC_I major : OSC_I minor : OSC_S patch : _))) ->
        Right $ mconcat
            [ Texts.toText name, " ", showt major, ".", showt minor
            , Texts.toText patch
            ]
    Just (Right osc) -> Left $
        "unexpected response from scsynth on " <> showt server_port <> ": "
        <> showt osc

initialize_patch :: Patch.Patch -> IO ()
initialize_patch = load_patch . Patch.filename

-- | Wait for patches to load.
sync :: IO ()
sync = void $ query server_port (OSC.OSC "/sync" [])

load_patch :: FilePath -> IO ()
load_patch = send server_port . OSC.encodeOSC . d_load

d_load :: FilePath -> OSC.OSC
d_load path = OSC.OSC "/d_load" [OSC_S (Texts.toByteString path)]

d_free :: Note.PatchName -> OSC.OSC
d_free name = OSC.OSC "/d_free" [OSC_S (Texts.toByteString name)]


-- * low level

send :: Socket.PortNumber -> ByteString.ByteString -> IO ()
send port bytes = Network.withConnection (Network.UDP port) $ \socket ->
    void $ Socket.ByteString.send socket bytes

-- | Send an OSC and expect a response.  Nothing if there was no server.
-- Otherwise, this could hang if the server does, throw an IO exception, or
-- return Left if the response couldn't be parsed.
query :: Socket.PortNumber -> OSC.OSC -> IO (Maybe (Either String OSC.OSC))
query port osc = Network.withConnection (Network.UDP port) $ \socket -> do
    Socket.ByteString.send socket $ OSC.encodeOSC osc
    fmap OSC.decodeOSC <$>
        Exceptions.ignoreEnoent (Socket.ByteString.recv socket 4096)
