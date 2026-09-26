-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
module Perform.Sc.Play (
    State(..)
    , play
    -- * stop
    , forceStop
    -- * thru
    , oscThru
    , noteOn, noteOff
    , setControl, pitchChange
    -- * initialize
    , version
    , addDefaultGroup
    , initializePatch
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
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Network as Network
import qualified Util.Num as Num
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified Derive.Controls as Controls
import qualified Derive.LEvent as LEvent
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Pitch as Pitch
import qualified Perform.Sc.Note as Note
import qualified Perform.Sc.Patch as Patch
import qualified Perform.Transport as Transport

import           Global
import           Types


-- | Talk to sclang IDE.  Use with OSCFunc.trace(true).
langPort :: Socket.PortNumber
langPort = 57120

-- | sclang starts the server with this port by default.  This is hardcoded for
-- now, but could be configured if necessary.
serverPort :: Socket.PortNumber
serverPort = 57110

data State = State
    -- | Communicate into the player.
    { _playControl :: Transport.PlayControl
    -- | Communicate out from the player.
    , _players :: Transport.ActivePlayers
    }

-- * NodeId

newtype NodeId = NodeId Int.Int32
    deriving (Eq, Show)

nodeIdsFrom :: NodeId -> [NodeId]
nodeIdsFrom (NodeId start) =
    NodeId start : nodeIdsFrom (NodeId next)
    where next = if start == maxBound then minNodeId else start + 1

-- I can't choose 1 because that's the default group.  I don't know if
-- there's some cutoff for
minNodeId :: Int.Int32
minNodeId = 10

-- * play

-- | Start a thread to stream a list of WriteMessages.
play :: State -> Note.PlayNotes -> Maybe RealTime
    -- ^ If given, loop back to the beginning when this time is reached.
    -> IO ()
play state pnotes repeatAt_ = do
    now <- Time.getCurrentTime
    Transport.playerStarted (_players state)
    let startId = timeToId now
    -- repeat_at_ has already had the shift and stretch applied, which is
    -- confusing.  TODO stop doing that.  But on the other hand, all the other
    -- stuff in PlayArgs had shift and stretch applied...
    let repeatAt = (+ Note.shift pnotes) . (/ Note.stretch pnotes) <$>
            repeatAt_
    Thread.startLogged "render sc" $
        playerThread state (convert tweak now startId pnotes repeatAt)
            `Exception.finally` Transport.playerStopped (_players state)
    pure ()

convert :: RealTime -> Time.UTCTime -> NodeId -> Note.PlayNotes
    -> Maybe RealTime -> [LEvent.LEvent OSC.OSCBundle]
convert tweak now startId pnotes repeatAt =
    toBundles now $ map (fmap (first place)) $
    notesToOsc startId $
    maybe (map (fmap (0,))) (cycleMessages (Note.shift pnotes)) repeatAt $
    Note.notes pnotes
    where
    place = (* Note.stretch pnotes) . subtract (Note.shift pnotes - tweak)

cycleMessages :: RealTime -> RealTime -> [LEvent.LEvent Note.Note]
    -> [LEvent.LEvent (RealTime, Note.Note)]
cycleMessages start repeatAt notes
    | null chunk = []
    | otherwise = concat $ chunk : chunks
    where
    chunk = map (fmap ((0,) . trimNote repeatAt)) $
        takeWhile (LEvent.logOr ((<repeatAt) . Note.start)) notes
    chunks = tail $ iterate shift stripped
    shift = map (fmap (first (+ (repeatAt - start))))
    stripped = map LEvent.Event (LEvent.eventsOf chunk)

trimNote :: RealTime -> Note.Note -> Note.Note
trimNote end note =
    note { Note.controls = Map.mapWithKey trim (Note.controls note) }
    where
    trim control sig
        | maybe True ((<=end) . fst) $ MSignal.last sig = sig
        | control == Note.gateId =
            MSignal.dropAtAfter end sig <> MSignal.fromPairs [(end, 0)]
        | otherwise = MSignal.dropAtAfter end sig

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
timeToId :: Time.UTCTime -> NodeId
timeToId now = NodeId $ minNodeId + fromIntegral (ms Bits..&. 0xffffff)
    where ms = Time.diffTimeToPicoseconds (Time.utctDayTime now) `div` 1000000

playerThread :: State -> [LEvent.LEvent OSC.OSCBundle] -> IO ()
playerThread state bundles = do
    playLoop state bundles
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.warn ("player died: " <> showt exc) -- game over

playLoop :: State -> [LEvent.LEvent OSC.OSCBundle] -> IO ()
playLoop state = flip Control.loop1 $ \loop bundles -> do
    now <- Time.getCurrentTime
    let until = Time.addUTCTime (writeAhead * 2) now
    let ((chunk, logs), rest) = first LEvent.partition $
            span ((<= OSC.timestampFromUTC until) . ltimeOf) bundles
    mapM_ Log.write logs
    -- unless (null chunk) $ do
    --     putStrLn "osc chunk: "
    --     mapM_ print chunk
    mapM_ (send serverPort . OSC.encodeOSCBundle) chunk
    let timeout = if null rest
            then maybe now (OSC.timestampToUTC . timeOf) (Lists.last chunk)
                `Time.diffUTCTime` now
            else writeAhead
    stop <- Transport.pollStopPlayer timeout (_playControl state)
    case (stop, rest) of
        (True, _) -> mapM_ sendOsc stopAll
        (_, []) -> pure ()
        _ -> loop rest
    where
    ltimeOf (LEvent.Log _) = OSC.Timestamp 0
    ltimeOf (LEvent.Event e) = timeOf e
    timeOf (OSC.OSCBundle ts _) = ts

writeAhead :: Time.NominalDiffTime
writeAhead = 1


-- * perform

toBundles :: Time.UTCTime -> [LEvent.LEvent (RealTime, OSC.OSC)]
    -> [LEvent.LEvent OSC.OSCBundle]
toBundles start = concatMap make . Lists.keyedGroupAdjacent timeOf
    where
    make (time, events) =
        map LEvent.Log logs ++ if null oscs then []
            else [LEvent.Event $ bundle t $ map snd oscs]
        where
        t = Time.addUTCTime (realToFrac time) start
        (oscs, logs) = LEvent.partition events

notesToOsc :: NodeId -> [LEvent.LEvent (RealTime, Note.Note)]
    -> [LEvent.LEvent (RealTime, OSC.OSC)]
notesToOsc startId =
    Lists.mergeAscLists timeOf . map (apply (uncurry noteToOsc))
        . LEvent.zip nodeIds
    where
    nodeIds = nodeIdsFrom startId
    apply f = \case
        LEvent.Event e -> map LEvent.Event (f e)
        LEvent.Log a -> [LEvent.Log a]

timeOf :: LEvent.LEvent (RealTime, x) -> RealTime
timeOf (LEvent.Log _) = -1/0
timeOf (LEvent.Event (t, _)) = t

noteToOsc :: NodeId -> (RealTime, Note.Note) -> [(RealTime, OSC.OSC)]
noteToOsc nodeId (offset, Note.Note patch start controls) =
    map (first (offset+)) $ (start, sNew patch nodeId initial)
        : dropWhile ((<=start) . fst) (controlOscs nodeId controls)
    where initial = map (second (MSignal.at start)) (Map.toAscList controls)

-- | Precondition: signals have already been trimmed to the right time range.
controlOscs :: NodeId -> Map Note.ControlId MSignal.Signal
    -> [(RealTime, OSC.OSC)]
controlOscs nodeId =
    map (second (nSet nodeId)) . Lists.groupAdjacentFst
        . Lists.mergeLists fst . map extract . Map.toAscList
    where
    extract (control, sig) =
        [(x, (control, y)) | (x, y) <- MSignal.toPairs sig]

sNew :: Note.PatchName -> NodeId -> [(Note.ControlId, Double)] -> OSC.OSC
sNew name (NodeId nodeId) controls = OSC.OSC "/s_new" $
    [ OSC_S name
    , OSC_I nodeId
    , OSC_I (fromIntegral (fromEnum Head))
    , let NodeId id = defaultGroup in OSC_I id
    ] ++ controlsToOsc controls

-- | sclang sets up this as the default group.  I think if I launch scsynth
-- standalone I'll have to create it.
defaultGroup :: NodeId
defaultGroup = NodeId 1

nSet :: NodeId -> [(Note.ControlId, Double)] -> OSC.OSC
nSet (NodeId nodeId) controls = OSC.OSC "/n_set" $
    OSC_I nodeId : controlsToOsc controls

clearSched :: OSC.OSC
clearSched = OSC.OSC "/clearSched" []

sNoid :: NodeId -> OSC.OSC
sNoid (NodeId id) = OSC.OSC "/s_noid" [OSC_I id]

controlsToOsc :: [(Note.ControlId, Double)] -> [OSCDatum]
controlsToOsc controls =
    [ a | (Note.ControlId control, val) <- controls
    , a <- [OSC_I control, OSC_F (Num.d2f val)]
    ]

bundle :: Time.UTCTime -> [OSC.OSC] -> OSC.OSCBundle
bundle time = OSC.OSCBundle (OSC.timestampFromUTC time) . map Right

data AddAction = Head | Tail | Before | After | Replace
    deriving (Eq, Ord, Show, Enum, Bounded)

-- * stop

stopAll :: [OSC.OSC]
stopAll =
    [ clearSched
    , nSet defaultGroup [(Note.gateId, 0)]
    ]

forceStop :: IO ()
forceStop = mapM_ sendOsc
    [ clearSched
    , gFreeAll defaultGroup
    ]

gFreeAll :: NodeId -> OSC.OSC
gFreeAll (NodeId id) = OSC.OSC "/g_freeAll" [OSC_I id]

-- * thru

-- Like MIDI thru, this is a vertical slice of convert and play for just a
-- single event, which is actually below the 'Note.Note' level.

-- | This is InputNote.NoteId, but I don't want a bunch of deps for a newtype
-- Int.
type NoteId = Int
type Triggered = Bool

noteToNode :: NoteId -> NodeId
noteToNode = NodeId . (+minNodeId) . fromIntegral

oscThru :: [OSC.OSC] -> IO ()
oscThru = mapM_ sendOsc

noteOn :: Patch.Patch -> Triggered -> NoteId -> Pitch.NoteNumber -> Double
    -> [OSC.OSC]
noteOn patch triggered noteId nn dyn =
    ( sNew (Patch.name patch) (noteToNode noteId) $
        (Note.gateId, 1) : mapMaybe convert
            [ (Patch.cPitch, Pitch.nnToDouble nn)
            , (Controls.dynamic, dyn)
            ]
    ) : if triggered
        -- TODO Should osc_thru should put a bit of space in between these?
        -- Or is separate packets enough separation for sc?
        then [nSet (noteToNode noteId) [(Note.gateId, 0)]]
        else []
    where
    convert (control, val) =
        (, val) <$> Map.lookup control (Patch.controls patch)

noteOff :: Triggered -> NoteId -> [OSC.OSC]
noteOff triggered noteId = concat
    [ if triggered then []
        else [nSet (noteToNode noteId) [(Note.gateId, 0)]]
    , [sNoid (noteToNode noteId)]
    ]

setControl :: Patch.Patch -> NoteId -> ScoreT.Control -> Double -> [OSC.OSC]
setControl patch noteId control val =
    case Map.lookup control (Patch.controls patch) of
        Nothing -> []
        Just c -> [nSet (noteToNode noteId) [(c, val)]]

pitchChange :: Patch.Patch -> NoteId -> Pitch.NoteNumber -> [OSC.OSC]
pitchChange patch noteId nn =
    case Map.lookup Patch.cPitch (Patch.controls patch) of
        Nothing -> []
        Just c -> [nSet (noteToNode noteId) [(c, Pitch.nnToDouble nn)]]

-- * initialize

-- | Left error if a valid server wasn't detected, Right msg if it was.
version :: IO (Either Text Text)
version = query serverPort (OSC.OSC "/version" []) >>= pure . \case
    Nothing -> Left $ "scsynth not running on " <> showt serverPort
    Just (Left err) -> Left $
        "unparseable response from scsynth on " <> showt serverPort <> ": "
        <> txt err
    Just (Right (OSC.OSC "/version.reply"
            (OSC_S name : OSC_I major : OSC_I minor : OSC_S patch : _))) ->
        Right $ mconcat
            [ Texts.toText name, " ", showt major, ".", showt minor
            , Texts.toText patch
            ]
    Just (Right osc) -> Left $
        "unexpected response from scsynth on " <> showt serverPort <> ": "
        <> showt osc

-- | The sclang IDE does this, but I have to do it myself for a standalone
-- scserver.
addDefaultGroup :: IO ()
addDefaultGroup = sendOsc $ OSC.OSC "/g_new"
    [OSC_I id, OSC_I (fromIntegral (fromEnum Head)), OSC_I 0]
    where
    NodeId id = defaultGroup

initializePatch :: Patch.Patch -> IO ()
initializePatch = loadPatch . Patch.filename

-- | Wait for patches to load.
sync :: IO ()
sync = void $ query serverPort (OSC.OSC "/sync" [])

loadPatch :: FilePath -> IO ()
loadPatch = sendOsc . dLoad

dLoad :: FilePath -> OSC.OSC
dLoad path = OSC.OSC "/d_load" [OSC_S (Texts.toByteString path)]

-- | Unfortunately this is kind of useless, because scsynth doesn't let you
-- unload all, and doesn't seem to have any way to see what's loaded.
dFree :: Note.PatchName -> OSC.OSC
dFree name = OSC.OSC "/d_free" [OSC_S (Texts.toByteString name)]


-- * low level

sendOsc :: OSC.OSC -> IO ()
sendOsc = send serverPort . OSC.encodeOSC

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
