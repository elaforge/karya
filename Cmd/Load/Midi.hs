-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert a midi file to a block.
module Cmd.Load.Midi (
    load
    , parse, convert
    -- * testing
    , Midi, NoteTrack(..)
    , convertTracks, splitTrack, collectNotes
) where
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Word as Word

import qualified ZMidi.Core as Z

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Maps as Maps

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Derive.Attrs as Attrs
import qualified Derive.Controls as Controls
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Common as Common
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui

import           Global
import           Types


type Warn = Text

load :: FilePath -> Cmd.CmdT IO BlockId
load fn = liftIO (parse fn) >>= \case
    Left err -> Cmd.throw $ "parsing " <> showt fn <> ": " <> err
    Right midiFile -> do
        let (tracks, skel, warns) = convert midiFile
        mapM_ (Log.warn . ((txt fn <> ": ") <>)) warns
        create tracks skel

create :: Ui.M m => [(Text, Track)] -> Skeleton.Skeleton -> m BlockId
create tracks skel = do
    blockId <- Create.block Ui.noRuler
    mapM_ (addTrack blockId) tracks
    Ui.setSkeleton blockId skel
    pure blockId
    where
    addTrack blockId (title, track) =
        Create.track blockId 9999 title $
            Events.fromList [Event.event start dur text
                | (start, (dur, text)) <- Map.toAscList track]

parse :: FilePath -> IO (Either Warn Z.MidiFile)
parse fn = first showError <$> Z.readMidi fn
    where showError (Z.ParseErr pos msg) = showt pos <> ": " <> txt msg

convert :: Z.MidiFile -> ([(Text, Track)], Skeleton.Skeleton, [Warn])
convert = convertTracks . extractTracks

-- * extract

type Midi = (RealTime, Midi.Message)

extractTracks :: Z.MidiFile -> [(Text, [Midi])]
extractTracks (Z.MidiFile header tracks) =
    filter (not . null . snd) $ map (extractTrack perSec) tracks
    where
    perSec = RealTime.seconds . fromIntegral $ case Z.time_division header of
        Z.FPS val -> val
        Z.TPB val -> val

extractTrack :: RealTime -> Z.MidiTrack -> (Text, [Midi])
extractTrack perSec (Z.MidiTrack msgs) =
    (name, concatMap extractMessage (zip times (map snd msgs)))
    where
    name = maybe "" txt $ Lists.head
        [name | Z.MetaEvent (Z.TextEvent _ name) <- take 10 (map snd msgs)]
    times = drop 1 $ scanl (+) 0 $
        map ((/perSec) . RealTime.seconds . fromIntegral . fst) msgs

extractMessage :: (RealTime, Z.MidiEvent) -> [Midi]
extractMessage (time, msg) = case msg of
    Z.VoiceEvent _ midi -> (:[]) . (,) time $ case midi of
        Z.NoteOff chan key vel ->
            Midi.ChannelMessage chan (Midi.NoteOff (Midi.toKey key) vel)
        Z.NoteOn chan key vel ->
            Midi.ChannelMessage chan (Midi.NoteOn (Midi.toKey key) vel)
        Z.NoteAftertouch chan key val ->
            Midi.ChannelMessage chan (Midi.Aftertouch (Midi.toKey key) val)
        Z.Controller chan cc val ->
            Midi.ChannelMessage chan (Midi.ControlChange cc val)
        Z.ProgramChange chan program ->
            Midi.ChannelMessage chan (Midi.ProgramChange program)
        Z.ChanAftertouch chan val ->
            Midi.ChannelMessage chan (Midi.ChannelPressure val)
        Z.PitchBend chan val ->
            Midi.ChannelMessage chan
                (Midi.PitchBend (fromIntegral val / 2^13 - 2^13))
    _ -> []


-- * convert

-- | (note, pitch, controls)
data NoteTrack = NoteTrack Track Track (Map ScoreT.Control Track)
    deriving (Show)
-- | Map start (dur, text)
type Track = Map ScoreTime (ScoreTime, Text)

instance Semigroup NoteTrack where
    (<>)    (NoteTrack notes1 pitches1 controls1)
            (NoteTrack notes2 pitches2 controls2) =
        NoteTrack (notes1 <> notes2) (pitches1 <> pitches2)
            (Maps.mappend controls1 controls2)
instance Monoid NoteTrack where
    mempty = NoteTrack mempty mempty mempty
    mappend = (<>)

-- | Take flat MIDI msgs to a list of tracks where events don't overlap, and
-- add pitch and control tracks.
convertTracks :: [(Text, [Midi])]
    -> ([(Text, Track)], Skeleton.Skeleton, [Warn])
convertTracks midiTracks = (concatMap convert tracks, skeleton, warns)
    where
    (tracks, warns) = mconcatMap convertTrack midiTracks
    skeleton = Skeleton.make $ noteTrackEdges $ map snd tracks
    convert (inst, NoteTrack notes pitches controls) =
        (ParseTitle.instrumentToTitle inst, notes)
        : ("*", pitches)
        : [(ParseTitle.controlToTitle (ScoreT.untyped control), track)
            | (control, track) <- Map.toAscList controls]

noteTrackEdges :: [NoteTrack] -> [Skeleton.Edge]
noteTrackEdges = concat . snd . List.mapAccumL edges 1
    -- The skeleton starts at 1, because it can't go on the 0th track.
    where
    edges n (NoteTrack _ _ controls) = (end, zip ns (drop 1 ns))
        where
        end = n + 2 + Map.size controls
        ns = [n .. end-1]

-- | Take flat MIDI msgs to a list of tracks where events don't overlap.
convertTrack :: (Text, [Midi]) -> ([(ScoreT.Instrument, NoteTrack)], [Warn])
convertTrack (title, msgs) = (map (ScoreT.Instrument title,) tracks, warns)
    where
    (tracks, stuckOn) = splitTrack msgs
    warns = if null stuckOn then []
        else [title <> ": omitted notes with no note-offs: " <> pretty stuckOn]

-- ** infer_keyswitches

-- TODO incomplete.  I need to modify convert_tracks to take [AttrMidi].

type AttrMidi = (RealTime, Either Attrs.Attributes Midi.Message)
type KeyswitchMap = Map (Set Midi.Key) Attrs.Attributes

-- | Collect sounding keys, and each time look in the AttributeMap for the
-- current set.  If I find a match, emit the attributes as the current state.
-- If the pitch is any keyswitch, omit it from the output.
inferKeyswitches :: KeyswitchMap -> [Midi] -> [AttrMidi]
inferKeyswitches ksMap = go Set.empty
    where
    go _ [] = []
    go oldHeld ((t, msg) : msgs) = case noteKey msg of
        Just (True, key) -> map (t,) (setAttrs ++ emit key msg) ++ go held msgs
            where
            held = Set.insert key oldHeld
            setAttrs = maybe [] ((:[]) . Left) (Map.lookup held ksMap)
        Just (False, key) -> map (t,) (emit key msg) ++ go held msgs
            where held = Set.delete key oldHeld
        Nothing -> (t, Right msg) : go oldHeld msgs
    emit key msg = if Set.member key keyswitches then [] else [Right msg]
    keyswitches = Set.unions (Map.keys ksMap)

noteKey :: Midi.Message -> Maybe (Bool, Midi.Key)
noteKey (Midi.ChannelMessage _ msg) = case msg of
    Midi.NoteOn key _ -> Just (True, key)
    Midi.NoteOff key _ -> Just (False, key)
    _ -> Nothing
noteKey _ = Nothing

keyswitchMap :: Patch.AttributeMap -> KeyswitchMap
keyswitchMap (Common.AttributeMap amap) =
    Map.fromList $ filter (not . Set.null . fst)
        [(Set.fromList (mapMaybe keyOf ks), attrs) | (attrs, (ks, _)) <- amap]
    where
    keyOf (Patch.Keyswitch k) = Just k
    keyOf _ = Nothing

-- ** split_track

-- | Keep a cache of the end of the last event, to make it easier to find
-- a free track.
type SplitState = IntMap (RealTime, NoteTrack)

-- | For each note, assign to the lowest track which doesn't have an overlap.
splitTrack :: [Midi] -> ([NoteTrack], [(RealTime, Midi.Key)])
    -- ^ (tracks, notes stuck on)
splitTrack msgs = (extract $ foldl' collect IntMap.empty notes, stuckOn)
    where
    (notes, stuckOn) = collectNotes msgs
    extract = map snd . IntMap.elems
    collect state note@(start, _, _, _, _) =
        IntMap.insert tracknum (insertNote note track) state
        where (tracknum, track) = findNonOverlapping start state

findNonOverlapping :: RealTime -> SplitState -> (Int, (RealTime, NoteTrack))
findNonOverlapping start state =
    fromMaybe (0, (0, mempty)) $ List.find ((<=start) . fst . snd) tracks
    where
    tracks = [(n, fromMaybe (0, mempty) $ IntMap.lookup n state) | n <- [0..]]

insertNote :: MidiNote -> (RealTime, NoteTrack) -> (RealTime, NoteTrack)
insertNote note (last, noteTrack) =
    (max last (noteLast note), noteToTrack note <> noteTrack)

noteLast :: MidiNote -> RealTime
noteLast (_, end, _, _, controls) = maximum $ end : map fst controls

noteToTrack :: MidiNote -> NoteTrack
noteToTrack (start, end, key, vel, controls) =
    NoteTrack (Map.singleton (score start) (score (end - start), ""))
        (Map.singleton (score start) (0, keyToPitch key))
        (dyn <> convertControls controls)
    where
    dyn = Map.singleton Controls.dynamic
        (Map.singleton (score start) (0, showVal vel))
    score = RealTime.toScore

convertControls :: [MidiControl] -> Map ScoreT.Control Track
convertControls cs =
    Map.fromList [(ccToControl cc, convert msgs)
        | (cc, msgs) <- Lists.keyedGroupSort (fst . snd) cs]
    where
    convert midiControls =
        Map.fromList [(RealTime.toScore start, (0, showVal val))
            | (start, (_, val)) <- midiControls]

keyToPitch :: Midi.Key -> Text
keyToPitch = maybe "?" Pitch.noteText . Twelve.showNn . Midi.fromKey

-- | This is the same as 'Cmd.InputNote.cc_to_control', but I don't want the
-- dependency.
ccToControl :: Midi.Control -> ScoreT.Control
ccToControl cc =
    fromMaybe (ScoreT.Control ("cc" <> showt cc)) (Map.lookup cc ccControl)
    where
    ccControl = Maps.invert Control.universalControlMap

showVal :: Word.Word8 -> Text -- the Midi types are aliases for Word8
showVal val = ShowVal.showHexVal $ d / 0x7f
    where
    d :: Double
    d = fromIntegral val

-- ** collect_notes

-- | (start, end, key, vel, controls)
type MidiNote = (RealTime, RealTime, Midi.Key, Midi.Velocity, [MidiControl])
type MidiControl = (RealTime, (Midi.Control, Midi.ControlValue))

-- for each (note-on -- note-off) collect the controls in its scope
collectNotes :: [Midi] -> ([MidiNote], [(RealTime, Midi.Key)])
collectNotes msgs = (Maybe.catMaybes notes, map (second fst) stuckOn)
    where
    ((_, stuckOn), notes) = List.mapAccumL go ([], []) msgs
    go (controls, noteOns) (time, Midi.ChannelMessage _ msg) = case msg of
        Midi.ControlChange cc val ->
            (((time, (cc, val)) : controls, noteOns), Nothing)
        Midi.NoteOn key vel ->
            ((controls, (time, (key, vel)) : noteOns), Nothing)
        Midi.NoteOff key _ ->
            case findRemove ((==key) . fst . snd) noteOns of
                Nothing -> ((controls, noteOns), Nothing)
                Just ((start, (_, vel)), noteOns) -> ((controls, noteOns),
                    Just (collectNote start time key vel controls))
        _ -> ((controls, noteOns), Nothing)
    go state _ = (state, Nothing)
    collectNote start end key vel controls =
        (start, end, key, vel, reverse (takeWhile ((>=start) . fst) controls))

-- | Like 'List.find', but also return the list with the found element
-- removed.
findRemove :: (a -> Bool) -> [a] -> Maybe (a, [a])
findRemove f xs = case go xs of
        (Nothing, _) -> Nothing
        (Just x, xs) -> Just (x, xs)
    where
    go [] = (Nothing, [])
    go (x:xs)
        | f x = (Just x, xs)
        | otherwise = let (found, rest) = go xs in (found, x : rest)
