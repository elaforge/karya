-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert a midi file to a block.
module Cmd.Load.Midi (
    load
    , parse, convert
    -- * testing
    , Midi, NoteTrack(..)
    , convert_tracks, split_track, collect_notes
) where
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Word as Word

import qualified ZMidi.Core as Z

import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Derive.Attrs as Attrs
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Instrument.Common as Common
import Global
import Types


type Warn = Text

load :: FilePath -> Cmd.CmdT IO BlockId
load fn = liftIO (parse fn) >>= \x -> case x of
    Left err -> Cmd.throw $ "parsing " <> showt fn <> ": " <> err
    Right midi_file -> do
        let (tracks, skel, warns) = convert midi_file
        mapM_ (Log.warn . ((txt fn <> ": ") <>)) warns
        create tracks skel

create :: Ui.M m => [(Text, Track)] -> Skeleton.Skeleton -> m BlockId
create tracks skel = do
    block_id <- Create.block Ui.no_ruler
    mapM_ (add_track block_id) tracks
    Ui.set_skeleton block_id skel
    return block_id
    where
    add_track block_id (title, track) =
        Create.track block_id 9999 title $
            Events.from_list [Event.event start dur text
                | (start, (dur, text)) <- Map.toAscList track]

parse :: FilePath -> IO (Either Warn Z.MidiFile)
parse fn = first show_error <$> Z.readMidi fn
    where show_error (Z.ParseErr pos msg) = showt pos <> ": " <> txt msg

convert :: Z.MidiFile -> ([(Text, Track)], Skeleton.Skeleton, [Warn])
convert = convert_tracks . extract_tracks

-- * extract

type Midi = (RealTime, Midi.Message)

extract_tracks :: Z.MidiFile -> [(Text, [Midi])]
extract_tracks (Z.MidiFile header tracks) =
    filter (not . null . snd) $ map (extract_track per_sec) tracks
    where
    per_sec = RealTime.seconds . fromIntegral $ case Z.time_division header of
        Z.FPS val -> val
        Z.TPB val -> val

extract_track :: RealTime -> Z.MidiTrack -> (Text, [Midi])
extract_track per_sec (Z.MidiTrack msgs) =
    (name, concatMap extract_message (zip times (map snd msgs)))
    where
    name = maybe "" txt $ Seq.head
        [name | Z.MetaEvent (Z.TextEvent _ name) <- take 10 (map snd msgs)]
    times = drop 1 $ scanl (+) 0 $
        map ((/per_sec) . RealTime.seconds . fromIntegral . fst) msgs

extract_message :: (RealTime, Z.MidiEvent) -> [Midi]
extract_message (time, msg) = case msg of
    Z.VoiceEvent _ midi -> (:[]) . (,) time $ case midi of
        Z.NoteOff chan key vel ->
            Midi.ChannelMessage chan (Midi.NoteOff (Midi.to_key key) vel)
        Z.NoteOn chan key vel ->
            Midi.ChannelMessage chan (Midi.NoteOn (Midi.to_key key) vel)
        Z.NoteAftertouch chan key val ->
            Midi.ChannelMessage chan (Midi.Aftertouch (Midi.to_key key) val)
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
data NoteTrack = NoteTrack Track Track (Map Score.Control Track)
    deriving (Show)
-- | Map start (dur, text)
type Track = Map ScoreTime (ScoreTime, Text)

instance Monoid NoteTrack where
    mempty = NoteTrack mempty mempty mempty
    mappend (NoteTrack notes1 pitches1 controls1)
            (NoteTrack notes2 pitches2 controls2) =
        NoteTrack (notes1 <> notes2) (pitches1 <> pitches2)
            (Map.mappend controls1 controls2)

-- | Take flat MIDI msgs to a list of tracks where events don't overlap, and
-- add pitch and control tracks.
convert_tracks :: [(Text, [Midi])]
    -> ([(Text, Track)], Skeleton.Skeleton, [Warn])
convert_tracks midi_tracks = (concatMap convert tracks, skeleton, warns)
    where
    (tracks, warns) = mconcatMap convert_track midi_tracks
    skeleton = Skeleton.make $ note_track_edges $ map snd tracks
    convert (inst, NoteTrack notes pitches controls) =
        (ParseTitle.instrument_to_title inst, notes)
        : ("*", pitches)
        : [(ParseTitle.control_to_title control, track)
            | (control, track) <- Map.toAscList controls]

note_track_edges :: [NoteTrack] -> [Skeleton.Edge]
note_track_edges = concat . snd . List.mapAccumL edges 1
    -- The skeleton starts at 1, because it can't go on the 0th track.
    where
    edges n (NoteTrack _ _ controls) = (end, zip ns (drop 1 ns))
        where
        end = n + 2 + Map.size controls
        ns = [n .. end-1]

-- | Take flat MIDI msgs to a list of tracks where events don't overlap.
convert_track :: (Text, [Midi]) -> ([(Score.Instrument, NoteTrack)], [Warn])
convert_track (title, msgs) = (map (Score.instrument title,) tracks, warns)
    where
    (tracks, stuck_on) = split_track msgs
    warns = if null stuck_on then []
        else [title <> ": omitted notes with no note-offs: " <> pretty stuck_on]

-- ** infer_keyswitches

-- TODO incomplete.  I need to modify convert_tracks to take [AttrMidi].

type AttrMidi = (RealTime, Either Attrs.Attributes Midi.Message)
type KeyswitchMap = Map (Set Midi.Key) Attrs.Attributes

-- | Collect sounding keys, and each time look in the AttributeMap for the
-- current set.  If I find a match, emit the attributes as the current state.
-- If the pitch is any keyswitch, omit it from the output.
infer_keyswitches :: KeyswitchMap -> [Midi] -> [AttrMidi]
infer_keyswitches ks_map = go Set.empty
    where
    go _ [] = []
    go old_held ((t, msg) : msgs) = case note_key msg of
        Just (True, key) -> map (t,) (set_attrs ++ emit key msg) ++ go held msgs
            where
            held = Set.insert key old_held
            set_attrs = maybe [] ((:[]) . Left) (Map.lookup held ks_map)
        Just (False, key) -> map (t,) (emit key msg) ++ go held msgs
            where held = Set.delete key old_held
        Nothing -> (t, Right msg) : go old_held msgs
    emit key msg = if Set.member key keyswitches then [] else [Right msg]
    keyswitches = Set.unions (Map.keys ks_map)

note_key :: Midi.Message -> Maybe (Bool, Midi.Key)
note_key (Midi.ChannelMessage _ msg) = case msg of
    Midi.NoteOn key _ -> Just (True, key)
    Midi.NoteOff key _ -> Just (False, key)
    _ -> Nothing
note_key _ = Nothing

keyswitch_map :: Patch.AttributeMap -> KeyswitchMap
keyswitch_map (Common.AttributeMap amap) =
    Map.fromList $ filter (not . Set.null . fst)
        [(Set.fromList (mapMaybe key_of ks), attrs) | (attrs, (ks, _)) <- amap]
    where
    key_of (Patch.Keyswitch k) = Just k
    key_of _ = Nothing

-- ** split_track

-- | Keep a cache of the end of the last event, to make it easier to find
-- a free track.
type SplitState = IntMap.IntMap (RealTime, NoteTrack)

-- | For each note, assign to the lowest track which doesn't have an overlap.
split_track :: [Midi] -> ([NoteTrack], [(RealTime, Midi.Key)])
    -- ^ (tracks, notes stuck on)
split_track msgs = (extract $ foldl' collect IntMap.empty notes, stuck_on)
    where
    (notes, stuck_on) = collect_notes msgs
    extract = map snd . IntMap.elems
    collect state note@(start, _, _, _, _) =
        IntMap.insert tracknum (insert_note note track) state
        where (tracknum, track) = find_non_overlapping start state

find_non_overlapping :: RealTime -> SplitState -> (Int, (RealTime, NoteTrack))
find_non_overlapping start state =
    fromMaybe (0, (0, mempty)) $ List.find ((<=start) . fst . snd) tracks
    where
    tracks = [(n, fromMaybe (0, mempty) $ IntMap.lookup n state) | n <- [0..]]

insert_note :: MidiNote -> (RealTime, NoteTrack) -> (RealTime, NoteTrack)
insert_note note (last, note_track) =
    (max last (note_last note), note_to_track note <> note_track)

note_last :: MidiNote -> RealTime
note_last (_, end, _, _, controls) = maximum $ end : map fst controls

note_to_track :: MidiNote -> NoteTrack
note_to_track (start, end, key, vel, controls) =
    NoteTrack (Map.singleton (score start) (score (end - start), ""))
        (Map.singleton (score start) (0, key_to_pitch key))
        (dyn <> convert_controls controls)
    where
    dyn = Map.singleton Score.c_dynamic
        (Map.singleton (score start) (0, show_val vel))
    score = RealTime.to_score

convert_controls :: [MidiControl] -> Map Score.Control Track
convert_controls cs =
    Map.fromList [(cc_to_control cc, convert msgs)
        | (cc, msgs) <- Seq.keyed_group_sort (fst . snd) cs]
    where
    convert midi_controls =
        Map.fromList [(RealTime.to_score start, (0, show_val val))
            | (start, (_, val)) <- midi_controls]

key_to_pitch :: Midi.Key -> Text
key_to_pitch = maybe "?" Pitch.note_text . Twelve.show_nn . Midi.from_key

-- | This is the same as 'Cmd.InputNote.cc_to_control', but I don't want the
-- dependency.
cc_to_control :: Midi.Control -> Score.Control
cc_to_control cc =
    fromMaybe (Score.unchecked_control ("cc" <> showt cc))
        (Map.lookup cc cc_control)
    where
    cc_control = Map.invert Control.universal_control_map

show_val :: Word.Word8 -> Text -- the Midi types are aliases for Word8
show_val val = ShowVal.show_hex_val $ d / 0x7f
    where
    d :: Double
    d = fromIntegral val

-- ** collect_notes

-- | (start, end, key, vel, controls)
type MidiNote = (RealTime, RealTime, Midi.Key, Midi.Velocity, [MidiControl])
type MidiControl = (RealTime, (Midi.Control, Midi.ControlValue))

-- for each (note-on -- note-off) collect the controls in its scope
collect_notes :: [Midi] -> ([MidiNote], [(RealTime, Midi.Key)])
collect_notes msgs = (Maybe.catMaybes notes, map (second fst) stuck_on)
    where
    ((_, stuck_on), notes) = List.mapAccumL go ([], []) msgs
    go (controls, note_ons) (time, Midi.ChannelMessage _ msg) = case msg of
        Midi.ControlChange cc val ->
            (((time, (cc, val)) : controls, note_ons), Nothing)
        Midi.NoteOn key vel ->
            ((controls, (time, (key, vel)) : note_ons), Nothing)
        Midi.NoteOff key _ ->
            case find_remove ((==key) . fst . snd) note_ons of
                Nothing -> ((controls, note_ons), Nothing)
                Just ((start, (_, vel)), note_ons) -> ((controls, note_ons),
                    Just (collect_note start time key vel controls))
        _ -> ((controls, note_ons), Nothing)
    go state _ = (state, Nothing)
    collect_note start end key vel controls =
        (start, end, key, vel, reverse (takeWhile ((>=start) . fst) controls))

-- | Like 'List.find', but also return the list with the found element
-- removed.
find_remove :: (a -> Bool) -> [a] -> Maybe (a, [a])
find_remove f xs = case go xs of
        (Nothing, _) -> Nothing
        (Just x, xs) -> Just (x, xs)
    where
    go [] = (Nothing, [])
    go (x:xs)
        | f x = (Just x, xs)
        | otherwise = let (found, rest) = go xs in (found, x : rest)
