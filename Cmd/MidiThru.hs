{- | Implement midi thru by mapping InputNotes to MIDI messages.

    This is a very complicated thru and might be too slow.  It has to deal
    with:

    - Remap input pitch according to scale and control pitch bend range
    (done by NoteEntry) and instrument pb range.  This means keeping track of
    previous note id and pb val.

    - Remap addr based on addrs assign to instrument, assigning round-robin.
    This means keeping track of note ids assigned to addrs and serial numbers
    for each addr.

    It's different from the usual simple thru in that it attempts to assign
    control messages to a single note.  So if the instrument is multiplexed,
    control changes (including pitch bend) will go only to the last sounding
    key.  This also means that controls will not go through at all unless
    there is a note sounding.

    It should be possible to reduce latency by bypassing the responder loop and
    running this in its own thread.  It does mean the InputNote work is
    duplicated and synchronization of state, such as current instrument info,
    gets more complicated because it has to go through an mvar or something.

    I should find out what makes the responder so slow.  Profile it!

    - The sync afterwards: Some mechanism to find out if no Ui.State changes
    have happened and skip sync.

    - Marshalling the cmd list: cache the expensive parts.  The only changing
    bit is the focus cmds, so keep those until focus changes.

    - Duplicate NoteInput conversions.

    - Instrument is looked up on every msg just for pb_range, so cache that.
    Effectively, the short-circuit thread is another way to cache this.
-}
module Cmd.MidiThru where
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import Cmd.InputNote (NoteId)
import qualified Cmd.Msg as Msg

import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import Perform.Midi.Instrument (Addr)
import qualified Perform.Pitch as Pitch


-- | Send midi thru, addressing it to the given Instrument.
cmd_midi_thru :: Cmd.Cmd
cmd_midi_thru msg = do
    input <- case msg of
        Msg.InputNote input -> return input
        _ -> Cmd.abort
    score_inst <- Cmd.require =<< EditUtil.lookup_instrument
    scale_id <- EditUtil.get_scale_id
    inst <- Cmd.get_midi_instrument Score.no_attrs score_inst
    -- If I do breath here I get it even with MIDI input.  But the MIDI input
    -- already has a breath control, I only want it for NoteEntry.
    let pb_range = Instrument.inst_pitch_bend_range inst

    scale <- Cmd.get_scale "cmd_midi_thru" scale_id
    input <- Cmd.require_msg
        (Pretty.pretty scale_id ++ " doesn't have " ++ show input)
        (map_scale scale input)

    -- TODO if the wdev is in a certain scale, then I'll have to map the
    -- pitch here
    addrs <- Map.get [] score_inst <$> State.get_midi_alloc
    wdev_state <- Cmd.get_wdev_state
    let (thru_msgs, maybe_wdev_state) =
            input_to_midi pb_range wdev_state addrs input
    case maybe_wdev_state of
        Just wdev_state -> Cmd.modify_wdev_state (const wdev_state)
        Nothing -> return ()
    mapM_ (uncurry Cmd.midi) thru_msgs
    return Cmd.Continue

map_scale :: Scale.Scale -> InputNote.Input -> Maybe InputNote.Input
map_scale scale input = case input of
        InputNote.NoteOn note_id key vel ->
            fmap (\k -> InputNote.NoteOn note_id k vel) (convert key)
        InputNote.PitchChange note_id key ->
            fmap (\k -> InputNote.PitchChange note_id k) (convert key)
        _ -> Just input
    where
    convert input_key = fmap (\(Pitch.NoteNumber nn) -> Pitch.InputKey nn)
        (Scale.scale_input_to_nn scale input_key)

input_to_midi :: Control.PbRange -> Cmd.WriteDeviceState
    -> [Addr] -> InputNote.Input
    -> ([(Midi.WriteDevice, Midi.Message)], Maybe Cmd.WriteDeviceState)
input_to_midi pb_range wdev_state addrs input = case alloc addrs input of
    (Nothing, _) -> ([], Nothing)
    (Just addr, new_state) ->
        let last_pb = Map.get 0 addr (Cmd.wdev_pb wdev_state)
            (msgs, note_key) = InputNote.to_midi pb_range last_pb
                (Cmd.wdev_note_key wdev_state) input
            state = merge_state new_state addr (pb_of last_pb msgs)
                (wdev_state { Cmd.wdev_note_key = note_key })
        in (map (with_addr addr) msgs, Just state)
    where
    alloc = alloc_addr (Cmd.wdev_note_addr wdev_state)
        (Cmd.wdev_addr_serial wdev_state) (Cmd.wdev_serial wdev_state)

merge_state :: Maybe (Map.Map NoteId Addr, Map.Map Addr Integer)
    -> Addr -> Midi.PitchBendValue -> Cmd.WriteDeviceState
    -> Cmd.WriteDeviceState
merge_state new_state addr pb old = case new_state of
    Nothing -> old { Cmd.wdev_pb = new_pb }
    Just (note_addr, addr_serial) -> old
        { Cmd.wdev_pb = new_pb
        , Cmd.wdev_note_addr = note_addr
        , Cmd.wdev_addr_serial = addr_serial
        , Cmd.wdev_serial = Cmd.wdev_serial old + 1
        }
    where new_pb = Map.insert addr pb (Cmd.wdev_pb old)

-- | If the note_id is already playing in an addr, return that one.  Otherwise,
-- if it's not NoteOn or NoteOff, abort.  If it is, pick a free addr, and if
-- there is no free one, pick the oldest one.  Update the wdev state and assign
-- the note id to the addr.
alloc_addr :: Map.Map NoteId Addr -> Map.Map Addr Integer -> Integer
    -> [Addr] -> InputNote.Input
    -> (Maybe Addr, Maybe (Map.Map NoteId Addr, Map.Map Addr Integer))
alloc_addr note_addr addr_serial serial addrs input
    | Just addr <- Map.lookup note_id note_addr = case input of
        InputNote.NoteOff _ _ -> (Just addr, unassign addr)
        _ -> (Just addr, Nothing)
    | not (new_note input) = (Nothing, Nothing)
    | addr:_ <- free = (Just addr, assign addr)
    | Just addr <- old_addr = (Just addr, assign addr)
    | otherwise = (Nothing, Nothing) -- addrs must be null
    where
    note_id = InputNote.input_id input
    new_note (InputNote.NoteOn _ _ _) = True
    new_note (InputNote.NoteOff _ _) = True
    new_note _ = False
    assign addr = Just
        (Map.insert note_id addr note_addr, Map.insert addr serial addr_serial)
    unassign addr = Just
        (Map.delete note_id note_addr, Map.insert addr serial addr_serial)
    (allocated, free) = List.partition (`elem` (Map.elems note_addr)) addrs
    old_addr = Seq.minimum_on (flip Map.lookup addr_serial) allocated

pb_of :: Midi.PitchBendValue -> [Midi.ChannelMessage] -> Midi.PitchBendValue
pb_of = List.foldl' f
    where
    f _ (Midi.PitchBend pb) = pb
    f pb _ = pb

with_addr :: Addr -> Midi.ChannelMessage -> (Midi.WriteDevice, Midi.Message)
with_addr (wdev, chan) msg = (wdev, Midi.ChannelMessage chan msg)


-- * util

-- | Send ChannelMessages to the addrs (or just the lowest addr) of the current
-- instrument.  This bypasses all of the WriteDeviceState stuff so it won't
-- cooperate with addr allocation, but hopefully this won't cause problems for
-- simple uses like keymapped instruments.
channel_messages :: (Cmd.M m) => Bool -> [Midi.ChannelMessage] -> m ()
channel_messages first_addr msgs = do
    addrs <- get_addrs
    let addrs2 = if first_addr then take 1 addrs else addrs
    sequence_ [Cmd.midi wdev (Midi.ChannelMessage chan msg)
        | (wdev, chan) <- addrs2, msg <- msgs]

get_addrs :: (Cmd.M m) => m [Addr]
get_addrs = do
    inst <- Cmd.require =<< EditUtil.lookup_instrument
    Map.get [] inst <$> State.get_midi_alloc
