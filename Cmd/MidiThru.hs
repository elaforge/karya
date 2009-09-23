{-# LANGUAGE PatternGuards #-}
{- | Implement midi thru by mapping InputNotes to MIDI messages.

    This is a very complicated thru and might be too slow.  It has to deal
    with:

    - Remap input pitch according to scale and controller pitch bend range
    (done by NoteEntry) and instrument pb range.  This means keeping track of
    previous note id and pb val.

    - Remap addr based on addrs assign to instrument, assigning round-robin.
    This means keeping track of note ids assigned to addrs and serial numbers
    for each addr.

    It should be possible to reduce latency by bypassing the responder loop and
    running this in its own thread.  It does mean the InputNote work is
    duplicated and synchronization of state, such as current instrument info,
    gets more complicated because it has to go through an mvar or something.

    I should find out what makes the responder so slow.  Profile it!

    - The sync afterwards: Some mechanism to find out if no Ui.State changes
    have happened and skip sync.

    - Marshalling the cmd list: cache the expensive parts.  The only changing
    bit is the focus cmds, so keep those until focus changes.  Or if it's the
    skeleton parse, that might be fixed by moving to the explicit skeleton.

    - Duplicate NoteInput conversions.

    - Instrument is looked up on every msg just for pb_range, so cache that.
    Effectively, the short-circuit thread is another way to cache this.
-}
module Cmd.MidiThru where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Util.Map as Map
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import Cmd.InputNote (NoteId)
import qualified Cmd.Msg as Msg

import qualified Derive.Score as Score

import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument
import Perform.Midi.Instrument (Addr)


-- | Send midi thru, addressing it to the given Instrument.
cmd_midi_thru :: Score.Instrument -> Cmd.Cmd
cmd_midi_thru score_inst msg = do
    input <- case msg of
        Msg.InputNote input -> return input
        _ -> Cmd.abort
    lookup_inst <- Cmd.get_lookup_midi_instrument
    -- I could try to get attrs from the inst track title, but I'm not sure
    -- how useful that will be.
    inst <- Cmd.require $ lookup_inst Score.no_attrs score_inst
    let pb_range = Instrument.inst_pitch_bend_range inst

    -- TODO if the wdev is in a certain scale, then I'll have to map the
    -- pitch here
    config <- State.get_midi_config
    let addrs = Map.get [] score_inst (Instrument.config_alloc config)
    wdev_state <- Cmd.get_wdev_state
    let (thru_msgs, maybe_wdev_state) =
            input_to_midi pb_range wdev_state addrs input
    case maybe_wdev_state of
        Just wdev_state -> Cmd.set_wdev_state wdev_state
        Nothing -> return ()
    mapM_ (uncurry Cmd.midi) thru_msgs
    return Cmd.Continue

input_to_midi :: Controller.PbRange -> Cmd.WriteDeviceState
    -> [Addr] -> InputNote.Input
    -> ([(Midi.WriteDevice, Midi.Message)], Maybe Cmd.WriteDeviceState)
input_to_midi pb_range wdev_state addrs input = case alloc addrs input of
    (Nothing, _) -> ([], Nothing)
    (Just addr, new_state) ->
        let pb = Map.get 0 addr (Cmd.wdev_pb wdev_state)
            msgs = InputNote.to_midi pb_range pb input
        in (map (with_addr addr) msgs,
            Just (merge_state new_state addr (last_pb pb msgs) wdev_state))
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
        , Cmd.wdev_serial = Cmd.wdev_serial old + 1 }
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
    old_addr = if null allocated then Nothing
        else Just $ Seq.minimum_with (flip Map.lookup addr_serial)
            (error "unreached") allocated

last_pb :: Midi.PitchBendValue -> [Midi.ChannelMessage] -> Midi.PitchBendValue
last_pb = List.foldl' f
    where
    f _ (Midi.PitchBend pb) = pb
    f pb _ = pb

with_addr :: Addr -> Midi.ChannelMessage -> (Midi.WriteDevice, Midi.Message)
with_addr (wdev, chan) msg = (wdev, Midi.ChannelMessage chan msg)
