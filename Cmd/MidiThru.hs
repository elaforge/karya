-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Implement midi thru by mapping InputNotes to MIDI messages.

    This is effectively a recreation of the deriver and MIDI performer, but
    geared to producing a single note immediately rather than deriving and
    performing an entire score.  But since derivation and performance are both
    very complicated, it's doomed to be complicated and inaccurate.

    The rationale is that the performer is oriented around a stream of events
    when their durations are known, while this must derive a single key, and in
    real time.  However, it's also due to history (derivation used to be much
    simpler), and concerns about efficiency, so in the future I'll probably
    move towards reusing as much of the deriver and performer as possible.

    Note that actually much of the deriver is already reused, courtesy of
    'Perf.derive_at'.  Also, 'Scale.scale_input_to_nn' may have a shortcut
    implementation, but for complicated scales falls back on derivation.

    An implementation that fully reuses deriver and performer is in
    "Cmd.Instrument.CUtil".insert_expr.

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
import qualified Data.Set as Set

import qualified Util.Map as Map
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import Cmd.InputNote (NoteId)
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import Perform.Midi.Instrument (Addr)
import qualified Perform.Pitch as Pitch

import Global


-- | Send midi thru, addressing it to the given Instrument.
cmd_midi_thru :: Cmd.Cmd
cmd_midi_thru msg = do
    input <- case msg of
        Msg.InputNote input -> return input
        _ -> Cmd.abort
    score_inst <- Cmd.abort_unless =<< EditUtil.lookup_instrument
    midi_thru_instrument score_inst input
    return Cmd.Continue

midi_thru_instrument :: Cmd.M m => Score.Instrument -> InputNote.Input -> m ()
midi_thru_instrument score_inst input = do
    addrs <- Instrument.get_addrs score_inst <$> State.get_midi_config
    if null addrs then return () else do
    (b, t) <- Selection.track
    scale <- Perf.get_scale b t
    patch <- Cmd.get_midi_patch score_inst
    input <- Cmd.require
        (pretty (Scale.scale_id scale) <> " doesn't have " <> pretty input)
        =<< map_scale (Instrument.patch_scale patch) scale
            (Instrument.patch_environ patch) input

    wdev_state <- Cmd.get_wdev_state
    let (thru_msgs, maybe_wdev_state) =
            input_to_midi pb_range wdev_state addrs input
        pb_range = Instrument.inst_pitch_bend_range
            (Instrument.patch_instrument patch)
    whenJust maybe_wdev_state $ Cmd.modify_wdev_state . const
    mapM_ (uncurry Cmd.midi) thru_msgs

-- | Realize the Input as a pitch in the given scale.
map_scale :: Cmd.M m => Maybe Instrument.PatchScale -> Scale.Scale
    -> TrackLang.Environ -- ^ Evaluate the pitch in this environ.  This is
    -- important because some scales change pitch based on environ.
    -> InputNote.Input -> m (Maybe InputNote.InputNn)
map_scale patch_scale scale environ input = case input of
    InputNote.NoteOn note_id input vel -> do
        maybe_nn <- convert input
        return $ fmap (\k -> InputNote.NoteOn note_id k vel) maybe_nn
    InputNote.PitchChange note_id input -> do
        maybe_nn <- convert input
        return $ fmap (InputNote.PitchChange note_id) maybe_nn
    InputNote.NoteOff note_id vel ->
        return $ Just $ InputNote.NoteOff note_id vel
    InputNote.Control note_id control val ->
        return $ Just $ InputNote.Control note_id control val
    where
    convert input = do
        (block_id, _, track_id, pos) <- Selection.get_insert
        -- I ignore _logs, any interesting errors should be in 'result'.
        (result, _logs) <- Perf.derive_at block_id track_id $
            Derive.with_environ environ $
            -- Otherwise, the sounding pitch doesn't match the entered pitch.
            Derive.remove_controls transposers $
            Scale.scale_input_to_nn scale pos input
        case result of
            Left err -> throw $ "derive_at: " <> err
            -- This just means the key isn't in the scale, it happens a lot so
            -- no need to shout about it.
            Right (Left Scale.InvalidInput) -> Cmd.abort
            Right (Left err) -> throw $ pretty err
            Right (Right nn) -> return $ map_patch_scale patch_scale nn
        where throw = Cmd.throw .  ("error deriving input key's nn: " <>)
    transposers = Set.toList (Scale.scale_transposers scale)

map_patch_scale :: Maybe Instrument.PatchScale -> Pitch.NoteNumber
    -> Maybe Pitch.NoteNumber
map_patch_scale Nothing = Just
map_patch_scale (Just scale) = Instrument.convert_patch_scale scale

input_to_midi :: Control.PbRange -> Cmd.WriteDeviceState
    -> [Addr] -> InputNote.InputNn
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

merge_state :: Maybe (Map.Map NoteId Addr, Map.Map Addr Cmd.Serial)
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
alloc_addr :: Map.Map NoteId Addr -> Map.Map Addr Cmd.Serial -> Cmd.Serial
    -> [Addr] -> InputNote.InputNn
    -> (Maybe Addr, Maybe (Map.Map NoteId Addr, Map.Map Addr Cmd.Serial))
alloc_addr note_addr addr_serial serial addrs input
    | Just addr <- Map.lookup note_id note_addr, addr `elem` addrs =
        case input of
            InputNote.NoteOff _ _ -> (Just addr, unassign addr)
            _ -> (Just addr, Nothing)
    | not (new_note input) = (Nothing, Nothing)
    | addr : _ <- free = (Just addr, assign addr)
    | Just addr <- old_addr = (Just addr, assign addr)
    | otherwise = (Nothing, Nothing) -- addrs must be null
    where
    note_id = InputNote.input_id input
    new_note (InputNote.NoteOn {}) = True
    new_note (InputNote.NoteOff {}) = True
    new_note _ = False
    assign addr = Just
        (Map.insert note_id addr note_addr, Map.insert addr serial addr_serial)
    unassign addr = Just
        (Map.delete note_id note_addr, Map.insert addr serial addr_serial)
    (allocated, free) = List.partition (`elem` Map.elems note_addr) addrs
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
channel_messages :: Cmd.M m => Maybe Score.Instrument -> Bool
    -> [Midi.ChannelMessage] -> m ()
channel_messages maybe_inst first_addr msgs = do
    addrs <- get_addrs maybe_inst
    let addrs2 = if first_addr then take 1 addrs else addrs
    sequence_ [Cmd.midi wdev (Midi.ChannelMessage chan msg)
        | (wdev, chan) <- addrs2, msg <- msgs]

get_addrs :: Cmd.M m => Maybe Score.Instrument -> m [Addr]
get_addrs maybe_inst = do
    inst <- maybe (Cmd.abort_unless =<< EditUtil.lookup_instrument)
        return maybe_inst
    Instrument.get_addrs inst <$> State.get_midi_config
