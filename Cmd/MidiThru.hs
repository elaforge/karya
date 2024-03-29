-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
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
module Cmd.MidiThru (
    cmd_midi_thru, for_instrument
    , convert_input
    -- * util
    , channel_messages
#ifdef TESTING
    , module Cmd.MidiThru
#endif
) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Vivid.OSC as OSC

import qualified Util.Log as Log
import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import           Cmd.InputNote (NoteId)
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Attrs as Attrs
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Scale as Scale
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import           Perform.Midi.Patch (Addr)
import qualified Perform.Pitch as Pitch
import qualified Perform.Sc.Patch as Sc.Patch
import qualified Perform.Sc.Play as Sc.Play

import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


-- | Send midi thru, addressing it to the given Instrument.
--
-- Actually, this handles 'Cmd.ImThru' as well, since it relies on the
-- instrument itself providing the thru function in 'Cmd.inst_thru'.
cmd_midi_thru :: Msg.Msg -> Cmd.CmdId Cmd.Status
cmd_midi_thru msg = do
    input <- case msg of
        Msg.InputNote input -> return input
        _ -> Cmd.abort
    score_inst <- Cmd.abort_unless =<< EditUtil.lookup_instrument
    attrs <- Cmd.get_instrument_attributes score_inst
    scale <- Perf.get_scale =<< Selection.track
    mapM_ Cmd.write_thru =<< for_instrument score_inst scale attrs input
    return Cmd.Continue

for_instrument :: ScoreT.Instrument -> Cmd.ThruFunction
for_instrument score_inst scale attrs input = do
    resolved <- Cmd.get_instrument score_inst
    let code_of = Common.common_code . Inst.inst_common . Cmd.inst_instrument
    let flags = Common.common_flags $ Cmd.inst_common resolved
    case Cmd.inst_thru (code_of resolved) of
        Nothing
            | Just (patch, config) <- Cmd.midi_patch resolved ->
                midi_thru patch config score_inst scale attrs input
            | Just patch <- Cmd.sc_patch resolved ->
                osc_thru patch flags score_inst scale attrs input
            | otherwise -> Cmd.abort
        Just thru -> thru scale attrs input

-- | This doesn't really fit with the name of the module, but OSC thru for
-- supercollider is so much simpler than MIDI I can just throw it in here.
osc_thru :: Sc.Patch.Patch -> Set Common.Flag -> ScoreT.Instrument
    -> Cmd.ThruFunction
osc_thru patch flags score_inst scale _attrs input = do
    -- attrs is used for keyswitches in MIDI, but sc doesn't support attrs yet.
    -- If I do, it'll probably be via string-valued controls.
    input <- convert_input score_inst scale input
    (:[]) . Cmd.OscThru <$> input_to_osc patch flags input

input_to_osc :: Cmd.M m => Sc.Patch.Patch -> Set Common.Flag
    -> InputNote.InputNn -> m [OSC.OSC]
input_to_osc patch flags = return . \case
    InputNote.NoteOn note_id nn vel ->
        Sc.Play.note_on patch triggered (unid note_id) nn vel
    InputNote.NoteOff note_id _ ->
        Sc.Play.note_off triggered (unid note_id)
    InputNote.Control note_id control val ->
        Sc.Play.set_control patch (unid note_id) control val
    InputNote.PitchChange note_id nn ->
        Sc.Play.pitch_change patch (unid note_id) nn
    where
    triggered = Common.Triggered `Set.member` flags
    unid (InputNote.NoteId id) = id

-- | I used to keep track of the previous PitchBend to avoid sending extra ones.
-- But it turns out I don't actually know the state of the MIDI channel, so
-- now I always send PitchBend.  I'm not sure why I ever thought it could work.
-- I could still do this by tracking channel state at the Midi.Interface level.
-- I actually already do that a bit of tracking with note_tracker, but it's
-- simpler to just always send PitchBend, unless it becomes a problem.
midi_thru :: Patch.Patch -> Patch.Config -> ScoreT.Instrument
    -> Cmd.ThruFunction
midi_thru patch config score_inst scale attrs input = do
    input <- convert_input score_inst scale input
    let addrs = Patch.config_addrs config
    if null addrs then return [] else do
        (input_nn, ks) <- Cmd.require
            (pretty (Scale.scale_id scale) <> " doesn't have " <> pretty input)
            =<< input_to_nn score_inst (Patch.patch_attribute_map patch)
                (Patch.settings#Patch.scale #$ config) attrs input
        wdev_state <- Cmd.get_wdev_state
        pb_range <- Cmd.require "no pb range" $
            Patch.settings#Patch.pitch_bend_range #$ config
        maybe (return []) (to_msgs ks) $
            input_to_midi pb_range wdev_state addrs input_nn
    where
    to_msgs ks (thru_msgs, wdev_state) = do
        Cmd.modify_wdev_state (const wdev_state)
        let ks_msgs = concatMap (keyswitch_to_midi thru_msgs) ks
        return $ map (uncurry Cmd.midi_thru) $ ks_msgs ++ thru_msgs

-- | The keyswitch winds up being simultaneous with the note on.  Especially
-- stupid VSTs like kontakt will sometimes miss a keyswitch if it doesn't have
-- enough lead time.  There's not much I can do about that, but to avoid making
-- the keyswitch too short I hold it down along with the note.
keyswitch_to_midi :: [(Midi.WriteDevice, Midi.Message)] -> Patch.Keyswitch
    -> [(Midi.WriteDevice, Midi.Message)]
keyswitch_to_midi msgs ks = case msum (map note_msg msgs) of
    Nothing -> []
    Just (addr, key, is_note_on) -> map (with_addr addr) $
        if is_note_on then [Patch.keyswitch_on key ks]
            else maybe [] (:[]) (Patch.keyswitch_off ks)
    where
    note_msg (dev, Midi.ChannelMessage chan msg) = case msg of
        Midi.NoteOn key _ -> Just ((dev, chan), key, True)
        Midi.NoteOff key _ -> Just ((dev, chan), key, False)
        _ -> Nothing
    note_msg _ = Nothing

-- | Realize the Input as a pitch in the given scale.
input_to_nn :: Cmd.M m => ScoreT.Instrument -> Patch.AttributeMap
    -> Maybe Patch.Scale -> Attrs.Attributes -> InputNote.InputNn
    -> m (Maybe (InputNote.InputNn, [Patch.Keyswitch]))
input_to_nn inst attr_map patch_scale attrs = \case
    InputNote.NoteOn note_id nn vel -> justm (convert nn) $ \(nn, ks) ->
        return $ Just (InputNote.NoteOn note_id nn vel, ks)
    InputNote.PitchChange note_id input -> justm (convert input) $ \(nn, _) ->
        return $ Just (InputNote.PitchChange note_id nn, [])
    input@(InputNote.NoteOff {}) -> return $ Just (input, ks)
        where
        ks = maybe [] (fst . snd) $ Common.lookup_attributes attrs attr_map
    input@(InputNote.Control {}) -> return $ Just (input, [])
    where
    convert nn = do
        let (result, not_found) = convert_pitch attr_map patch_scale attrs nn
        when not_found $
            Log.warn $ "inst " <> pretty inst <> " doesn't have attrs "
                <> pretty attrs <> ", understood attrs are: "
                <> pretty (Common.mapped_attributes attr_map)
        return result

-- | Convert a keyboard input into the NoteNumber desired by the scale.
convert_input :: Cmd.M m => ScoreT.Instrument -> Scale.Scale -> InputNote.Input
    -> m InputNote.InputNn
convert_input inst scale = \case
    InputNote.NoteOn note_id input vel -> do
        nn <- convert input
        return $ InputNote.NoteOn note_id nn vel
    InputNote.PitchChange note_id input ->
        InputNote.PitchChange note_id <$> convert input
    InputNote.NoteOff note_id vel -> return $ InputNote.NoteOff note_id vel
    InputNote.Control note_id control val ->
        return $ InputNote.Control note_id control val
    where
    convert = convert_input_pitch inst scale

convert_input_pitch :: Cmd.M m => ScoreT.Instrument -> Scale.Scale
    -> Pitch.Input -> m Pitch.NoteNumber
convert_input_pitch inst scale input = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    -- I ignore _logs, any interesting errors should be in 'result'.
    (result, _logs) <- Perf.derive_at_exc block_id track_id $
        Derive.with_instrument inst $
        filter_transposers scale $
        Scale.scale_input_to_nn scale pos input
    case result of
        Left (Derive.Error _ _ err) -> throw $ pretty err
        -- This just means the key isn't in the scale, it happens a lot so
        -- no need to shout about it.
        Right (Left DeriveT.InvalidInput) -> Cmd.abort
        Right (Left err) -> throw $ pretty err
        Right (Right nn) -> return nn
    where
    throw = Cmd.throw . (("scale_input_to_nn for " <> pretty input <> ": ") <>)

-- | Remove transposers because otherwise the thru pitch doesn't match the
-- entered pitch and it's very confusing.  However, I retain 'Controls.octave'
-- and 'Controls.hz' because those are used to configure a scale, e.g. via
-- 'Patch.config_controls', and the pitch is nominally the same.
filter_transposers :: Scale.Scale -> Derive.Deriver a
    -> Derive.Deriver a
filter_transposers scale = Derive.with_controls transposers
    where
    transposers = zip
        (filter (`notElem` [Controls.octave, Controls.hz])
            (Set.toList (Scale.scale_transposers scale)))
        (repeat (ScoreT.untyped mempty))

-- | This is a midi thru version of 'Perform.Midi.Convert.convert_midi_pitch'.
-- It's different because it works with a scalar NoteNumber instead of
-- a Score.Event with a pitch signal, which makes it hard to share code.
convert_pitch :: Patch.AttributeMap -> Maybe Patch.Scale -> Attrs.Attributes
    -> Pitch.NoteNumber -> (Maybe (Pitch.NoteNumber, [Patch.Keyswitch]), Bool)
    -- ^ The Bool is True if the attrs were non-empty but not found.
convert_pitch attr_map patch_scale attrs nn =
    case Common.lookup_attributes attrs attr_map of
        Nothing -> ((, []) <$> maybe_pitch, attrs /= mempty)
        Just (_, (keyswitches, maybe_keymap)) ->
            ( (, keyswitches) <$> maybe maybe_pitch set_keymap maybe_keymap
            , False
            )
    where
    maybe_pitch = apply_patch_scale nn
    apply_patch_scale = maybe Just Patch.convert_scale patch_scale
    set_keymap (Patch.UnpitchedKeymap key) = Just $ Midi.from_key key
    set_keymap (Patch.PitchedKeymap low _ low_pitch) =
        (+ Midi.from_key (low - low_pitch)) <$> maybe_pitch

input_to_midi :: Control.PbRange -> Cmd.WriteDeviceState
    -> [Addr] -> InputNote.InputNn
    -> Maybe ([(Midi.WriteDevice, Midi.Message)], Cmd.WriteDeviceState)
input_to_midi pb_range wdev_state addrs input_nn = case alloc addrs input_nn of
    (Nothing, _) -> Nothing
    (Just addr, new_state) -> Just (map (with_addr addr) msgs, state)
        where
        (msgs, note_key) = InputNote.to_midi pb_range
            (Cmd.wdev_note_key wdev_state) input_nn
        state = merge_state new_state
            (wdev_state { Cmd.wdev_note_key = note_key })
    where
    alloc = alloc_addr (Cmd.wdev_note_addr wdev_state)
        (Cmd.wdev_addr_serial wdev_state) (Cmd.wdev_serial wdev_state)

merge_state :: Maybe (Map NoteId Addr, Map Addr Cmd.Serial)
    -> Cmd.WriteDeviceState -> Cmd.WriteDeviceState
merge_state new_state old = case new_state of
    Nothing -> old
    Just (note_addr, addr_serial) -> old
        { Cmd.wdev_note_addr = note_addr
        , Cmd.wdev_addr_serial = addr_serial
        , Cmd.wdev_serial = Cmd.wdev_serial old + 1
        }

-- | If the note_id is already playing in an addr, return that one.  Otherwise,
-- if it's not NoteOn or NoteOff, abort.  If it is, pick a free addr, and if
-- there is no free one, pick the oldest one.  Update the wdev state and assign
-- the note id to the addr.
alloc_addr :: Map NoteId Addr -> Map Addr Cmd.Serial -> Cmd.Serial
    -> [Addr] -- ^ Addrs allocated to this instrument.
    -> InputNote.InputNn
    -> (Maybe Addr, Maybe (Map NoteId Addr, Map Addr Cmd.Serial))
alloc_addr note_addr addr_serial serial addrs input
    | Just addr <- Map.lookup note_id note_addr, addr `elem` addrs =
        case input of
            InputNote.NoteOff _ _ -> (Just addr, unassign addr)
            _ -> (Just addr, Nothing)
    | not (new_note input) = (Nothing, Nothing)
    | Just addr <- oldest = (Just addr, assign addr)
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
    -- Always pick the channel with the oldest note, whether or not it's
    -- allocated.  Previously I would try to pick a free one, but reusing
    -- a free channel led to audible artifacts with long-ringing instruments.
    oldest = Lists.minimumOn (flip Map.lookup addr_serial) addrs

with_addr :: Addr -> Midi.ChannelMessage -> (Midi.WriteDevice, Midi.Message)
with_addr (wdev, chan) msg = (wdev, Midi.ChannelMessage chan msg)


-- * util

-- | Send ChannelMessages to the addrs (or just the lowest addr) of the current
-- instrument.  This bypasses all of the WriteDeviceState stuff so it won't
-- cooperate with addr allocation, but hopefully this won't cause problems for
-- simple uses like keymapped instruments.
channel_messages :: Cmd.M m => Maybe ScoreT.Instrument -- ^ use this inst, or
    -- the one on the selected track if Nothing.
    -> Bool -> [Midi.ChannelMessage] -> m ()
channel_messages maybe_inst first_addr msgs = do
    addrs <- get_addrs maybe_inst
    let addrs2 = if first_addr then take 1 addrs else addrs
    sequence_
        [ Cmd.midi wdev (Midi.ChannelMessage chan msg)
        | (wdev, chan) <- addrs2, msg <- msgs
        ]

get_addrs :: Cmd.M m => Maybe ScoreT.Instrument -> m [Addr]
get_addrs maybe_inst = do
    inst <- maybe (Cmd.abort_unless =<< EditUtil.lookup_instrument)
        return maybe_inst
    alloc <- Ui.allocation inst <#> Ui.get
    return $ case UiConfig.alloc_backend <$> alloc of
        Just (UiConfig.Midi config) -> Patch.config_addrs config
        _ -> []
