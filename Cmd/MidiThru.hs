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
    cmdMidiThru, forInstrument
    , convertInput
    -- * util
    , channelMessages
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
cmdMidiThru :: Msg.Msg -> Cmd.CmdId Cmd.Status
cmdMidiThru msg = do
    input <- case msg of
        Msg.InputNote input -> pure input
        _ -> Cmd.abort
    scoreInst <- Cmd.abortUnless =<< EditUtil.lookupInstrument
    attrs <- Cmd.getInstrumentAttributes scoreInst
    scale <- Perf.getScale =<< Selection.track
    mapM_ Cmd.writeThru =<< forInstrument scoreInst scale attrs input
    pure Cmd.Continue

forInstrument :: ScoreT.Instrument -> Cmd.ThruFunction
forInstrument scoreInst scale attrs input = do
    resolved <- Cmd.getInstrument scoreInst
    let codeOf = Common.commonCode . Inst.instCommon . Cmd.instInstrument
    let flags = Common.commonFlags $ Cmd.instCommon resolved
    case Cmd.instThru (codeOf resolved) of
        Nothing
            | Just (patch, config) <- Cmd.midiPatch resolved ->
                midiThru patch config scoreInst scale attrs input
            | Just patch <- Cmd.scPatch resolved ->
                oscThru patch flags scoreInst scale attrs input
            | otherwise -> Cmd.abort
        Just thru -> thru scale attrs input

-- | This doesn't really fit with the name of the module, but OSC thru for
-- supercollider is so much simpler than MIDI I can just throw it in here.
oscThru :: Sc.Patch.Patch -> Set Common.Flag -> ScoreT.Instrument
    -> Cmd.ThruFunction
oscThru patch flags scoreInst scale _attrs input = do
    -- attrs is used for keyswitches in MIDI, but sc doesn't support attrs yet.
    -- If I do, it'll probably be via string-valued controls.
    input <- convertInput scoreInst scale input
    (:[]) . Cmd.OscThru <$> inputToOsc patch flags input

inputToOsc :: Cmd.M m => Sc.Patch.Patch -> Set Common.Flag
    -> InputNote.InputNn -> m [OSC.OSC]
inputToOsc patch flags = pure . \case
    InputNote.NoteOn noteId nn vel ->
        Sc.Play.noteOn patch triggered (unid noteId) nn vel
    InputNote.NoteOff noteId _ ->
        Sc.Play.noteOff triggered (unid noteId)
    InputNote.Control noteId control val ->
        Sc.Play.setControl patch (unid noteId) control val
    InputNote.PitchChange noteId nn ->
        Sc.Play.pitchChange patch (unid noteId) nn
    where
    triggered = Common.Triggered `Set.member` flags
    unid (InputNote.NoteId id) = id

-- | I used to keep track of the previous PitchBend to avoid sending extra ones.
-- But it turns out I don't actually know the state of the MIDI channel, so
-- now I always send PitchBend.  I'm not sure why I ever thought it could work.
-- I could still do this by tracking channel state at the Midi.Interface level.
-- I actually already do that a bit of tracking with note_tracker, but it's
-- simpler to just always send PitchBend, unless it becomes a problem.
midiThru :: Patch.Patch -> Patch.Config -> ScoreT.Instrument
    -> Cmd.ThruFunction
midiThru patch config scoreInst scale attrs input = do
    input <- convertInput scoreInst scale input
    let addrs = Patch.configAddrs config
    if null addrs then pure [] else do
        (inputNn, ks) <- Cmd.require
            (pretty (Scale.scaleId scale) <> " doesn't have " <> pretty input)
            =<< inputToNn scoreInst (Patch.patchAttributeMap patch)
                (Patch.settings#Patch.scale #$ config) attrs input
        wdevState <- Cmd.getWdevState
        pbRange <- Cmd.require "no pb range" $
            Patch.settings#Patch.pitchBendRange #$ config
        maybe (pure []) (toMsgs ks) $
            inputToMidi pbRange wdevState addrs inputNn
    where
    toMsgs ks (thruMsgs, wdevState) = do
        Cmd.modifyWdevState (const wdevState)
        let ksMsgs = concatMap (keyswitchToMidi thruMsgs) ks
        pure $ map (uncurry Cmd.midiThru) $ ksMsgs ++ thruMsgs

-- | The keyswitch winds up being simultaneous with the note on.  Especially
-- stupid VSTs like kontakt will sometimes miss a keyswitch if it doesn't have
-- enough lead time.  There's not much I can do about that, but to avoid making
-- the keyswitch too short I hold it down along with the note.
keyswitchToMidi :: [(Midi.WriteDevice, Midi.Message)] -> Patch.Keyswitch
    -> [(Midi.WriteDevice, Midi.Message)]
keyswitchToMidi msgs ks = case msum (map noteMsg msgs) of
    Nothing -> []
    Just (addr, key, isNoteOn) -> map (withAddr addr) $
        if isNoteOn then [Patch.keyswitchOn key ks]
            else maybe [] (:[]) (Patch.keyswitchOff ks)
    where
    noteMsg (dev, Midi.ChannelMessage chan msg) = case msg of
        Midi.NoteOn key _ -> Just ((dev, chan), key, True)
        Midi.NoteOff key _ -> Just ((dev, chan), key, False)
        _ -> Nothing
    noteMsg _ = Nothing

-- | Realize the Input as a pitch in the given scale.
inputToNn :: Cmd.M m => ScoreT.Instrument -> Patch.AttributeMap
    -> Maybe Patch.Scale -> Attrs.Attributes -> InputNote.InputNn
    -> m (Maybe (InputNote.InputNn, [Patch.Keyswitch]))
inputToNn inst attrMap patchScale attrs = \case
    InputNote.NoteOn noteId nn vel -> justm (convert nn) $ \(nn, ks) ->
        pure $ Just (InputNote.NoteOn noteId nn vel, ks)
    InputNote.PitchChange noteId input -> justm (convert input) $ \(nn, _) ->
        pure $ Just (InputNote.PitchChange noteId nn, [])
    input@(InputNote.NoteOff {}) -> pure $ Just (input, ks)
        where
        ks = maybe [] (fst . snd) $ Common.lookupAttributes attrs attrMap
    input@(InputNote.Control {}) -> pure $ Just (input, [])
    where
    convert nn = do
        let (result, notFound) = convertPitch attrMap patchScale attrs nn
        when notFound $
            Log.warn $ "inst " <> pretty inst <> " doesn't have attrs "
                <> pretty attrs <> ", understood attrs are: "
                <> pretty (Common.mappedAttributes attrMap)
        pure result

-- | Convert a keyboard input into the NoteNumber desired by the scale.
convertInput :: Cmd.M m => ScoreT.Instrument -> Scale.Scale -> InputNote.Input
    -> m InputNote.InputNn
convertInput inst scale = \case
    InputNote.NoteOn noteId input vel -> do
        nn <- convert input
        pure $ InputNote.NoteOn noteId nn vel
    InputNote.PitchChange noteId input ->
        InputNote.PitchChange noteId <$> convert input
    InputNote.NoteOff noteId vel -> pure $ InputNote.NoteOff noteId vel
    InputNote.Control noteId control val ->
        pure $ InputNote.Control noteId control val
    where
    convert = convertInputPitch inst scale

convertInputPitch :: Cmd.M m => ScoreT.Instrument -> Scale.Scale
    -> Pitch.Input -> m Pitch.NoteNumber
convertInputPitch inst scale input = do
    (blockId, _, trackId, pos) <- Selection.getInsert
    -- I ignore _logs, any interesting errors should be in 'result'.
    (result, _logs) <- Perf.deriveAtExc blockId trackId $
        Derive.withInstrument inst $
        filterTransposers scale $
        Scale.scaleInputToNn scale pos input
    case result of
        Left (Derive.Error _ _ err) -> throw $ pretty err
        -- This just means the key isn't in the scale, it happens a lot so
        -- no need to shout about it.
        Right (Left DeriveT.InvalidInput) -> Cmd.abort
        Right (Left err) -> throw $ pretty err
        Right (Right nn) -> pure nn
    where
    throw = Cmd.throw . (("scale_input_to_nn for " <> pretty input <> ": ") <>)

-- | Remove transposers because otherwise the thru pitch doesn't match the
-- entered pitch and it's very confusing.  However, I retain 'Controls.octave'
-- and 'Controls.hz' because those are used to configure a scale, e.g. via
-- 'Patch.config_controls', and the pitch is nominally the same.
filterTransposers :: Scale.Scale -> Derive.Deriver a
    -> Derive.Deriver a
filterTransposers scale = Derive.withControls transposers
    where
    transposers = zip
        (filter (`notElem` [Controls.octave, Controls.hz])
            (Set.toList (Scale.scaleTransposers scale)))
        (repeat (ScoreT.untyped mempty))

-- | This is a midi thru version of 'Perform.Midi.Convert.convert_midi_pitch'.
-- It's different because it works with a scalar NoteNumber instead of
-- a Score.Event with a pitch signal, which makes it hard to share code.
convertPitch :: Patch.AttributeMap -> Maybe Patch.Scale -> Attrs.Attributes
    -> Pitch.NoteNumber -> (Maybe (Pitch.NoteNumber, [Patch.Keyswitch]), Bool)
    -- ^ The Bool is True if the attrs were non-empty but not found.
convertPitch attrMap patchScale attrs nn =
    case Common.lookupAttributes attrs attrMap of
        Nothing -> ((, []) <$> maybePitch, attrs /= mempty)
        Just (_, (keyswitches, maybeKeymap)) ->
            ( (, keyswitches) <$> maybe maybePitch setKeymap maybeKeymap
            , False
            )
    where
    maybePitch = applyPatchScale nn
    applyPatchScale = maybe Just Patch.convertScale patchScale
    setKeymap (Patch.UnpitchedKeymap key) = Just $ Midi.fromKey key
    setKeymap (Patch.PitchedKeymap low _ lowPitch) =
        (+ Midi.fromKey (low - lowPitch)) <$> maybePitch

inputToMidi :: Control.PbRange -> Cmd.WriteDeviceState
    -> [Addr] -> InputNote.InputNn
    -> Maybe ([(Midi.WriteDevice, Midi.Message)], Cmd.WriteDeviceState)
inputToMidi pbRange wdevState addrs inputNn = case alloc addrs inputNn of
    (Nothing, _) -> Nothing
    (Just addr, newState) -> Just (map (withAddr addr) msgs, state)
        where
        (msgs, noteKey) = InputNote.toMidi pbRange
            (Cmd.wdevNoteKey wdevState) inputNn
        state = mergeState newState
            (wdevState { Cmd.wdevNoteKey = noteKey })
    where
    alloc = allocAddr (Cmd.wdevNoteAddr wdevState)
        (Cmd.wdevAddrSerial wdevState) (Cmd.wdevSerial wdevState)

mergeState :: Maybe (Map NoteId Addr, Map Addr Cmd.Serial)
    -> Cmd.WriteDeviceState -> Cmd.WriteDeviceState
mergeState newState old = case newState of
    Nothing -> old
    Just (noteAddr, addrSerial) -> old
        { Cmd.wdevNoteAddr = noteAddr
        , Cmd.wdevAddrSerial = addrSerial
        , Cmd.wdevSerial = Cmd.wdevSerial old + 1
        }

-- | If the note_id is already playing in an addr, return that one.  Otherwise,
-- if it's not NoteOn or NoteOff, abort.  If it is, pick a free addr, and if
-- there is no free one, pick the oldest one.  Update the wdev state and assign
-- the note id to the addr.
allocAddr :: Map NoteId Addr -> Map Addr Cmd.Serial -> Cmd.Serial
    -> [Addr] -- ^ Addrs allocated to this instrument.
    -> InputNote.InputNn
    -> (Maybe Addr, Maybe (Map NoteId Addr, Map Addr Cmd.Serial))
allocAddr noteAddr addrSerial serial addrs input
    | Just addr <- Map.lookup noteId noteAddr, addr `elem` addrs =
        case input of
            InputNote.NoteOff _ _ -> (Just addr, unassign addr)
            _ -> (Just addr, Nothing)
    | not (newNote input) = (Nothing, Nothing)
    | Just addr <- oldest = (Just addr, assign addr)
    | otherwise = (Nothing, Nothing) -- addrs must be null
    where
    noteId = InputNote.inputId input
    newNote (InputNote.NoteOn {}) = True
    newNote (InputNote.NoteOff {}) = True
    newNote _ = False
    assign addr = Just
        (Map.insert noteId addr noteAddr, Map.insert addr serial addrSerial)
    unassign addr = Just
        (Map.delete noteId noteAddr, Map.insert addr serial addrSerial)
    -- Always pick the channel with the oldest note, whether or not it's
    -- allocated.  Previously I would try to pick a free one, but reusing
    -- a free channel led to audible artifacts with long-ringing instruments.
    oldest = Lists.minimumOn (flip Map.lookup addrSerial) addrs

withAddr :: Addr -> Midi.ChannelMessage -> (Midi.WriteDevice, Midi.Message)
withAddr (wdev, chan) msg = (wdev, Midi.ChannelMessage chan msg)


-- * util

-- | Send ChannelMessages to the addrs (or just the lowest addr) of the current
-- instrument.  This bypasses all of the WriteDeviceState stuff so it won't
-- cooperate with addr allocation, but hopefully this won't cause problems for
-- simple uses like keymapped instruments.
channelMessages :: Cmd.M m => Maybe ScoreT.Instrument -- ^ use this inst, or
    -- the one on the selected track if Nothing.
    -> Bool -> [Midi.ChannelMessage] -> m ()
channelMessages maybeInst firstAddr msgs = do
    addrs <- getAddrs maybeInst
    let addrs2 = if firstAddr then take 1 addrs else addrs
    sequence_
        [ Cmd.midi wdev (Midi.ChannelMessage chan msg)
        | (wdev, chan) <- addrs2, msg <- msgs
        ]

getAddrs :: Cmd.M m => Maybe ScoreT.Instrument -> m [Addr]
getAddrs maybeInst = do
    inst <- maybe (Cmd.abortUnless =<< EditUtil.lookupInstrument)
        pure maybeInst
    alloc <- Ui.allocation inst <#> Ui.get
    pure $ case UiConfig.allocBackend <$> alloc of
        Just (UiConfig.Midi config) -> Patch.configAddrs config
        _ -> []
