-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Get track-specific Cmds.  Essentially, this detects the type of track
-- under the selection, and then returns cmds from one of "Cmd.NoteTrack",
-- "Cmd.PitchTrack", or "Cmd.ControlTrack".  This also handles per-instrument
-- and per-scale cmds.
module Cmd.Track (
    trackCmd
    , eventAndNoteStep
) where
import qualified Control.Monad.Except as Except

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.Edit as Edit
import qualified Cmd.Info as Info
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap
import qualified Cmd.Perf as Perf
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.ParseTitle as ParseTitle
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Ui.Ui as Ui

import           Global
import           Types


trackCmd :: Msg.Msg -> Cmd.CmdId Cmd.Status
trackCmd msg = do
    cmds <- getTrackCmds `Except.catchError` \exc -> do
        case exc of
            Ui.Abort -> pure ()
            Ui.Error stack msg ->
                Log.write $ Log.msgCallStack stack Log.Warn Nothing
                    ("getting track cmds: " <> msg)
        pure []
    Cmd.sequenceCmds cmds msg

-- | Get cmds according to the currently focused block and track.
getTrackCmds :: Cmd.CmdId [Msg.Msg -> Cmd.CmdId Cmd.Status]
getTrackCmds = do
    -- If this fails, it means the the track type can't be determined and there
    -- will be no track cmds.
    blockId <- Cmd.getFocusedBlock
    tracknum <- Cmd.abortUnless =<< Cmd.getInsertTracknum
    trackId <- Cmd.abortUnless =<< Ui.eventTrackAt blockId tracknum
    mbResolved <- lookupInst blockId trackId
    trackType <- ParseTitle.trackType <$> Ui.getTrackTitle trackId
    hasNoteChildren <- Info.hasNoteChildren blockId tracknum

    editState <- Cmd.gets Cmd.stateEdit
    let editMode = Cmd.stateEditMode editState
    let withInput = NoteEntry.cmdsWithInput
            (Cmd.stateKbdEntry editState)
            (fmap snd . Cmd.midiPatch =<< mbResolved)
    -- The order is important:
    -- - Per-instrument cmds can override all others.
    --
    -- - The note cmds make sure that kbd entry can take over the kbd, and midi
    -- thru gets first whack at incoming midi.
    --
    -- - Track cmds are also wanting to take over the whole keyboard.
    --
    -- - Keymap cmds are "background" and only apply if no more special mode is
    -- active, so they go last.
    --
    -- Instrument cmds aren't under 'NoteEntry.cmds_with_input' so they don't
    -- get 'Pitch.Input's.  This is because if they are creating their own
    -- kbd entry then they will want the underlying keystrokes, as drum
    -- mappings do.  If they want 'Pitch.Input's, they can call
    -- 'NoteEntry.cmds_with_input'.
    pure $ concat
        [ case (trackType, mbResolved) of
            (ParseTitle.NoteTrack, Just resolved) ->
                map Cmd.call $ Cmd.instCmds $ Common.commonCode $
                    Inst.instCommon $ Cmd.instInstrument resolved
            _ -> []
        , (:[]) $ Edit.handleFloatingInput $ case trackType of
            ParseTitle.NoteTrack -> False
            _ -> True
        , (:[]) $ withInput (inputCmds editMode trackType hasNoteChildren)
        , trackTypeCmds editMode trackType
        , keymapCmds trackType
        ]

lookupInst :: Cmd.M m => BlockId -> TrackId -> m (Maybe Cmd.ResolvedInstrument)
lookupInst blockId trackId =
    justm (Perf.lookupInstrument (blockId, Just trackId)) $ \inst ->
    Cmd.lookupInstrument inst

-- | Cmds that use InputNotes, and hence must be called with
-- 'NoteEntry.cmds_with_input'.
inputCmds :: Cmd.EditMode -> ParseTitle.Type -> Bool
    -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
inputCmds editMode trackType hasNoteChildren =
    universal ++ case trackType of
        ParseTitle.NoteTrack
            | hasNoteChildren -> []
            | otherwise -> case editMode of
                Cmd.ValEdit -> [NoteTrack.cmdValEdit]
                _ -> []
        ParseTitle.PitchTrack -> case editMode of
            Cmd.ValEdit -> [PitchTrack.cmdValEdit]
            _ -> []
        ParseTitle.ControlTrack -> case editMode of
            Cmd.ValEdit -> [ControlTrack.cmdValEdit]
            _ -> []
        ParseTitle.TempoTrack -> case editMode of
            Cmd.ValEdit -> [ControlTrack.cmdTempoValEdit]
            _ -> []
    where
    universal =
        [ PitchTrack.cmdRecordNoteStatus
        , MidiThru.cmdMidiThru
        , NoteEntry.floatingInputInsert
        ]

-- | Track-specific Cmds.
trackTypeCmds :: Cmd.EditMode -> ParseTitle.Type
    -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
trackTypeCmds editMode = \case
    ParseTitle.NoteTrack -> case editMode of
        Cmd.MethodEdit -> [NoteTrack.cmdMethodEdit]
        _ -> []
    ParseTitle.PitchTrack -> case editMode of
        Cmd.MethodEdit -> [PitchTrack.cmdMethodEdit]
        _ -> []
    ParseTitle.ControlTrack -> case editMode of
        Cmd.MethodEdit -> [ControlTrack.cmdMethodEdit]
        _ -> []
    ParseTitle.TempoTrack -> case editMode of
        Cmd.MethodEdit -> [ControlTrack.cmdMethodEdit]
        _ -> []

-- | Track-specific keymaps.
keymapCmds :: ParseTitle.Type -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
keymapCmds = \case
    ParseTitle.NoteTrack -> [Cmd.call $ Cmd.Keymap $ NoteTrackKeymap.keymap]
    _ -> []


-- * misc

-- | Like 'TimeStep.event_step', step to start and end of events.  But also
-- step to the start and end of the events of a parent note track, if any.
eventAndNoteStep :: Cmd.M m => m TimeStep.TimeStep
eventAndNoteStep = do
    (blockId, tracknum, _, _) <- Selection.getInsert
    maybeTrack <- Info.lookupTrackType blockId tracknum
    noteTracknum <- case Info.trackType <$> maybeTrack of
        Nothing -> pure Nothing
        Just (Info.Note {}) -> pure Nothing
        Just (Info.Pitch Nothing) -> pure Nothing
        Just (Info.Pitch (Just note)) -> pure $ Just $ Ui.trackTracknum note
        Just (Info.Control tracks) -> firstJusts (map noteTracknumOf tracks)
    let tracknums = TimeStep.TrackNums $
            [tracknum] ++ maybe [] (:[]) noteTracknum
    pure $ TimeStep.fromList
        [ TimeStep.EventStart tracknums, TimeStep.EventEnd tracknums
        , TimeStep.BlockEdge
        ]
    where
    noteTracknumOf track = ifM (isNote track)
        (pure (Just (Ui.trackTracknum track))) (pure Nothing)
    isNote = fmap ParseTitle.isNoteTrack . Ui.getTrackTitle
        . Ui.trackId
