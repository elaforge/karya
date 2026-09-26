-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The default keyboard layout.

    I group cmds by their function.  Overlapping keys will be detected and show
    up in the log as a warning on startup.

    Some global cmds also have a command-key version, so you can invoke them
    even when kbd entry is on.

    Track-specific cmds are bound in 'Cmd.Track'.

    Global generic cmds:

    0. State level operations, like load, save, undo.  Set timestep mode, set
    edit mode.  Play, stop.  Move the selection.

    1. Modify text independent attributes, like start position and duration.
    This includes copy and paste, remove, modify duration, nudge, ...

    Track-specific cmds are looked up in "Cmd.Track".

    2. Track specific event operations, possibly affecting all events in the
    selection: transposition, etc.  These are active regardless of EditMode,
    and are found in Cmd.*TrackKeymap.

    3. Track specific event editing, which modifies a single event.
    E.g., input notes and note text, pitches in a certain scale, control
    values, etc.  This is enabled by edit mode, and will shadow printable keys,
    depending on the track type and edit mode.  Found in Cmd.*Track.

    4. Kbd entry, if on, will hijack the letter keys and turn them into
    NoteOns.

    Then there's midi recording:  (TODO which is theoretical since it's not
    implemented)

    In record mode, the block is played while recording midi msgs and their
    timestamps, which are later passed to the integrator to convert into
    events.
-}
module Cmd.GlobalKeymap (
    pureKeymap, ioKeymap, allKeymap, allKeymapErrors
) where
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Factor as Factor
import qualified Cmd.Keymap as Keymap
import           Cmd.Keymap
    (SimpleMod(..), bindClick, bindDrag, bindKey, bindKeyStatus,
     bindRepeatable, commandChar, plainChar, plainKey, secondaryChar,
     shiftChar)
import qualified Cmd.PhysicalKey as PhysicalKey
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Play as Play
import qualified Cmd.RulerCmd as RulerCmd
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Track as Track
import qualified Cmd.Undo as Undo
import qualified Cmd.ViewConfig as ViewConfig
import qualified Cmd.Views as Views

import qualified Derive.Scale as Scale
import qualified Ui.Block as Block
import qualified Ui.Key as Key
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ui as Ui

import           Global


pureKeymap :: Cmd.Handler Cmd.CmdId
pureKeymap = Cmd.Keymap $ fst $ Keymap.makeKeymap pureBindings

ioKeymap :: Cmd.Handler (Cmd.CmdT IO)
ioKeymap = Cmd.Keymap $ fst $ Keymap.makeKeymap ioBindings

-- | This is not useful for execution since the cmds themselves have been
-- stripped of their code, but it's still useful to find keymap collisions and
-- print a global keymap.  They're stripped to make them all the same type, so
-- they can all go into the same Keymap, so collision detection and
-- documentation doesn't have to care about 'pure_keymap' vs 'io_keymap'.
allKeymap :: Cmd.Keymap Cmd.CmdId
allKeymapErrors :: [Text]
(allKeymap, allKeymapErrors) =
    Keymap.makeKeymap (pureBindings ++ map strip ioBindings)
    where
    strip = second $ \(Cmd.NamedCmd name _) ->
        Cmd.NamedCmd name (const (pure Cmd.Done))

-- * io cmds

-- | Cmds that use IO.  This should be a limited to the small set of cmds that
-- need it.
ioBindings :: [Keymap.Binding (Cmd.CmdT IO)]
ioBindings = concat
    [ fileBindings, undoBindings, quitBindings
    -- This actually belongs in 'play_bindings', but needs to be in IO, because
    -- stopping involves possibly killing threads and stopping audio.
    , bindKeyStatus [] (Key.Char ' ') "stop or play" stopOrPlay
    ]

-- Previously a second space would contract the selection, but since then I
-- took over space for stop_or_play.
-- context_stop :: Cmd.CmdT IO ()
-- context_stop = unlessM Play.cmd_context_stop (Selection.to_point True)

stopOrPlay :: Cmd.CmdT IO Cmd.Status
stopOrPlay = Play.cmdContextStop >>= \case
    True -> pure Cmd.Done
    False -> Cmd.gets (Cmd.statePreviousPlay . Cmd.statePlay) >>= \case
        Nothing -> Cmd.Play <$> Play.rootTop
        Just (Cmd.PlayCmd _ cmd) -> Cmd.Play <$> Cmd.liftId cmd

fileBindings :: [Keymap.Binding (Cmd.CmdT IO)]
fileBindings = concat
    [ commandChar 'S' "save" $ Cmd.gets Cmd.stateSaveFile >>= \case
        Just (_, Cmd.SaveRepo repo) -> do
            -- Even when using git, write a standalone state as a safeguard.
            Save.writeCurrentState $
                Save.statePathForRepo (Path.toPath repo)
            Save.saveGitAs (Path.toPath repo)
        _ -> Save.saveState
    ]

undoBindings :: [Keymap.Binding (Cmd.CmdT IO)]
undoBindings = concat
    [ commandChar 'u' "undo" Undo.undo
    , commandChar 'r' "redo" Undo.redo
    ]

-- | Quit is special because it's the only Cmd that returns Cmd.Quit.
-- See how annoying it is to make a keymap by hand?
quitBindings :: [Keymap.Binding (Cmd.CmdT IO)]
quitBindings = concat
    [ bindKeyStatus [PrimaryCommand] q "soft quit" Save.softQuit
    , bindKeyStatus [Shift, PrimaryCommand] q "hard quit" Save.hardQuit
    ]
    where q = Key.Char (PhysicalKey.physicalKey 'q')

-- * pure cmds

-- | Cmds that don't use IO.  Exported from the module for the responder.
pureBindings :: [Keymap.Binding (Cmd.CmdT Identity.Identity)]
pureBindings = concat
    [ playBindings, mouseBindings, selectionBindings, stepPlayBindings
    , viewConfigBindings, blockConfigBindings, editStateBindings
    , eventBindings, pitchBindings, createBindings, clipBindings
    -- TODO use this to catch the focus bug
    , bindKey [SecondaryCommand] (Key.Char 'a') "debug" $
        Cmd.modify $ \st -> st { Cmd.stateDebug = not (Cmd.stateDebug st) }
    ]

playBindings :: [Keymap.Binding (Cmd.CmdT Identity.Identity)]
playBindings = concat
    -- The pattern is that the modifiers select where to start playing, and
    -- the key says whether it's the local block or from the root block.
    [ bind block local "play local block" Play.localBlock
    , bind sel local "play or loop local selection" Play.localSelection
    -- See if playing from the top is more useful than the previous step.
    , bind prev local "play local from top of window" Play.localTop
    , bind block root "play root from start" Play.rootBlock
    , bind sel root "play or loop root from local selection" Play.rootSelection
    -- It plays from the selection on the root, instead of the local one.
    -- This breaks the modifier+key pattern, but it's useful to manually set
    -- a play starting point on the root.
    , bind (block ++ sel) root "play root from root selection"
        Play.rootFromRootSelection
    , bind prev root "play root from top of window" Play.rootTop
    ]
    where
    bind smods key desc cmd = bindKeyStatus smods key desc $
        Cmd.Play <$> (cmd <* Play.setPreviousPlay desc cmd)
    block = [PrimaryCommand]
    sel = [Shift]
    prev = []
    local = Key.Enter
    root = Key.Char (PhysicalKey.physicalKey '\'')

-- | I bind the mouse by device rather than function, since I can't detect
-- overlaps as easily for mouse bindings.
mouseBindings :: Cmd.M m => [Keymap.Binding m]
mouseBindings = concat
    [ bindDrag [] btn Cmd.OnTrack "snap drag selection"
        (Selection.cmdSnapSelection btn False)
    , bindDrag [Shift] btn Cmd.OnTrack "snap drag selection"
        (Selection.cmdSnapSelection btn True)
    , bindDrag [PrimaryCommand] btn Cmd.OnTrack "drag selection"
        (Selection.cmdMouseSelection btn False)
    , bindDrag [Shift, PrimaryCommand] btn Cmd.OnTrack "extend selection"
        (Selection.cmdMouseSelection btn True)
    , Keymap.bindRelease [] btn Cmd.OnTrack "mouse release"
        (const Selection.recordHistory)

    , bindClick [] btn Cmd.OnTrack 2 "open block"
        (const (BlockConfig.cmdOpenBlock False))
    , bindClick [PrimaryCommand] btn Cmd.OnTrack 2 "open block"
        (const (BlockConfig.cmdOpenBlock True))

    -- TODO without a track_drag equivalent for skeleton clicks, this
    -- will interfere with the OnTrack bind_drag when you drag into the
    -- track.
    , bindDrag [] btn Cmd.OnSkeleton "select track"
        (Selection.cmdSelectTrack btn)
    , bindClick [] btn Cmd.OnSkeleton 2 "add block title"
        BlockConfig.cmdAddBlockTitle
    , bindClick [PrimaryCommand] btn Cmd.OnSkeleton 1
        "toggle skeleton edge" BlockConfig.cmdToggleEdge
    , bindClick [Shift] btn Cmd.OnSkeleton 1 "move tracks"
        BlockConfig.cmdMoveTracks
    , bindClick [SecondaryCommand] btn Cmd.OnSkeleton 1 "toggle mute"
        BlockConfig.cmdMuteOrUnsolo
    , bindClick [SecondaryCommand] btn Cmd.OnSkeleton 2 "toggle solo"
        BlockConfig.cmdSetSolo

    , bindClick [Shift] btn Cmd.OnDivider 1 "expand collapsed"
        BlockConfig.cmdExpandTrack
    ]
    where
    btn = Config.mouseSelect

selectionBindings :: Cmd.M m => [Keymap.Binding m]
selectionBindings = concat
    [ bindRepeatable [] Key.Down "advance selection" $
        Selection.step TimeStep.Advance move
    , bindRepeatable [Shift] Key.Down "extend advance selection" $
        Selection.step TimeStep.Advance Selection.Extend

    , bindRepeatable [] Key.Up "rewind selection" $
        Selection.step TimeStep.Rewind move
    , bindRepeatable [Shift] Key.Up "extend rewind selection" $
        Selection.step TimeStep.Rewind Selection.Extend

    , bindRepeatable [] Key.Right "shift selection right" $
        Selection.shift True move 1
    , bindRepeatable [Shift] Key.Right "extend selection right" $
        Selection.shift True Selection.Extend 1

    , bindRepeatable [] Key.Left "shift selection left" $
        Selection.shift True move (-1)
    , bindRepeatable [Shift] Key.Left "extend selection left" $
        Selection.shift True Selection.Extend (-1)

    , repeatableChar 'h' "move selection left" $
        Selection.shift True move (-1)
    , repeatableChar 'H' "move selection left" $
        Selection.shift True Selection.Extend (-1)
    , repeatableChar 'l' "move selection right" $
        Selection.shift True move 1
    , repeatableChar 'L' "move selection right" $
        Selection.shift True Selection.Extend 1
    , repeatableChar 'j' "move selection advance" $
        Selection.step TimeStep.Advance move
    , repeatableChar 'J' "move selection advance" $
        Selection.step TimeStep.Advance Selection.Extend
    , repeatableChar 'k' "move selection rewind" $
        Selection.step TimeStep.Rewind move
    , repeatableChar 'K' "move selection rewind" $
        Selection.step TimeStep.Rewind Selection.Extend
    -- Mnemonic: next, previous.
    , repeatableChar 'n' "move to next note track" $
        Selection.jumpToTrack move =<< Cmd.abortUnless
            =<< Selection.findNoteTrack Selection.R False
    , repeatableChar 'p' "move to previous note track" $
        Selection.jumpToTrack move =<< Cmd.abortUnless
            =<< Selection.findNoteTrack Selection.L False
    , plainChar 'N' "expand to next note track" $
        Selection.jumpToTrack Selection.Extend =<< Cmd.abortUnless
            =<< Selection.findNoteTrack Selection.R True
    , plainChar 'P' "expand to previous note track" $
        Selection.jumpToTrack Selection.Extend =<< Cmd.abortUnless
            =<< Selection.findNoteTrack Selection.L True

    , repeatableChar 'w' "move to next event" $
        Selection.stepWith 1 move =<< Track.eventAndNoteStep
    , repeatableChar 'W' "extend to next event" $
        Selection.stepWith 1 Selection.Extend =<< Track.eventAndNoteStep
    , repeatableChar 'b' "move to previous event" $
        Selection.stepWith (-1) move =<< Track.eventAndNoteStep
    , repeatableChar 'B' "extend to previous event" $
        Selection.stepWith (-1) Selection.Extend =<< Track.eventAndNoteStep

    , bindKey [PrimaryCommand] (Key.Char 'a') "select track / all"
        Selection.cmdTrackAll
    , bindKey [PrimaryCommand] (Key.Char 'A') "toggle extend tracks"
        Selection.cmdToggleExtendTracks

    , bindKey [Shift] (Key.Char ' ') "selection start to point"
        (Selection.toPoint True)
    ]
    where
    move = Selection.defaultMove
    repeatableChar c = bindRepeatable [] (Key.Char c)

stepPlayBindings :: Cmd.M m => [Keymap.Binding m]
stepPlayBindings = concat
    [ bindRepeatable [PrimaryCommand] Key.Down
        "step play advance" (StepPlay.cmdSetOrAdvance False)
    , bindRepeatable [Shift, PrimaryCommand] Key.Down
        "step play tracks advance" (StepPlay.cmdSetOrAdvance True)
    , bindRepeatable [PrimaryCommand] Key.Up
        "step play rewind" StepPlay.cmdRewind
    , bindRepeatable [Shift, PrimaryCommand] Key.Up
        "step play rewind" StepPlay.cmdRewind
    , bindKey [PrimaryCommand] Key.Right
        "step play here" (StepPlay.cmdHere False)
    , bindKey [Shift, PrimaryCommand] Key.Right
        "step play tracks here" (StepPlay.cmdHere True)
    ]

viewConfigBindings :: Cmd.M m => [Keymap.Binding m]
viewConfigBindings = concat
    [ plainChar '[' "zoom out step" (ViewConfig.zoomByRank TimeStep.Rewind)
    , plainChar ']' "zoom in step" (ViewConfig.zoomByRank TimeStep.Advance)
    , plainChar '{' "zoom out *0.8" (ViewConfig.cmdZoomAroundInsert (*0.8))
    , plainChar '}' "zoom in *1.25" (ViewConfig.cmdZoomAroundInsert (*1.25))
    -- undo and redo for selection, but I never use it
    , secondaryChar '[' "previous selection"
        (Selection.previousSelection True)
    , secondaryChar ']' "next selection" (Selection.nextSelection True)

    -- TODO experimental
    , commandChar 'q' "set suggested track widths"
        (ViewConfig.setSuggestedTrackWidths =<< Cmd.getFocusedView)

    , plainChar '\\' "zoom to ruler or selection"
        ViewConfig.zoomToRulerOrSelection
    , commandChar 'R' "resize to fit"
        (Views.resizeToFit False =<< Cmd.getFocusedView)
    , commandChar 'L' "horizontal tile" ViewConfig.horizontalTile
    , commandChar '\\' "maximize and zoom"
        (Views.maximizeAndZoom =<< Cmd.getFocusedView)

    -- Unfortunately cmd-` is taken by an edit state bind, and they all live
    -- there so it would create an inconsistency to move just one.
    , secondaryChar '`' "cycle focus forward" (ViewConfig.cycleFocus True)
    , bindKey [SecondaryCommand, Shift] (Key.Char '`') "cycle focus backward"
        (ViewConfig.cycleFocus False)
    , secondaryChar 'H' "block focus left" $
        ViewConfig.moveFocus ViewConfig.West
    , secondaryChar 'J' "block focus down" $
        ViewConfig.moveFocus ViewConfig.South
    , secondaryChar 'K' "block focus up" $
        ViewConfig.moveFocus ViewConfig.North
    , secondaryChar 'L' "block focus right" $
        ViewConfig.moveFocus ViewConfig.East
    , secondaryChar 'W' "close view" $
        Ui.destroyView =<< Cmd.getFocusedView

    , secondaryChar 'f' "scroll advance" $ ViewConfig.scrollPages 0.75
    , secondaryChar 'b' "scroll rewind" $ ViewConfig.scrollPages (-0.75)
    , bindKey [] Key.Home "scroll home" ViewConfig.scrollToHome
    , bindKey [] Key.End "scroll end" ViewConfig.scrollToEnd
    , bindKey [] Key.PageDown "scroll advance" $ ViewConfig.scrollPages 0.75
    , bindKey [] Key.PageUp "scroll rewind" $ ViewConfig.scrollPages (-0.75)
    ]

blockConfigBindings :: Cmd.M m => [Keymap.Binding m]
blockConfigBindings = concat
    [ plainChar 'M' "toggle mute" (BlockConfig.cmdToggleFlag Block.Mute)
    , plainChar 'S' "toggle solo" (BlockConfig.cmdToggleFlag Block.Solo)
    , plainChar 'D' "toggle disable"
        (BlockConfig.cmdToggleFlag Block.Disable)
    , plainChar 'C' "toggle collapse"
        (BlockConfig.cmdToggleFlag Block.Collapse)
    , commandChar 'C' "collapse empty"
        (BlockConfig.toggleCollapseEmpty =<< Cmd.getFocusedBlock)
    , commandChar 'M' "toggle merge all"
        (BlockConfig.toggleMergeAll =<< Cmd.getFocusedBlock)
    , commandChar 'm' "toggle merged" BlockConfig.toggleMergeSelected

    -- The idea is cmd-; works in kbd mode.
    , commandChar ';' "add ruler section" RulerCmd.localAddSection
    , plainChar ':' "clip block" BlockConfig.clip
    ]

-- | Modify global edit state.
editStateBindings :: Cmd.M m => [Keymap.Binding m]
editStateBindings = concat
    [ plainKey Key.Escape "toggle val edit" Edit.cmdToggleValEdit
    , commandChar '[' "toggle val edit" Edit.cmdToggleValEdit
    , bindKey [] Key.Tab "toggle method edit" Edit.cmdToggleMethodEdit
    , commandChar ']' "toggle kbd entry mode" Edit.cmdToggleKbdEntry
    , commandChar '{' "toogle val edit and kbd entry"
        Edit.cmdToggleValEditKbdEntry

    , commandChar '0' "set event step" $ Edit.setStep TimeStep.eventStep
    , uncurry (commandChar '1') (stepRank Meter.Section)
    , uncurry (commandChar '2') (stepRank Meter.W)
    , uncurry (commandChar '3') (stepRank Meter.H)
    , uncurry (commandChar '4') (stepRank Meter.Q)
    , uncurry (commandChar '5') (stepRank Meter.E)
    , uncurry (commandChar '6') (stepRank Meter.S)
    , uncurry (commandChar '7') (stepRank Meter.T32)
    , uncurry (commandChar '8') (stepRank Meter.T64)
    , shiftChar '=' "toggle duration" Edit.toggleNoteDuration

    , bindKey [PrimaryCommand] (Key.Char '`') "toggle absolute/relative step"
        Edit.toggleAbsoluteRelativeStep
    , bindKey [PrimaryCommand, Shift] (Key.Char '`')
        "toggle input note orientation"
        (Edit.cmdToggleNoteOrientation *> Selection.updateOrientation)
    , plainChar '`' "toggle advance" Edit.toggleAdvance
    , shiftChar '`' "toggle chord" Edit.toggleChord

    , plainChar '-' "octave -1" (Edit.cmdModifyOctave (subtract 1))
    , plainChar '=' "octave +1" (Edit.cmdModifyOctave (+1))
    , commandChar '-' "play speed / 9/8"
        (Play.modifyPlayMultiplier (/ (9/8)))
    , commandChar '=' "play speed * 9/8"
        (Play.modifyPlayMultiplier (* (9/8)))
    ]
    where
    stepRank rank =
        ( "set step: " <> TimeStep.showTimeStep step
        , Edit.setStepRank step rank
        )
        where step = TimeStep.rank rank

-- delete = remove events and move following events back
-- clear = just remove events

-- | Modify events.
eventBindings :: Cmd.M m => [Keymap.Binding m]
eventBindings = concat
    -- J = move previous event to cursor, K = move next event to cursor.
    [ commandChar 'J' "move event forward" Edit.cmdMoveEventForward
    , commandChar 'K' "move event backward" Edit.cmdMoveEventBackward
    , commandChar 'j' "insert time" Edit.cmdInsertTime
    , commandChar 'k' "delete time" Edit.cmdDeleteTime
    -- Unlike other event editing commands, you don't have to be in insert
    -- mode to remove events.  Maybe I'll change that later.
    , plainKey Key.Backspace "clear selected" Edit.cmdClearAndAdvance
    , commandChar 'o' "join events" Edit.cmdJoinEvents
    , commandChar 'O' "split events" Edit.cmdSplitEvents

    -- This used to be cmd-s but 'a' is unmapped in kbd entry and I often want
    -- to adjust note durations.
    , plainChar 'a' "set dur" Edit.cmdSetDuration
    , commandChar 'z' "set call duration" Edit.cmdSetCallDuration
    , commandChar 'Z' "toggle zero-dur" Edit.cmdToggleZeroTimestep
    , commandChar 'g' "set start" Edit.cmdSetStart
    , commandChar 'G' "invert orientation" Edit.cmdInvertOrientation

    , bindRepeatable [] (Key.Char '.') "run last action"
        (Edit.runActionAt '.')
    , shiftChar '1' "run action 1" (Edit.runActionAt '1')
    , shiftChar '2' "run action 2" (Edit.runActionAt '2')
    , shiftChar '3' "run action 3" (Edit.runActionAt '3')
    , shiftChar '4' "run action 4" (Edit.runActionAt '4')
    , shiftChar '5' "cycle enharmonic"
        (PitchTrack.pitches PitchTrack.cycleEnharmonics)
    , shiftCommand '1' "record in slot 1" (Edit.saveLastActionTo '1')
    , shiftCommand '2' "record in slot 2" (Edit.saveLastActionTo '2')
    , shiftCommand '3' "record in slot 3" (Edit.saveLastActionTo '3')
    , shiftCommand '4' "record in slot 4" (Edit.saveLastActionTo '4')

    -- modify event text

    -- There are placed to avoid conflict with 'Cmd.state_kbd_entry' for either
    -- pitched instruments or specialized keymaps like drums.
    , bindKeyStatus [] (Key.Char (PhysicalKey.physicalKey '/'))
        "append text" Edit.appendText
    , bindKeyStatus [] (Key.Char (PhysicalKey.physicalKey '?'))
        "replace last call" Edit.replaceLastCall
    , bindKeyStatus [] (Key.Char 'i') "prepend text" Edit.prependText
    , bindKeyStatus [] (Key.Char 'I') "replace first call"
        Edit.replaceFirstCall

    , bindKey [SecondaryCommand] (Key.Char 'c')
        "toggle commented" Edit.cmdToggleCommented
    , commandChar ',' "strip transformer" Edit.stripTransformer
    ]
    where
    shiftCommand = bindKey [Shift, PrimaryCommand] . Key.Char

-- | Bindings which work on pitch tracks.  The reason this is global rather
-- than in pitch track keymaps is that it's handy to select multiple tracks
-- and have the cmd automatically skip non pitch tracks.  This only really
-- applies to pitch tracks though, because they are often collapsed and edited
-- from their note track.
pitchBindings :: Cmd.M m => [Keymap.Binding m]
pitchBindings = concat
    -- These are named after the vi commands for up and down, but they don't
    -- feel right.
    [ commandChar 'y' "transpose up chromatic degree" $
        PitchTrack.transposeSelection Scale.Chromatic 0 1
    , commandChar 'e' "transpose down chromatic degree" $
        PitchTrack.transposeSelection Scale.Chromatic 0 (-1)
    , bindKey [SecondaryCommand] (Key.Char 'y')
        "transpose up diatonic degree" $
        PitchTrack.transposeSelection Scale.Diatonic 0 1
    , bindKey [SecondaryCommand] (Key.Char 'e')
        "transpose down diatonic degree" $
        PitchTrack.transposeSelection Scale.Diatonic 0 (-1)
    , commandChar 'Y' "transpose up octave" $
        PitchTrack.transposeSelection Scale.Chromatic 1 0
    , commandChar 'E' "transpose down octave" $
        PitchTrack.transposeSelection Scale.Chromatic (-1) 0
    ]

createBindings :: Cmd.M m => [Keymap.Binding m]
createBindings = concat
    [ commandChar 'n' "insert track right" (void Create.insertTrackRight)
    , commandChar 't' "splice track below" (void Create.spliceBelow)
    , commandChar 'T' "insert branch" Create.insertBranch
    , commandChar 'h' "splice track above" (void Create.spliceAbove)
    , commandChar 'H' "splice track above ancestors"
        (void Create.spliceAboveAncestors)
    , commandChar 'd' "delete tracks" Create.destroySelectedTracks

    , commandChar 'N' "create view"
        (void $ Create.view =<< Cmd.getFocusedBlock)
    , commandChar 'b' "create block"
        (void $ Create.view =<< Create.block =<< Ui.blockRuler
            =<< Cmd.getFocusedBlock)
    , commandChar 'B' "create block from template or selection"
        Factor.blockFromTemplate
    ]

clipBindings :: Cmd.M m => [Keymap.Binding m]
clipBindings = concat
    [ commandChar 'c' "copy selection" Clip.cmdCopySelection
    , commandChar 'x' "cut selection" Clip.cmdCutSelection
    , commandChar 'v' "paste selection" Clip.cmdPasteOverwrite
    , commandChar 'V' "insert selection" Clip.cmdPasteInsert
    , commandChar 'i' "merge selection" Clip.cmdPasteMerge
    ]
