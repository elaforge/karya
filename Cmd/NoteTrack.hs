-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
{- | Cmds to add notes to a note track.

    This module is sister to "Derive.Note" since it edits events that
    Derive.Note parses.

    Note events are usually given a duration of the current time step.  If a
    "trigger only" instrument (e.g. percussion) is in scope, they are created
    with zero duration.  Also, starting a raw edit with space will create a
    zero duration event.
-}
module Cmd.NoteTrack (
    ControlTrack(..)
    , cmdValEdit
    , cmdMethodEdit
) where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.Create as Create
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Info as Info
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection

import qualified Derive.Controls as Controls
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * val edit

-- | A control track belonging to the note track.  This can be a pitch track,
-- or a dyn track.
data ControlTrack = ControlTrack
    { trackNote :: TrackNum
    , trackControl :: TrackNum
    } deriving (Show, Eq)

-- | The val edit for note tracks edits its pitch track (possibly creating one
-- if necessary), and creates a blank event on the note track.  It may also
-- edit multiple pitch tracks for chords, or record velocity in addition to
-- pitch.
--
-- If I'm in chord mode, try to find the next track and put notes there.  If
-- there is no appropriate next track, the cmd will throw an error.
cmdValEdit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdValEdit msg = Cmd.suppressHistory Cmd.ValEdit "note track val edit" $ do
    EditUtil.fallthrough EditUtil.NoBackspace msg
    EditUtil.Pos blockId selTracknum pos dur <- EditUtil.getPos
    case msg of
        Msg.InputNote inputNote -> case inputNote of
            InputNote.NoteOn noteId input vel -> do
                note <- EditUtil.inputToNote input
                noteOn blockId selTracknum pos dur noteId note vel
            InputNote.PitchChange noteId input -> do
                (pitchTracknum, trackId) <- Cmd.require
                    ("no track for note_id " <> showt noteId)
                    =<< findPitchTrack noteId
                note <- EditUtil.inputToNote input
                -- If advance is set, the selection may have advanced past
                -- the pitch's position, so look for a previous event.
                pos <- eventPosAtOrBefore trackId pos
                PitchTrack.valEditAt
                    (EditUtil.Pos blockId pitchTracknum pos dur) note
            InputNote.NoteOff noteId _vel -> do
                dissociateNoteId noteId
                chordDone <- andM [getState Cmd.stateChord, allKeysUp]
                when chordDone $ setTempSel pos Nothing
                whenM (andM [pure chordDone, getState Cmd.stateAdvance])
                    Selection.advance
            InputNote.Control {} -> pure ()
        _ -> Cmd.abort
    pure Cmd.Done
    where
    -- NoteOn handling is especially complicated.
    noteOn blockId selTracknum pos dur noteId note vel = do
        chordMode <- getState Cmd.stateChord
        -- Pitch track.
        (ctrack, create) <- if chordMode
            then do
                (ctrack, create, maybeNext) <-
                    nextControlTrack blockId selTracknum isPitch
                setTempSel pos maybeNext
                pure (ctrack, create)
            else thisControlTrack blockId selTracknum isPitch
        when create $ createPitchTrack blockId ctrack
        associateNoteId blockId (trackControl ctrack) noteId
        orient <- Cmd.gets (Cmd.stateNoteOrientation . Cmd.stateEdit)
        let pitchPos = EditUtil.Pos blockId (trackControl ctrack)
                pos (if orient == Types.Positive then 0 else -0)
        PitchTrack.valEditAt pitchPos note
        -- Dyn track.
        whenM (getState Cmd.stateRecordVelocity) $ do
            (dtrack, create) <- if chordMode
                then (\(a, b, _) -> (a, b)) <$>
                    nextControlTrack blockId selTracknum isDyn
                else thisControlTrack blockId selTracknum isDyn
            when create $ createDynTrack blockId dtrack
            ControlTrack.valEditAt
                (EditUtil.Pos blockId (trackControl dtrack) pos 0) vel

        -- Create note and advance.
        ensureNoteEvent (EditUtil.Pos blockId (trackNote ctrack) pos dur)
        advanceMode <- getState Cmd.stateAdvance
        when (advanceMode && not chordMode) Selection.advance

    isPitch = ParseTitle.isPitchTrack
    isDyn = (== Just Controls.dynamic) . ParseTitle.titleToControl
    setTempSel pos maybeTracknum = Selection.setCurrent
        Config.temporaryInsertSelnum $
            fmap (\num -> Sel.point num pos Sel.Positive) maybeTracknum
    dissociateNoteId noteId = Cmd.modifyWdevState $ \st -> st
        { Cmd.wdevPitchTrack = Map.delete noteId (Cmd.wdevPitchTrack st) }
    associateNoteId blockId tracknum noteId = Cmd.modifyWdevState $
        \st -> st { Cmd.wdevPitchTrack =
            Map.insert noteId (blockId, tracknum) (Cmd.wdevPitchTrack st) }

-- | Find the next available control track.  Available means it is the given
-- track or is to its right, has either the same instrument or has the default
-- instrument, and doesn't already have a note_id associated with it.
--
-- If none is found, return the tracknum at which one should be created.
nextControlTrack :: Cmd.M m => BlockId -> TrackNum -> (Text -> Bool)
    -> m (ControlTrack, Bool, Maybe TrackNum)
    -- ^ (selected_track_pair, should_create, next_control_track)
nextControlTrack blockId tracknum isControl = do
    wdev <- Cmd.getWdevState
    let associated =
            [tracknum | (_, tracknum) <- Map.elems (Cmd.wdevPitchTrack wdev)]
    tracks <- Info.blockTracks blockId
    inst <- Info.getInstrumentOf blockId tracknum
    let find rightOf = findM (candidate inst associated rightOf) tracks
    found <- find tracknum
    case found of
        Nothing -> Cmd.throw $ "no next note track in " <> showt blockId
        Just track -> do
            (ctrack, create) <- shouldCreateControl blockId track isControl
            next <- find (trackControl ctrack + 1)
            pure (ctrack, create,
                Ui.trackTracknum . Info.trackInfo <$> next)
    where
    -- Wow, monads can be awkward.
    candidate inst associated rightOf
            (Info.Track track (Info.Note controls _)) =
        andM
            [ pure $ tracknum >= rightOf
            , pure $ maybe True (`notElem` associated) pitchTracknum
            , (== Just inst) <$> Info.lookupInstrumentOf blockId tracknum
            ]
        where
        tracknum = Ui.trackTracknum track
        pitchTracknum = Ui.trackTracknum <$>
            List.find (isControl . Ui.trackTitle) controls
    candidate _ _ _ _ = pure False

-- | The given track should be a note track.  Figure out if it has a control
-- track, or if one should be created.
thisControlTrack :: Cmd.M m => BlockId -> TrackNum -> (Text -> Bool)
    -> m (ControlTrack, Bool)
thisControlTrack blockId tracknum isControl = do
    track <- Info.getTrackType blockId tracknum
    shouldCreateControl blockId track isControl

-- | Find the ControlTrack of the given note Track.  If there is none, return
-- the tracknum where you should create one.
shouldCreateControl :: Cmd.M m => BlockId -> Info.Track
    -> (Text -> Bool) -> m (ControlTrack, Bool)
shouldCreateControl blockId track isControl = case Info.trackType track of
    Info.Note controls _ -> case find controls of
        Nothing -> pure (ControlTrack tracknum (tracknum+1), True)
        Just control ->
            pure (ControlTrack tracknum (Ui.trackTracknum control), False)
    ttype -> Cmd.throw $ "expected a note track for "
        <> showt (blockId, tracknum) <> " but got " <> showt ttype
    where
    find = List.find (isControl . Ui.trackTitle)
    tracknum = Ui.trackTracknum (Info.trackInfo track)

eventPosAtOrBefore :: Cmd.M m => TrackId -> ScoreTime -> m ScoreTime
eventPosAtOrBefore trackId pos = do
    (_, events) <- Events.splitAtBefore pos <$> Ui.getEvents trackId
    pure $ maybe pos Event.start (Lists.head events)

allKeysUp :: Cmd.M m => m Bool
allKeysUp = do
    st <- Cmd.getWdevState
    pure (Map.null (Cmd.wdevPitchTrack st))

-- | Find the pitch track associated with the given NoteId, if one exists.
findPitchTrack :: Cmd.M m => InputNote.NoteId
    -> m (Maybe (TrackNum, TrackId))
findPitchTrack noteId = do
    st <- Cmd.getWdevState
    let maybeTrack = Map.lookup noteId (Cmd.wdevPitchTrack st)
    case maybeTrack of
        Nothing -> pure Nothing
        Just (blockId, tracknum) -> do
            trackId <- Ui.getEventTrackAt blockId tracknum
            pure $ Just (tracknum, trackId)


-- * method edit

-- | Method edit is redirected to the pitch track, creating one if necessary.
cmdMethodEdit :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmdMethodEdit msg =
    Cmd.suppressHistory Cmd.MethodEdit "note track method edit" $ do
    EditUtil.fallthrough EditUtil.WantBackspace msg
    case msg of
        (EditUtil.methodKey -> Just key) -> do
            (blockId, tracknum, _, pos) <- Selection.getInsert
            (ctrack, create) <- thisControlTrack blockId tracknum
                ParseTitle.isPitchTrack
            when create $ createPitchTrack blockId ctrack
            PitchTrack.methodEditAt
                (EditUtil.Pos blockId (trackControl ctrack) pos 0) key
            ensureNoteEvent (EditUtil.Pos blockId (trackNote ctrack) pos 0)
        _ -> Cmd.abort
    pure Cmd.Done

-- * implementation

-- | Create a pitch track.
createPitchTrack :: Cmd.M m => BlockId -> ControlTrack -> m ()
createPitchTrack blockId (ControlTrack note pitch) = do
    createTrack blockId pitch "*"
    when (pitch == note + 1) $ Ui.mergeTrack blockId note pitch
    Create.widen =<< Cmd.getFocusedView
    -- Link note track underneath newly created pitch track.
    whenM (Ui.hasExplicitSkeleton blockId) $
        Ui.spliceSkeletonBelow blockId pitch note

createDynTrack :: Cmd.M m => BlockId -> ControlTrack -> m ()
createDynTrack blockId (ControlTrack note dyn) = do
    createTrack blockId dyn $
        ParseTitle.controlToTitle (ScoreT.untyped Controls.dynamic)
    Create.widen =<< Cmd.getFocusedView
    whenM (Ui.hasExplicitSkeleton blockId) $
        Ui.spliceSkeletonBelow blockId dyn note

createTrack :: Cmd.M m => BlockId -> TrackNum -> Text -> m ()
createTrack blockId tracknum title =
    void $ Create.track blockId tracknum title Events.empty

-- | Ensure that a note event exists at the given spot.  An existing event is
-- left alone, but if there is no existing event a new one will be created.
ensureNoteEvent :: Cmd.M m => EditUtil.Pos -> m ()
ensureNoteEvent pos = do
    text <- Cmd.gets (Cmd.stateNoteText . Cmd.stateEdit)
    EditUtil.modifyEventAtTrigger pos False False $
        maybe (Just text, False) (\old -> (Just old, False))

getState :: Cmd.M m => (Cmd.EditState -> a) -> m a
getState f = Cmd.gets (f . Cmd.stateEdit)
