-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Support for high level score modifications.  This is companion to
    "Cmd.ModifyEvents", which is for low level transformations.

    The main interface to this is "Cmd.Repl.LNote".

    The score language is code to to be interpreted, not data to be
    manipulated.  This is good for flexibility, but bad for direct
    transformation.  Therefore, all the functions in here rely on a certain
    amount of conventional structure to tame the flexibility.

    The lowest level, represented by a list of 'Note's, assumes that each
    note track has a single \"branch\" of control tracks underneath it, and
    each note event has control values within its extent, so each note can be
    sliced out and treated as a unit.  So it doesn't support note tracks with
    multiple parallel children, and it doesn't support order-dependent control
    tracks, which means that relative controls are out too (TODO actually
    a relative control track is fine as long as there's only one).  Also,
    since notes only carry along the controls directly underneath them,
    they can wind up with different control values when they are placed on
    a different track (TODO it would be possible to deal with this too, by
    copying the events forward).

    I initially attempted to support trees of control tracks in full
    generality, or even just an ordered list of controls, but there's a problem
    when different Notes have different controls: where do the control tracks
    get merged into a tree, relative to each other?  Not only do I have to
    invent an order, but it has to be linear, since there's also no information
    to merge into a branching skeleton.  Since I can't create one with 'Note's,
    I felt Notes shouldn't be able to parse them either.

    The 'Note's can be annotated with additional data, such as pitch, but
    of course will make it more specialized and reliant on convention.  For
    instance, the pitches have to be extracted from the pitch events, which
    will fail unless there's an easily parseable pitch in there.

    TODO it should be possible to get the pitch out of the derivation by
    finding the corresponding Score.Event by looking for its stack.
-}
module Cmd.ModifyNotes where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector

import qualified Util.Lens as Lens
import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Trees as Trees

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Selection as Selection

import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | This represents a single event on a note track.
data Note = Note
    { noteStart :: !TrackTime
    , noteDuration :: !TrackTime
    , noteText :: !Text
    -- | This is the contents of the child tracks, where they overlap this
    -- Note's range.
    , noteControls :: !Controls
    , noteIndex :: !Index
    , noteControlTrackIds :: ![TrackId]
    } deriving (Eq, Show)

noteEnd :: Note -> TrackTime
noteEnd note = noteStart note + noteDuration note

start = Lens.lens noteStart (\r a -> r { noteStart = a })
duration = Lens.lens noteDuration (\r a -> r { noteDuration = a })
text = Lens.lens noteText (\r a -> r { noteText = a })
controls = Lens.lens noteControls (\r a -> r { noteControls = a })
index = Lens.lens noteIndex (\r a -> r { noteIndex = a })

end = Lens.lens noteEnd (\r a -> r { noteStart = a - noteDuration r })

noteMin :: Note -> TrackTime
noteMin n = min (noteStart n) (noteEnd n)

noteMax :: Note -> TrackTime
noteMax n = max (noteStart n) (noteEnd n)

noteOrientation :: Note -> Types.Orientation
noteOrientation = Event.orientationOf . noteDuration

-- | Each note has an Index, which indicates which of the selected note tracks
-- it came from, or should be written to.
type Index = Int

instance Pretty Note where
    format (Note start dur text controls index controlTrackIds) =
        Pretty.record title $
            (if text == mempty then [] else [("text", Pretty.format text)]) ++
            [ ("controls", Pretty.format controls)
            , ("index", Pretty.format index)
            , ("control_track_ids", Pretty.format controlTrackIds)
            ]
        where
        title = Pretty.text "Note" <> Pretty.format (start, dur)

notesOverlap :: Note -> Note -> Bool
notesOverlap n1 n2 =
    not $ noteMin n1 >= noteMax n2 || noteMax n1 <= noteMin n2

-- * controls

type Controls = Map Control Events.Events

-- | A simplified version of 'ParseTitle.ControlType', since Notes don't
-- support all the forms of control tracks.  Put Pitch first so it sorts first,
-- to support the convention of putting the pitch track right after the note
-- track.
data Control = Pitch Pitch.ScaleId | Control ScoreT.Control
    deriving (Eq, Ord, Show)

instance Pretty Control where
    pretty = controlToTitle

controlToTitle :: Control -> Text
controlToTitle control = case control of
    Control c -> ParseTitle.controlToTitle $ ScoreT.untyped c
    Pitch scaleId -> ParseTitle.scaleToTitle scaleId

type Error = Text

titleToControl :: Text -> Either Error Control
titleToControl title = ParseTitle.parseControlType title >>= \case
    ParseTitle.Control (Right (ScoreT.Typed ScoreT.Untyped c)) Nothing ->
        pure $ Control c
    ParseTitle.Pitch scaleId (Right pcontrol)
        | pcontrol == ScoreT.defaultPitch -> pure $ Pitch scaleId
    _ -> Left $ "complicated controls unsupported: " <> title

-- | Put the pitch tracks next to the note, the rest go in alphabetical order.
sortedControls :: Controls -> [(Control, Events.Events)]
sortedControls = Lists.sortOn (key . fst) . Map.toList
    where
    key c@(Pitch {}) = (0, c)
    key c@(Control {}) = (1, c)


-- * selection

-- | Modify notes.
type ModifyNotes m = BlockId -> [(Note, TrackId)] -> m [Note]

notes :: Monad m => ([Note] -> [Note]) -> ModifyNotes m
notes f _ = pure . f . map fst

-- | Modify a single note.
note :: Monad m => (Note -> Note) -> ModifyNotes m
note f _ = pure . map (f . fst)

-- | Modify notes on the selected tracks.  Only the top level note tracks are
-- affected, so you can select an entire block and not worry about mangling
-- parent controls.
--
-- This may add new tracks, but will not delete tracks that are made empty.
-- It could, but it seems easy enough to delete the tracks by hand once
-- I verify that the transformation worked.  TODO revisit this if it's annoying
selection :: Cmd.M m => ModifyNotes m -> m ()
selection modify = do
    oldNotes <- selectedNotes
    blockId <- Cmd.getFocusedBlock
    newNotes <- modify blockId oldNotes
    -- Clear selected events before merging in new ones.
    let ranges = removeRanges oldNotes
    forM_ ranges $ \(trackId, range) ->
        Ui.removeEventsRange trackId range
    writeTracks blockId (map fst ranges) (mergeNotes newNotes)

removeRanges :: [(Note, TrackId)] -> [(TrackId, Events.Range)]
removeRanges = concatMap range . Lists.groupSnd
    where
    range ([], _) = [] -- shouldn't happen, per Lists.groupSnd's postcondition
    range (notes@(note : _), trackId) =
        (trackId, Events.Range start end)
            : map (, Events.Range start end) (noteControlTrackIds note)
        where
        start = minimum $ map noteMin notes
        end = maximum $ map noteMax notes
        -- All Notes with the same TrackId should also have the same
        -- note_control_track_ids.

-- | Find the top-level note tracks in the selection, and reduce them down to
-- Notes.
selectedNotes :: Cmd.M m => m [(Note, TrackId)]
selectedNotes = do
    let isNote = fmap ParseTitle.isNoteTrack . Ui.getTrackTitle . fst
    sel <- filterM isNote =<< Selection.events
    tree <- TrackTree.trackTreeOf =<< Cmd.getFocusedBlock
    sliceTracks tree sel

-- ** annotated transformations

type Annotated a m = [(Note, a)] -> m [Note]

annotateNns :: Cmd.M m => Annotated (Maybe Pitch.NoteNumber) m
    -> ModifyNotes m
annotateNns modify = annotateControls (modify . map (second (eval <=< fst)))
    where eval = either (const Nothing) Just . PSignal.pitchNn

annotateControls :: Cmd.M m
    => Annotated (Maybe PSignal.Transposed, ScoreT.ControlValMap) m
    -> ModifyNotes m
annotateControls modify blockId noteTrackIds = do
    events <- Cmd.perfEvents <$> Cmd.getPerformance blockId
    modify $ findControls noteTrackIds events

-- | This finds the controls of each note by looking for its corresponding
-- event in the performance.  TODO matching by stack seems like it could be
-- inaccurate, and inefficient too.  Shouldn't I look up the signal directly
-- from the performance?
findControls :: [(Note, TrackId)] -> Vector.Vector Score.Event
    -> [(Note, (Maybe PSignal.Transposed, ScoreT.ControlValMap))]
findControls noteTrackIds events =
    zip (map fst noteTrackIds) $
        map (extract . convert events) noteTrackIds
    where
    convert events (note, trackId) = findEvent trackId note events
    extract Nothing = (Nothing, mempty)
    extract (Just event) =
        ( Score.initialPitch event
        , Score.eventControlsAt (Score.eventStart event) event
        )

findEvent :: TrackId -> Note -> Vector.Vector Score.Event -> Maybe Score.Event
findEvent trackId note = Vector.find $ \event ->
    stackMatches trackId (noteStart note) (noteEnd note) $
        Score.eventStack event

stackMatches :: TrackId -> TrackTime -> TrackTime -> Stack.Stack -> Bool
stackMatches trackId start end =
    findTrack . findRegion start end . Stack.innermost
    where
    findRegion start end = drop 1 . dropWhile (/= Stack.Region start end)
    -- Find the Track, but abort if I see a region or block
    findTrack frames = case frames of
        Stack.Track tid : _ -> trackId == tid
        Stack.Call {} : rest -> findTrack rest
        _ -> False

-- * read

sliceTracks :: Ui.M m => TrackTree.TrackTree -> [(TrackId, [Event.Event])]
    -> m [(Note, TrackId)]
sliceTracks tree = concatMapM slice . zip [0..]
    where
    slice :: Ui.M m => (Index, (TrackId, [Event.Event]))
        -> m [(Note, TrackId)]
    slice (index, (trackId, events)) =
        case Trees.find ((==trackId) . Ui.trackId) tree of
            Nothing -> pure []
            Just (Tree.Node _track subs) -> do
                subs <- mapM (traverse getEvents) subs
                notes <- Ui.requireRight id $
                    mapM (sliceNote index subs) events
                pure $ map (, trackId) notes
    getEvents track = (track,) <$> Ui.getEvents (Ui.trackId track)

-- | The whole thing fails if a title is unparseable or the control tracks have
-- a fork in the skeleton.
--
-- This is similar to 'Derive.Slice.slice' and I initially spent some time
-- trying to reuse it, but it's different enough that most of the work that
-- slice does doesn't apply here.
sliceNote :: Index -> [Tree.Tree (Ui.TrackInfo, Events.Events)]
    -> Event.Event -> Either Error Note
sliceNote index subs event = do
    controls <- extractControls (Event.range event) subs
    pure $ Note
        { noteStart = Event.start event
        , noteDuration = Event.duration event
        , noteText = Event.text event
        , noteControls = Map.fromList controls
        , noteIndex = index
        , noteControlTrackIds = map (Ui.trackId . fst) $
            concatMap Tree.flatten subs
        }

extractControls :: (TrackTime, TrackTime)
    -> [Tree.Tree (Ui.TrackInfo, Events.Events)]
    -> Either Error [(Control, Events.Events)]
extractControls range tracks = case tracks of
    [] -> pure []
    [Tree.Node (track, events) subs] -> do
        control <- annotate (showt (Ui.trackId track)) $
            titleToControl (Ui.trackTitle track)
        rest <- extractControls range subs
        pure $ (control, slice range events) : rest
    _ -> Left $ ">1 subtrack: "
        <> showt (map (Ui.trackId . fst . Tree.rootLabel) tracks)
    where
    slice (start, end) e = Events.inRange (Events.Range start end) e

annotate :: Text -> Either Error a -> Either Error a
annotate prefix = first ((prefix <> ": ") <>)

-- * write

data NoteTrack = NoteTrack Events.Events Controls
    deriving (Eq, Show)

instance Semigroup NoteTrack where
    NoteTrack events1 controls1 <> NoteTrack events2 controls2 =
        NoteTrack (events1 <> events2) (Maps.mappend controls1 controls2)
instance Monoid NoteTrack where
    mempty = NoteTrack mempty mempty
    mappend = (<>)

mergeNotes :: [Note] -> [NoteTrack]
mergeNotes = map makeTrack . Lists.groupSort noteIndex
    where
    makeTrack :: [Note] -> NoteTrack
    makeTrack = foldl' (<>) mempty . map noteTrack
    noteTrack note = NoteTrack (Events.singleton event) (noteControls note)
        where
        event = Event.event (noteStart note) (noteDuration note)
            (noteText note)

-- | Write NoteTracks to the given block.  It may create new tracks, but won't
-- delete ones that are made empty.
writeTracks :: Ui.M m => BlockId
    -> [TrackId] -- ^ The TrackIds are expected to line up with NoteTracks.
    -- If there are more NoteTracks than TrackIds, new tracks will be created.
    -> [NoteTrack] -> m ()
writeTracks blockId trackIds tracks = do
    oldTree <- extractNoteTrees blockId trackIds
    zipWithM_ merge oldTree tracks
    nextTracknum <- tracknumAfter blockId trackIds
    create nextTracknum (drop (length oldTree) tracks)
    where
    merge (Tree.Node track subs) (NoteTrack events controls) = do
        Ui.insertEvents (Ui.trackId track) (Events.ascending events)
        mergeControls blockId (Ui.trackId track) subs $
            sortedControls controls
    -- | Create new tracks.
    create _ [] = pure ()
    create tracknum (NoteTrack events controls : rest)
        | Events.null events = create tracknum rest
        | otherwise = do
            let tracks = (">", events)
                    : map (first controlToTitle) (sortedControls controls)
            forM_ (zip [tracknum..] tracks) $ \(n, (title, events)) ->
                Create.track blockId n title events
            Ui.addEdges blockId $ take (length tracks - 1) $
                zip [tracknum..] [tracknum+1..]
            parent <- maybe (pure Nothing) (parentOf blockId)
                (Lists.head trackIds)
            whenJust parent $ \p ->
                Ui.addEdges blockId [(Ui.trackTracknum p, tracknum)]
            create (tracknum + length tracks) rest

extractNoteTrees :: Ui.M m => BlockId -> [TrackId] -> m TrackTree.TrackTree
extractNoteTrees blockId trackIds =
    Trees.findAll (wantedTrack (Set.fromList trackIds)) <$>
        TrackTree.trackTreeOf blockId
    where
    -- | Accept the top level note tracks.
    wantedTrack trackIds track =
        ParseTitle.isNoteTrack (Ui.trackTitle track)
        && Ui.trackId track `Set.member` trackIds

mergeControls :: Ui.M m => BlockId -> TrackId -> TrackTree.TrackTree
    -> [(Control, Events.Events)] -> m ()
mergeControls blockId noteTrackId tree controls = do
    -- Don't use Ui.track_tracknum because it will be out of date if
    -- an earlier merge inserted a new track.
    nextTracknum <- tracknumAfter blockId $
        noteTrackId : map Ui.trackId tracks
    go nextTracknum controls
    where
    go _ [] = pure ()
    go tracknum ((control, events) : controls) = case find control of
        Just track -> do
            Ui.insertEvents (Ui.trackId track) (Events.ascending events)
            go tracknum controls
        Nothing -> do
            Create.track blockId tracknum (controlToTitle control) events
            -- Link the new track into the skeleton below the bottom control.
            parent <- bottomTrack blockId noteTrackId
            whenJust parent $ \p ->
                Ui.addEdges blockId [(Ui.trackTracknum p, tracknum)]
            go (tracknum+1) controls
    find control =
        List.find ((== controlToTitle control) . Ui.trackTitle) tracks
    tracks = concatMap Tree.flatten tree

-- | Get the tracknum after the given tracks.
tracknumAfter :: Ui.M m => BlockId -> [TrackId] -> m TrackNum
tracknumAfter blockId trackIds = do
    tracknums <- mapM (Ui.getTracknumOf blockId) trackIds
    maybe (Ui.trackCount blockId) (pure . (+1)) (Lists.maximum tracknums)

-- | Get the bottom track below the given TrackId.  If there are more than one,
-- pick the one with the highest TrackNum.
bottomTrack :: Ui.M m => BlockId -> TrackId -> m (Maybe Ui.TrackInfo)
bottomTrack blockId trackId = do
    tree <- TrackTree.trackTreeOf blockId
    pure $ Lists.maximumOn Ui.trackTracknum . Trees.leaves
        =<< Trees.find ((==trackId) . Ui.trackId) tree

parentOf :: Ui.M m => BlockId -> TrackId -> m (Maybe Ui.TrackInfo)
parentOf blockId trackId = do
    tree <- TrackTree.trackTreeOf blockId
    pure $ Lists.head
        [ track
        | (track, _, children) <- Trees.flatPaths tree
        , trackId `elem` map Ui.trackId children
        ]
