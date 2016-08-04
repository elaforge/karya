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
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Tree

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Selection as Selection

import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import Global
import Types


-- | This represents a single event on a note track.
data Note = Note {
    note_start :: !TrackTime
    , note_duration :: !TrackTime
    , note_orientation :: !Event.Orientation
    , note_text :: !Text
    -- | This is the contents of the child tracks, where they overlap this
    -- Note's range.
    , note_controls :: !Controls
    , note_index :: !Index
    , note_control_track_ids :: ![TrackId]
    } deriving (Eq, Show)

note_end :: Note -> TrackTime
note_end note = note_start note + note_duration note

start = Lens.lens note_start
    (\f r -> r { note_start = f (note_start r) })
duration = Lens.lens note_duration
    (\f r -> r { note_duration = f (note_duration r) })
orientation = Lens.lens note_orientation
    (\f r -> r { note_orientation = f (note_orientation r) })
text = Lens.lens note_text
    (\f r -> r { note_text = f (note_text r) })
controls = Lens.lens note_controls
    (\f r -> r { note_controls = f (note_controls r) })
index = Lens.lens note_index
    (\f r -> r { note_index = f (note_index r) })

end = Lens.lens note_end
    (\f r -> r { note_start = f (note_end r) - note_duration r })

-- | Each note has an Index, which indicates which of the selected note tracks
-- it came from, or should be written to.
type Index = Int

instance Pretty.Pretty Note where
    format (Note start dur orientation text controls index control_track_ids) =
        Pretty.record title $
            (if text == mempty then [] else [("text", Pretty.format text)]) ++
            [ ("controls", Pretty.format controls)
            , ("index", Pretty.format index)
            , ("control_track_ids", Pretty.format control_track_ids)
            ]
        where
        title = Pretty.text "Note" <> minus <> Pretty.format (start, dur)
        minus = case orientation of
            Event.Positive -> mempty
            Event.Negative -> "-"

notes_overlap :: Note -> Note -> Bool
notes_overlap n1 n2 =
    not $ note_start n1 >= note_end n2 || note_end n1 <= note_start n2

-- * controls

type Controls = Map.Map Control Events.Events
-- | A simplified version of 'ParseTitle.ControlType', since Notes don't
-- support all the forms of control tracks.
data Control = Control Score.Control | Pitch Pitch.ScaleId
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Control where
    pretty = control_to_title

control_to_title :: Control -> Text
control_to_title control = ParseTitle.unparse_control $ case control of
    Control c -> ParseTitle.Control Nothing (Score.untyped c)
    Pitch scale_id -> ParseTitle.Pitch Nothing scale_id Score.default_pitch

type Error = Text

title_to_control :: Text -> Either Error Control
title_to_control title = ParseTitle.parse_control title >>= \x -> case x of
    ParseTitle.Control Nothing (Score.Typed Score.Untyped c) ->
        return $ Control c
    ParseTitle.Pitch Nothing scale_id pcontrol
        | pcontrol == Score.default_pitch -> return $ Pitch scale_id
    _ -> Left $ "complicated controls unsupported: " <> title

-- | Put the pitch tracks next to the note, the rest go in alphabetical order.
sorted_controls :: Controls -> [(Control, Events.Events)]
sorted_controls = Seq.sort_on (key . fst) . Map.toList
    where
    key c@(Pitch {}) = (0, c)
    key c@(Control {}) = (1, c)


-- * selection

-- | Modify notes.
type ModifyNotes m = BlockId -> [(Note, TrackId)] -> m [Note]

notes :: Monad m => ([Note] -> [Note]) -> ModifyNotes m
notes f _ = return . f . map fst

-- | Modify a single note.
note :: Monad m => (Note -> Note) -> ModifyNotes m
note f _ = return . map (f . fst)

-- | Modify notes on the selected tracks.  Only the top level note tracks are
-- affected, so you can select an entire block and not worry about mangling
-- parent controls.
--
-- This may add new tracks, but will not delete tracks that are made empty.
-- It could, but it seems easy enough to delete the tracks by hand once
-- I verify that the transformation worked.  TODO revisit this if it's annoying
selection :: Cmd.M m => ModifyNotes m -> m ()
selection modify = do
    old_notes <- selected_notes
    block_id <- Cmd.get_focused_block
    new_notes <- mapM verify =<< modify block_id old_notes
    -- Clear selected events before merging in new ones.
    let ranges = remove_ranges old_notes
    forM_ ranges $ \(track_id, range) ->
        State.remove_event_range track_id range
    let track_ids = map fst ranges
    write_tracks block_id track_ids (merge_notes new_notes)

remove_ranges :: [(Note, TrackId)] -> [(TrackId, Events.Range)]
remove_ranges = concatMap range . Seq.group_snd
    where
    range ([], _) = [] -- shouldn't happen, per Seq.group_snd's postcondition
    range (notes@(note : _), track_id) =
        (track_id, Events.Inclusive start (maximum (map note_start notes)))
        : map (, range) (note_control_track_ids note)
        where
        start = minimum (map note_start notes)
        end = maximum (map note_end notes)
        -- All Notes with the same TrackId should also have the same
        -- note_control_track_ids.
        range = Events.range (note_orientation note) start end

-- | Find the top-level note tracks in the selection, and reduce them down to
-- Notes.
selected_notes :: Cmd.M m => m [(Note, TrackId)]
selected_notes = do
    let is_note = fmap ParseTitle.is_note_track . State.get_track_title . fst
    sel <- filterM is_note =<< Selection.events
    tree <- TrackTree.track_tree_of =<< Cmd.get_focused_block
    slice_tracks tree sel

verify :: Cmd.M m => Note -> m Note
verify note
    | note_duration note < 0 = Cmd.throw $ "duration <0: " <> pretty note
    | otherwise = return note

-- ** annotated transformations

type Annotated a m = [(Note, a)] -> m [Note]

annotate_nns :: Cmd.M m => Annotated (Maybe Pitch.NoteNumber) m
    -> ModifyNotes m
annotate_nns modify = annotate_controls (modify . map (second (eval <=< fst)))
    where eval = either (const Nothing) Just . PSignal.pitch_nn

annotate_controls :: Cmd.M m =>
    Annotated (Maybe PSignal.Transposed, Score.ControlValMap) m
    -> ModifyNotes m
annotate_controls modify block_id note_track_ids = do
    events <- Cmd.perf_events <$> Cmd.get_performance block_id
    modify $ find_controls note_track_ids events

-- | This finds the controls of each note by looking for its corresponding
-- event in the performance.  TODO matching by stack seems like it could be
-- inaccurate, and inefficient too.  Shouldn't I look up the signal directly
-- from the performance?
find_controls :: [(Note, TrackId)] -> Vector.Vector Score.Event
    -> [(Note, (Maybe PSignal.Transposed, Score.ControlValMap))]
find_controls note_track_ids events =
    zip (map fst note_track_ids) $
        map (extract . convert events) note_track_ids
    where
    convert events (note, track_id) = find_event track_id note events
    extract Nothing = (Nothing, mempty)
    extract (Just event) = (Score.initial_pitch event,
        Score.event_controls_at (Score.event_start event) event)

find_event :: TrackId -> Note -> Vector.Vector Score.Event -> Maybe Score.Event
find_event track_id note = Vector.find $ \event ->
    stack_matches track_id (note_start note) (note_end note) $
        Score.event_stack event

stack_matches :: TrackId -> TrackTime -> TrackTime -> Stack.Stack -> Bool
stack_matches track_id start end =
    find_track . find_region start end . Stack.innermost
    where
    find_region start end = drop 1 . dropWhile (/= Stack.Region start end)
    -- Find the Track, but abort if I see a region or block
    find_track frames = case frames of
        Stack.Track tid : _ -> track_id == tid
        Stack.Call {} : rest -> find_track rest
        _ -> False

-- * read

slice_tracks :: State.M m => TrackTree.TrackTree -> [(TrackId, [Event.Event])]
    -> m [(Note, TrackId)]
slice_tracks tree = concatMapM slice . zip [0..]
    where
    slice :: State.M m => (Index, (TrackId, [Event.Event]))
        -> m [(Note, TrackId)]
    slice (index, (track_id, events)) =
        case Util.Tree.find ((==track_id) . State.track_id) tree of
            Nothing -> return []
            Just (Tree.Node _track subs) -> do
                subs <- mapM (traverse get_events) subs
                notes <- State.require_right id $
                    mapM (slice_note index subs) events
                return $ map (, track_id) notes
    get_events track = (track,) <$> State.get_events (State.track_id track)

-- | The whole thing fails if a title is unparseable or the control tracks have
-- a fork in the skeleton.
--
-- This is similar to 'Derive.Slice.slice' and I initially spent some time
-- trying to reuse it, but it's different enough that most of the work that
-- slice does doesn't apply here.
slice_note :: Index -> [Tree.Tree (State.TrackInfo, Events.Events)]
    -> Event.Event -> Either Error Note
slice_note index subs event = do
    controls <- extract_controls (Event.orientation event)
        (Event.range event) subs
    return $ Note
        { note_start = Event.start event
        , note_duration = Event.duration event
        , note_orientation = Event.orientation event
        , note_text = Event.text event
        , note_controls = Map.fromList controls
        , note_index = index
        , note_control_track_ids = map (State.track_id . fst) $
            concatMap Tree.flatten subs
        }

extract_controls :: Event.Orientation -> (TrackTime, TrackTime)
    -> [Tree.Tree (State.TrackInfo, Events.Events)]
    -> Either Error [(Control, Events.Events)]
extract_controls orientation range tracks = case tracks of
    [] -> return []
    [Tree.Node (track, events) subs] -> do
        control <- annotate (showt (State.track_id track)) $
            title_to_control (State.track_title track)
        rest <- extract_controls orientation range subs
        return $ (control, slice range events) : rest
    _ -> Left $ ">1 subtrack: "
        <> showt (map (State.track_id . fst . Tree.rootLabel) tracks)
    where
    slice (start, end) = Events.from_list . extract . Events.at_after start
        where
        extract = case orientation of
            Event.Positive -> takeWhile ((<end) . Event.start)
            Event.Negative -> takeWhile ((<=end) . Event.start)
                . dropWhile ((<=start) . Event.start)

annotate :: Text -> Either Error a -> Either Error a
annotate prefix = first ((prefix <> ": ") <>)

-- * write

data NoteTrack = NoteTrack Events.Events Controls
    deriving (Eq, Show)

instance Monoid NoteTrack where
    mempty = NoteTrack mempty mempty
    mappend (NoteTrack events1 controls1) (NoteTrack events2 controls2) =
        NoteTrack (events1 <> events2) (Map.mappend controls1 controls2)

merge_notes :: [Note] -> [NoteTrack]
merge_notes = map make_track . Seq.group_sort note_index
    where
    make_track :: [Note] -> NoteTrack
    make_track = List.foldl' (<>) mempty . map note_track
    note_track note = NoteTrack (Events.singleton event) (note_controls note)
        where
        event = Event.set_orientation (note_orientation note) $
            Event.event (note_start note) (note_duration note) (note_text note)

-- | Write NoteTracks to the given block.  It may create new tracks, but won't
-- delete ones that are made empty.
write_tracks :: State.M m => BlockId
    -> [TrackId] -- ^ The TrackIds are expected to line up with NoteTracks.
    -- If there are more NoteTracks than TrackIds, new tracks will be created.
    -> [NoteTrack] -> m ()
write_tracks block_id track_ids tracks = do
    old_tree <- extract_note_trees block_id track_ids
    zipWithM_ merge old_tree tracks
    next_tracknum <- tracknum_after block_id track_ids
    create next_tracknum (drop (length old_tree) tracks)
    where
    merge (Tree.Node track subs) (NoteTrack events controls) = do
        State.insert_events (State.track_id track) (Events.ascending events)
        merge_controls block_id (State.track_id track) subs $
            sorted_controls controls
    -- | Create new tracks.
    create _ [] = return ()
    create tracknum (NoteTrack events controls : rest)
        | Events.null events = create tracknum rest
        | otherwise = do
            let tracks = (">", events)
                    : map (first control_to_title) (sorted_controls controls)
            forM_ (zip [tracknum..] tracks) $ \(n, (title, events)) ->
                Create.track block_id n title events
            State.add_edges block_id $ take (length tracks - 1) $
                zip [tracknum..] [tracknum+1..]
            parent <- maybe (return Nothing) (parent_of block_id)
                (Seq.head track_ids)
            whenJust parent $ \p ->
                State.add_edges block_id [(State.track_tracknum p, tracknum)]
            create (tracknum + length tracks) rest

extract_note_trees :: State.M m => BlockId -> [TrackId]
    -> m TrackTree.TrackTree
extract_note_trees block_id track_ids =
    Util.Tree.filter (wanted_track (Set.fromList track_ids)) <$>
        TrackTree.track_tree_of block_id
    where
    -- | Accept the top level note tracks.
    wanted_track track_ids track =
        ParseTitle.is_note_track (State.track_title track)
        && State.track_id track `Set.member` track_ids

merge_controls :: State.M m => BlockId -> TrackId -> TrackTree.TrackTree
    -> [(Control, Events.Events)] -> m ()
merge_controls block_id note_track_id tree controls = do
    -- Don't use State.track_tracknum because it will be out of date if
    -- an earlier merge inserted a new track.
    next_tracknum <- tracknum_after block_id $
        note_track_id : map State.track_id tracks
    go next_tracknum controls
    where
    go _ [] = return ()
    go tracknum ((control, events) : controls) = case find control of
        Just track -> do
            State.insert_events (State.track_id track)
                (Events.ascending events)
            go tracknum controls
        Nothing -> do
            Create.track block_id tracknum (control_to_title control) events
            -- Link the new track into the skeleton below the bottom control.
            parent <- bottom_track block_id note_track_id
            whenJust parent $ \p ->
                State.add_edges block_id [(State.track_tracknum p, tracknum)]
            go (tracknum+1) controls
    find control =
        List.find ((== control_to_title control) . State.track_title) tracks
    tracks = concatMap Tree.flatten tree

-- | Get the tracknum after the given tracks.
tracknum_after :: State.M m => BlockId -> [TrackId] -> m TrackNum
tracknum_after block_id track_ids = do
    tracknums <- mapM (State.get_tracknum_of block_id) track_ids
    maybe (State.track_count block_id) (return . (+1)) (Seq.maximum tracknums)

-- | Get the bottom track below the given TrackId.  If there are more than one,
-- pick the one with the highest TrackNum.
bottom_track :: State.M m => BlockId -> TrackId -> m (Maybe State.TrackInfo)
bottom_track block_id track_id = do
    tree <- TrackTree.track_tree_of block_id
    return $ Seq.maximum_on State.track_tracknum . Util.Tree.leaves
        =<< Util.Tree.find ((==track_id) . State.track_id) tree

parent_of :: State.M m => BlockId -> TrackId -> m (Maybe State.TrackInfo)
parent_of block_id track_id = do
    tree <- TrackTree.track_tree_of block_id
    return $ Seq.head
        [ track
        | (track, _, children) <- Util.Tree.flat_paths tree
        , track_id `elem` map State.track_id children
        ]
