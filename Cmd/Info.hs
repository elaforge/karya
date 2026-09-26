-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Functions to get higher level information about blocks and tracks.

    This builds on "Derive.ParseTitle" but the Derive module only has functions
    needed by derivation, and doesn't run in the State monad.
-}
module Cmd.Info where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Text.Printf as Printf

import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty
import qualified Util.Trees as Trees

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Midi.Patch as Patch
import qualified Ui.Block as Block
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- * track info

data Track = Track
    { trackInfo :: Ui.TrackInfo
    , trackType :: TrackType
    } deriving (Show, Eq)

data TrackType
    -- | A note track has a list of control tracks, and a list of child note
    -- tracks.  The controls start with the children and continues with its
    -- parents.  However, it stops at another note track or as soon as a parent
    -- has more than one child, because that control track doesn't belong to
    -- just this note track.
    = Note [Ui.TrackInfo] [Ui.TrackInfo]
    -- | The note track for this pitch track.
    | Pitch (Maybe Ui.TrackInfo)
    -- | Tracks this control track has scope over.  This means all its
    -- children, but because of inversion, also a parent note track, if there
    -- is one.
    | Control [Ui.TrackInfo]
    deriving (Show, Eq)

instance Pretty TrackType where
    format = \case
        Note controls children -> Pretty.record "Note"
            [ ("controls", Pretty.format controls)
            , ("children", Pretty.format children)
            ]
        Pitch tinfo -> "Pitch" Pretty.<+> Pretty.format tinfo
        Control children -> "Control" Pretty.<+> Pretty.format children

getTrackType :: Ui.M m => BlockId -> TrackNum -> m Track
getTrackType blockId tracknum = Ui.require
    ("get_track_type: bad tracknum: " <> showt (blockId, tracknum))
        =<< lookupTrackType blockId tracknum

lookupTrackType :: Ui.M m => BlockId -> TrackNum -> m (Maybe Track)
lookupTrackType blockId tracknum = do
    trackTree <- TrackTree.trackTreeOf blockId
    pure $ makeTrack <$>
        Trees.findWithParents ((==tracknum) . Ui.trackTracknum) trackTree

-- | Get all the Tracks in a block, sorted by tracknum.
blockTracks :: Ui.M m => BlockId -> m [Track]
blockTracks blockId = Lists.sortOn (Ui.trackTracknum . trackInfo)
    . map makeTrack . Trees.paths <$> TrackTree.trackTreeOf blockId

makeTrack :: (Tree.Tree Ui.TrackInfo, TrackTree.TrackTree) -> Track
makeTrack (tree, parents) =
    Track (Tree.rootLabel tree) (trackTypeOf (tree, parents))

trackTypeOf :: (Tree.Tree Ui.TrackInfo, TrackTree.TrackTree) -> TrackType
trackTypeOf (Tree.Node track subs, parents)
    | ParseTitle.isNoteTrack title = Note
        (takeWhile isControl children ++ takeWhile isControl
            (map Tree.rootLabel (takeWhile isSingle parents)))
        (filter isNote children)
    | ParseTitle.isPitchTrack title =
        Pitch $ List.find isNote (children ++ map Tree.rootLabel parents)
    | otherwise = Control $ children
        -- If there is a note track above assume it will invert itself below
        -- the control stack.
        ++ maybe [] (:[]) (List.find isNote (map Tree.rootLabel parents))
    where
    children = concatMap Tree.flatten subs
    isSingle (Tree.Node _ [_]) = True
    isSingle _ = False
    isControl track =
        ParseTitle.isControlTrack t && not (ParseTitle.isTempoTrack t)
        where t = Ui.trackTitle track
    title = Ui.trackTitle track
    isNote = ParseTitle.isNoteTrack . Ui.trackTitle

-- ** specialized lookups

-- | Pitch track of a note track, if any.
pitchOfNote :: Ui.M m => BlockId -> TrackNum -> m (Maybe Ui.TrackInfo)
pitchOfNote blockId tracknum = do
    maybeTrack <- lookupTrackType blockId tracknum
    pure $ case maybeTrack of
        Just (Track _ (Note controls _)) ->
            List.find (ParseTitle.isPitchTrack . Ui.trackTitle) controls
        _ -> Nothing

-- | Note track of a pitch track, if any.
noteOfPitch :: Ui.M m => BlockId -> TrackNum -> m (Maybe Ui.TrackInfo)
noteOfPitch blockId tracknum = do
    maybeTrack <- lookupTrackType blockId tracknum
    pure $ case maybeTrack of
        Just (Track _ (Pitch note)) -> note
        _ -> Nothing

-- | True if this has any note track children.  It should be the same as
-- 'get_track_type' then match Track _ children | not (null children),
-- but more efficient.
hasNoteChildren :: Ui.M m => BlockId -> TrackNum -> m Bool
hasNoteChildren blockId tracknum = do
    skel <- Ui.getSkeleton blockId
    go skel tracknum
    where
    go skel tracknum = do
        let cs = Skeleton.children skel tracknum
        orM $ map isNote cs ++ map (go skel) cs
    isNote tnum = ParseTitle.isNoteTrack <$>
        (Ui.getTrackTitle =<< Ui.getEventTrackAt blockId tnum)


-- * misc

-- | Get the instrument of a track, or fail if it's not a note track.  This is
-- different than 'Perf.lookup_instrument' because it looks at the track title
-- first.  This is useful for new tracks which don't have a performance yet.
-- But if the track title doesn't specify an instrument it falls back on
-- 'Perf.lookup_instrument'.
getInstrumentOf :: Cmd.M m => BlockId -> TrackNum -> m ScoreT.Instrument
getInstrumentOf blockId tracknum =
    Ui.require ("get_instrument_of expected a note track: "
            <> showt (blockId, tracknum))
        =<< lookupInstrumentOf blockId tracknum

lookupInstrumentOf :: Cmd.M m => BlockId -> TrackNum
    -> m (Maybe ScoreT.Instrument)
lookupInstrumentOf blockId tracknum = do
    trackId <- Ui.getEventTrackAt blockId tracknum
    track <- Ui.getTrack trackId
    case ParseTitle.titleToInstrument (Track.trackTitle track) of
        Nothing -> Perf.lookupInstrument (blockId, Just trackId)
        Just inst -> Just <$> getDefaultInstrument blockId trackId inst

-- | If the instrument is 'ScoreT.empty_instrument', look up what it really is
-- in the performance.
getDefaultInstrument :: Cmd.M m => BlockId -> TrackId
    -> ScoreT.Instrument -> m ScoreT.Instrument
getDefaultInstrument blockId trackId inst
    | inst == ScoreT.emptyInstrument =
        fromMaybe inst <$> Perf.lookupInstrument (blockId, Just trackId)
    | otherwise = pure inst


-- * inst info

-- | Looks like: "wdev1 [1..3]; wdev2 [1,5]"
--
-- This adds 1 so MIDI channels are 1-based.
showAddrs :: [Patch.Addr] -> Text
showAddrs addrs = semicolonList
    [ pretty wdev
        <> " [" <> Text.intercalate "," (showRuns (map ((+1) . snd) addrs))
        <> "]"
    | (wdev, addrs) <- Lists.keyedGroupSort fst addrs
    ]

semicolonList :: [Text] -> Text
semicolonList [] = "[]"
semicolonList xs = Text.intercalate "; " xs

showRuns :: (Show a, Num a, Ord a) => [a] -> [Text]
showRuns = concatMap showRun . Lists.splitBetween (\a b -> a+1 < b)
    where
    showRun xs@(_:_:_:_) = [showt (head xs) <> ".." <> showt (last xs)]
    showRun xs = map showt xs


-- * set_instrument_status

-- | Stick some handy info about the current instrument into the status.
--
-- This should be run whenever the track focus changes, or tracks are expanded
-- or collapsed.
setInstrumentStatus :: Cmd.M m => BlockId -> TrackNum -> m ()
setInstrumentStatus blockId tracknum = do
    status <- getTrackStatus blockId tracknum
    whenJust status $ Cmd.setGlobalStatus "inst"

-- | Looks like:
-- title (tracknum): inst_name, allocation, [control tracks]
-- fm8/inst1 at 1: fm8:0,1,2, [vel {collapse 2}, pedal {expand 3}]
getTrackStatus :: Cmd.M m => BlockId -> TrackNum -> m (Maybe Text)
getTrackStatus blockId tracknum = do
    tree <- TrackTree.trackTreeOf blockId
    case findNoteTrack tree tracknum of
        Just (track, inst) -> do
            inst <- getDefaultInstrument blockId (Ui.trackId track) inst
            Just <$> status blockId tree (Ui.trackTracknum track) inst
        Nothing -> pure Nothing
    where
    status blockId tree noteTracknum inst = do
        let controls = controlTracksOf tree noteTracknum
        trackDescs <- showTrackStatus blockId controls
        alloc <- Ui.allocation inst <#> Ui.get
        let addrs = case UiConfig.allocBackend <$> alloc of
                Just (UiConfig.Midi config) -> Patch.configAddrs config
                _ -> []
        let title = ParseTitle.instrumentToTitle inst
        pure $ txt $ Printf.printf "%s at %d: %s -- [%s]" (untxt title)
            noteTracknum (untxt (showAddrs addrs))
            (Lists.join ", " trackDescs)

-- | Given a tracknum, find the note track associated with it.  Since there
-- may be multiple ones, pick the first one.  First try children, then
-- parents.
findNoteTrack :: TrackTree.TrackTree -> TrackNum
    -> Maybe (Ui.TrackInfo, ScoreT.Instrument)
findNoteTrack tree tracknum = case pathsOf tree tracknum of
        Nothing -> Nothing
        Just (track, parents, children) ->
            Lists.head $ mapMaybe instOf (track : children ++ parents)
    where
    instOf track =
        case ParseTitle.titleToInstrument (Ui.trackTitle track) of
            Nothing -> Nothing
            Just inst -> Just (track, inst)

-- | Get the controls associated with the given track.  This means all
-- children until the next note track, and all parents with only one child
-- until the next note track.  Parents with multiple children are not
-- associated with a single track, so they're omitted.  Tempo tracks are always
-- omitted.
controlTracksOf :: TrackTree.TrackTree -> TrackNum -> [Ui.TrackInfo]
controlTracksOf tree tracknum =
    case Trees.findWithParents ((==tracknum) . Ui.trackTracknum) tree of
        Nothing -> []
        Just (Tree.Node _ children, parents) ->
            takeWhile isControl (concatMap Tree.flatten children)
            ++ takeWhile isControl (map Tree.rootLabel
                (takeWhile isSingle parents))
    where
    isSingle (Tree.Node _ [_]) = True
    isSingle _ = False
    isControl track =
        ParseTitle.isControlTrack t && not (ParseTitle.isTempoTrack t)
        where t = Ui.trackTitle track

-- | Looks like: [vel {collapse 2}, pedal {expand 3}]
showTrackStatus :: Ui.M m => BlockId -> [Ui.TrackInfo] -> m [String]
showTrackStatus blockId status = forM status $ \info -> do
    let tracknum = Ui.trackTracknum info
    btrack <- Ui.blockTrackAt blockId tracknum
    let cmdText :: String
        cmdText = case fmap Block.trackFlags btrack of
            Nothing -> "?"
            Just flags
                | Block.Collapse `Set.member` flags -> "expand"
                | otherwise -> "collapse"
    pure $ Printf.printf "%s {%s %d}"
        (untxt $ ParseTitle.stripExpr $ Ui.trackTitle info)
        cmdText tracknum

pathsOf :: TrackTree.TrackTree -> TrackNum
    -> Maybe (Ui.TrackInfo, [Ui.TrackInfo], [Ui.TrackInfo])
    -- ^ (track, parents, children)
pathsOf trackTree tracknum =
    List.find ((==tracknum) . Ui.trackTracknum . (\(a, _, _) -> a))
        (Trees.flatPaths trackTree)

