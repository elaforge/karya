-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NamedFieldPuns #-}
-- | Functions for larger scale transformations on a State.
module Ui.Transform where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Logger as Logger
import qualified Util.Maps as Maps
import qualified Util.Memory as Memory
import qualified Util.Num as Num

import qualified Derive.ShowVal as ShowVal
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- | Transform TracklikeIds.
tracks :: Ui.M m => BlockId -> (Block.TracklikeId -> Block.TracklikeId) -> m ()
tracks blockId f = do
    block <- modify <$> Ui.getBlock blockId
    Ui.modify $ \st -> st
        { Ui.stateBlocks = Map.insert blockId block (Ui.stateBlocks st) }
    where
    modify block = block
        { Block.blockTracks = map modifyTrack (Block.blockTracks block) }
    modifyTrack t = t { Block.tracklikeId = f (Block.tracklikeId t) }

-- * map IDs

-- | Map a function across the IDs in the given state.  Any collisions are
-- thrown in Left.
mapStateIds :: (Id.Id -> Id.Id) -> Ui.State -> Either Ui.Error Ui.State
mapStateIds f state = Ui.exec state (mapIds f)

-- | Transform IDs, but don't update view_id pointer map.  So only use this
-- when you are sure there are no visible views (\"invisible\" views occur
-- after they are created but before the sync).  This should probably only be
-- used by 'map_state_ids'.
mapIds :: Ui.M m => (Id.Id -> Id.Id) -> m ()
mapIds f = do
    mapViewIds f
    mapBlockIds f
    mapTrackIds f
    mapRulerIds f

mapNamespace :: Ui.M m => (Id.Namespace -> Id.Namespace) -> m ()
mapNamespace modify = mapIds set
    where set ident = Id.setNamespace (modify (Id.idNamespace ident)) ident

mapViewIds :: Ui.M m => (Id.Id -> Id.Id) -> m ()
mapViewIds f = do
    views <- Ui.gets Ui.stateViews
    newViews <- safeMapKeys "state_views" (Id.modify f) views
    Ui.modify $ \st -> st { Ui.stateViews = newViews }

-- | Rename a BlockId.  Views are updated to point to the new block.
mapBlockIds :: Ui.M m => (Id.Id -> Id.Id) -> m ()
mapBlockIds f = do
    maybeRoot <- Ui.lookupRootId
    let newRoot = fmap (Id.BlockId . f . Id.unpackId) maybeRoot
    blocks <- Ui.gets Ui.stateBlocks
    newBlocks <- safeMapKeys "state_blocks" modify blocks
    views <- Ui.gets Ui.stateViews
    let newViews = Map.map
            (\v -> v { Block.viewBlock = modify $ Block.viewBlock v })
            views
    Ui.modify $ \st -> st
        { Ui.stateBlocks = mapBlock <$> newBlocks
        , Ui.stateViews = newViews
        }
    Ui.modifyConfig $ \config -> config { UiConfig.configRoot = newRoot }
    where
    mapBlock b = b
        { Block.blockIntegrated =
            fmap (first modify) (Block.blockIntegrated b)
        }
    modify = Id.modify f

mapTrackIds :: Ui.M m => (Id.Id -> Id.Id) -> m ()
mapTrackIds f = do
    tracks <- Ui.gets Ui.stateTracks
    newTracks <- safeMapKeys "state_tracks" modify tracks
    blocks <- Ui.gets Ui.stateBlocks
    let newBlocks = Map.map mapBlock blocks
    Ui.modify $ \st -> st
        { Ui.stateTracks = newTracks, Ui.stateBlocks = newBlocks }
    where
    mapBlock b = b
        { Block.blockTracks =
            map (mapTrack modify) (Block.blockTracks b)
        , Block.blockIntegrated =
            fmap (fmap mapTrackDests) (Block.blockIntegrated b)
        , Block.blockIntegratedTracks =
            map (\(tid, dests) -> (modify tid, mapTrackDests dests))
                (Block.blockIntegratedTracks b)
        , Block.blockIntegratedManual =
            fmap (fmap mapNoteDest) (Block.blockIntegratedManual b)
        }
    -- This is getting complicated with integrate.  A generic map like uniplate
    -- would be able to do this automatically.
    mapTrack f = mapMerged f . mapTrackId f
    mapTrackId f track = case Block.tracklikeId track of
        Block.TId tid rid ->
            Block.modifyId (const (Block.TId (f tid) rid)) track
        _ -> track
    mapMerged f track = track
        { Block.trackMerged = Set.map f (Block.trackMerged track) }
    mapTrackDests = \case
        Block.DeriveDestinations dests ->
            Block.DeriveDestinations $ map mapNoteDest dests
        Block.ScoreDestinations dests ->
            Block.ScoreDestinations $ map (bimap modify (first modify)) dests
    mapNoteDest (Block.NoteDestination key note controls) =
        Block.NoteDestination
            { destKey = key
            , destNote = first modify note
            , destControls = fmap (first modify) controls
            }
    modify = Id.modify f

mapRulerIds :: Ui.M m => (Id.Id -> Id.Id) -> m ()
mapRulerIds f = do
    rulers <- Ui.gets Ui.stateRulers
    newRulers <- safeMapKeys "state_rulers" (Id.modify f) rulers

    blocks <- Ui.gets Ui.stateBlocks
    let newBlocks = Map.map
            (\b -> b { Block.blockTracks =
                map (mapTrack (Id.modify f)) (Block.blockTracks b) })
            blocks
    Ui.modify $ \st ->
        st { Ui.stateRulers = newRulers, Ui.stateBlocks = newBlocks }
    where
    mapTrack f track = case Block.tracklikeId track of
        Block.TId tid rid ->
            Block.modifyId (const (Block.TId tid (f rid))) track
        Block.RId rid -> Block.modifyId (const (Block.RId (f rid))) track
        _ -> track

safeMapKeys :: (Ui.M m, Ord k, Show k) =>
    String -> (k -> k) -> Map k v -> m (Map k v)
safeMapKeys name f fm0
    | Map.size fm1 == Map.size fm0 = pure fm1
    | otherwise = Ui.throw $ "keys collided in " <> showt name <> ": "
        <> showt (Map.keys (Map.difference fm0 fm1))
    where fm1 = Map.mapKeys f fm0

-- * namespace

-- | Destroy all views, blocks, tracks, and rulers with the given namespace.
destroyNamespace :: Ui.M m => Id.Namespace -> m ()
destroyNamespace ns = do
    -- Will destroy any views too.
    mapM_ Ui.destroyBlock =<< Ui.gets (inNs . Map.keys . Ui.stateBlocks)
    mapM_ Ui.destroyTrack =<< Ui.gets (inNs . Map.keys . Ui.stateTracks)
    mapM_ Ui.destroyRuler =<< Ui.gets (inNs . Map.keys . Ui.stateRulers)
    where
    inNs :: Id.Ident a => [a] -> [a]
    inNs = filter $ (==ns) . Id.identNamespace

-- | Replace the namespace of the second state with the one from the first
-- state.
replaceNamespace :: Id.Namespace -> Ui.State -> Ui.State -> Ui.State
replaceNamespace ns from to = to
    { Ui.stateViews = merge Ui.stateViews
    , Ui.stateBlocks = merge Ui.stateBlocks
    , Ui.stateTracks = merge Ui.stateTracks
    , Ui.stateRulers = merge Ui.stateRulers
    }
    where
    merge field =
        Map.union (Map.filterWithKey (\k _ -> wanted k) (field from)) (field to)
        where wanted = (==ns) . Id.identNamespace

-- * code

type UpdateM a = Logger.Logger Text a

-- | Modify tracklang code: block titles, track titles, events.
mapCode :: (Text -> Text) -> Ui.State -> (Ui.State, [Text])
mapCode modify state = Logger.runId $ do
    tracks <- mapM (\(tid, track) -> (tid,) <$> uTrack tid track)
        (Map.toList (Ui.stateTracks state))
    blocks <- mapM (\(bid, block) -> (bid,) <$> uBlock bid block)
        (Map.toList (Ui.stateBlocks state))
    pure $ state
        { Ui.stateTracks = Map.fromList tracks
        , Ui.stateBlocks = Map.fromList blocks
        }
    where
    uTrack trackId track = do
        let old = Track.trackTitle track
        let new = modify old
        log (pretty trackId) old new
        events <- Events.fromList <$>
            mapM uEvent (Events.ascending (Track.trackEvents track))
        pure $ track { Track.trackTitle = new, Track.trackEvents = events }
    uEvent event = do
        let old = Event.text event
        let new = modify old
        log (pretty (Event.start event)) old new
        pure $ Event.setTextRaw new event
    uBlock blockId block = do
        let old = Block.blockTitle block
        let new = modify old
        log (pretty blockId) old new
        pure $ block { Block.blockTitle = new }
    log place old new
        | new /= old = Logger.log $ place <> ": "
            <> ShowVal.showVal old <> " -> " <> ShowVal.showVal new
        | otherwise = pure ()

-- * merge

-- | Merge ID maps from the states together.  Collisions will throw.
-- The 'Ui.Config' comes from the first state.
mergeStates :: Ui.State -> Ui.State -> Either Ui.Error Ui.State
mergeStates st0 st1 = Ui.exec st0 $ do
    views <- safeUnion "views" (Ui.stateViews st0) (Ui.stateViews st1)
    blocks <- safeUnion "blocks" (Ui.stateBlocks st0) (Ui.stateBlocks st1)
    tracks <- safeUnion "tracks" (Ui.stateTracks st0) (Ui.stateTracks st1)
    rulers <- safeUnion "rulers" (Ui.stateRulers st0) (Ui.stateRulers st1)
    Ui.modify $ \st -> st
        { Ui.stateViews = views, Ui.stateBlocks = blocks
        , Ui.stateTracks = tracks, Ui.stateRulers = rulers
        }

safeUnion :: (Ui.M m, Ord k, Show k) => String
    -> Map k a -> Map k a -> m (Map k a)
safeUnion name fm0 fm1
    | Map.null overlapping = pure fm
    | otherwise = Ui.throw $ "keys collided in " <> showt name <> ": "
        <> showt (Map.keys overlapping)
    where (fm, overlapping) = Maps.uniqueUnion fm0 fm1


-- * intern

-- | Increase sharing in event text with an intern table.
internText :: Ui.State -> (Ui.State, Map Text Int)
internText state =
    (state { Ui.stateTracks = Map.fromAscList tracks }, Map.map snd table)
    where
    (table, tracks) = List.mapAccumL internTrack Map.empty
        (Map.toAscList (Ui.stateTracks state))
    internTrack state (trackId, track) =
        ( state2, (trackId
        , track { Track.trackEvents = Events.fromList events })
        )
        where
        (state2, events) = List.mapAccumL Event.internEvent state
            (Events.ascending (Track.trackEvents track))

internStats :: Map Text Int -> (Memory.Size, Int)
internStats table =
    (Memory.fromBytes $ Num.sum (map stats (Map.toList table)), totalHits)
    where
    totalHits = Num.sum (Map.elems table) - Map.size table
    stats (text, hits) = size * (hits - 1)
        where size = Text.length text * 2 + 3 * 4 -- pointer + length + start

-- * stats

data Stats = Stats
    { _blocks :: Int
    , _tracks :: Int
    , _events :: Int
    } deriving (Show)

shortStats :: Ui.State -> Text
shortStats state = Text.unwords
    [ "blocks:" <> showt _blocks
    , "tracks:" <> showt _tracks
    , "events:" <> showt _events
    ]
    where Stats { _blocks, _tracks, _events } = stats state

showStats :: Ui.State -> Text
showStats state = Text.unlines
    [ "blocks: " <> showt _blocks
    , "tracks: " <> showt _tracks
        <> " (" <> Num.showFloat 2 perBlock <>  " per block)"
    , "events: " <> showt _events
        <> " (" <> Num.showFloat 2 perEvent <> " per track)"
    ]
    where
    perBlock = fromIntegral _tracks / fromIntegral _blocks
    perEvent = fromIntegral _events / fromIntegral _tracks
    Stats { _blocks, _tracks, _events } = stats state

stats :: Ui.State -> Stats
stats state = Stats
    { _blocks = Map.size (Ui.stateBlocks state)
    , _tracks = Map.size (Ui.stateTracks state)
    , _events = Num.sum $ map (Events.length . Track.trackEvents) $
        Map.elems $ Ui.stateTracks state
    }
