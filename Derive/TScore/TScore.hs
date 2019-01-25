-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | This ties together the lower level tscore components.
--
-- Parse tscore, check and postprocess it, convert to Ui.State, and integrate.
module Derive.TScore.TScore where
import qualified Control.Monad.Identity as Identity
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified App.Config as Config
import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Perf as Perf
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Modify as Ruler.Modify

import qualified Derive.Derive as Derive
import qualified Derive.Note
import qualified Derive.Stack as Stack
import qualified Derive.TScore.Check as Check
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * types

data Block track = Block {
    _block_id :: !BlockId
    , _block_title :: !Text
    , _meter :: ![Meter.LabeledMark]
    , _tracks :: ![track]
    } deriving (Eq, Show, Functor)

data NTrack = NTrack {
    _note :: !Track
    , _controls :: [Track]
    } deriving (Eq, Show)

type Token = T.Token T.Pitch T.NDuration T.Duration

data Track = Track {
    _track_id :: !TrackId
    , _title :: !Text
    , _events :: !Events.Events
    } deriving (Eq, Show)

track_ids :: NTrack -> [TrackId]
track_ids (NTrack note controls) = _track_id note : map _track_id controls

-- * toplevel

cmd_integrate :: Cmd.M m => Text -> m [BlockId]
cmd_integrate source = do
    ui_state <- Ui.get
    cmd_state <- Cmd.get
    let get_ext_dur = get_external_duration ui_state cmd_state
    integrate get_ext_dur source

integrate :: Ui.M m => GetExternalCallDuration -> Text
    -> m [BlockId] -- ^ newly created blocks
integrate get_ext_dur source = do
    blocks <- Ui.require_right id $ (track_blocks get_ext_dur) source
    -- TODO trim blocks that disappeared like with tracks?
    mapMaybeM integrate_block blocks

track_blocks :: GetExternalCallDuration -> Text -> Either Text [Block NTrack]
track_blocks get_ext_dur source = do
    blocks <- first (T.show_error source) $ parsed_blocks source
    let tokens_of (_, _, ts) = ts
    whenJust (check_recursion $ map (tokens_of <$>) blocks) Left
    (errs, blocks) <- return $
        partition_errors $ make_tracks get_ext_dur source blocks
    unless (null errs) $
        Left $ Text.intercalate "; " errs
    return blocks

partition_errors :: [Block (Either err track)] -> ([err], [Block track])
partition_errors = first concat . unzip . map partition_block
    where
    partition_block block = (errs, block { _tracks = tracks })
        where (errs, tracks) = Either.partitionEithers (_tracks block)

-- | Get the duration of a block call from the tracklang performance, not
-- tscore.
type GetExternalCallDuration = Text -> (Either Text TrackTime, [Log.Msg])

-- TODO I'll need some way to get the logs out, but I'd prefer to not make
-- everything monadic.
get_external_duration :: Ui.State -> Cmd.State -> Text
    -> (Either Text TrackTime, [Log.Msg])
get_external_duration ui_state cmd_state call =
    first adapt $ Identity.runIdentity $
        Cmd.eval ui_state cmd_state (lookup_call_duration call)
    where
    adapt (Left err) = Left (txt err)
    adapt (Right Nothing) = Left "call doesn't support CallDuration"
    adapt (Right (Just dur)) = Right dur

lookup_call_duration :: Cmd.M m => Text -> m (Maybe TrackTime)
lookup_call_duration call = do
    -- I need BlockId, TrackId to get the Dynamic, for deriving context.
    -- I think it shouldn't really matter for call duration, but of course it
    -- could.  If I pick the root block then I get global transform and
    -- whatever transform is in the root.
    (block_id, track_id) <- root_block
    result <- Perf.get_derive_at block_id track_id $
        Derive.get_score_duration deriver
    case result of
        Left err -> Cmd.throw $ pretty err
        Right Derive.Unknown -> return Nothing
        Right (Derive.CallDuration dur) -> return $ Just dur
    where
    -- I think if I have a root block with a performance then I don't need
    -- with_default_imported, but for tests I don't have a Performance.
    -- TODO it would be better to get a Performance for tests.
    deriver = Derive.with_default_imported $
        Perf.derive_event (Derive.Note.track_info track [])
            (Event.event 0 1 call)
    track = TrackTree.make_track "title" mempty 1

root_block :: Ui.M m => m (BlockId, TrackId)
root_block = do
    block_id <- Ui.get_root_id
    track_id <- Ui.require "root block has no tracks" . Seq.head
        =<< Ui.track_ids_of block_id
    return (block_id, track_id)


-- * memo

-- | Look for recursive block calls.  If there are none, it's safe to
-- 'make_tracks'.
check_recursion :: [Block [Token]] -> Maybe Text
check_recursion blocks =
    either Just (const Nothing) $ mapM_ (check_block []) blocks
    where
    by_block_id = Map.fromList $ Seq.key_on _block_id blocks
    check_block stack_ block
        | _block_id block `elem` stack_ =
            Left $ "recursive loop: "
                <> Text.intercalate ", " (map show_id (reverse stack))
        | otherwise = mapM_ (check_track stack) (_tracks block)
        where stack = _block_id block : stack_
    check_track stack = mapM_ (check_call stack) . mapMaybe call_of
    check_call stack call =
        whenJust (Map.lookup (Check.call_block_id call) by_block_id) $
            check_block stack
    show_id = Id.show_short Parse.default_namespace . Id.unpack_id

call_of :: T.Token pitch ndur rdur -> Maybe Text
call_of (T.TNote _ note) = Just $ (\(T.Call a) -> a) $ T.note_call note
call_of _ = Nothing

type ParsedTrack = (Check.Config, Text, [Token])

-- | Check and resolve pitches and durations with 'Check.process'.
--
-- This has to be interleaved across blocks because 'T.CallDuration' means the
-- duration of a note can depend on the duration of other blocks, and so forth.
-- I can get this interleaved and cached via a lazy memo table, but it's only
-- safe because I previously did 'check_recursion'.
make_tracks :: GetExternalCallDuration -> Text -> [Block ParsedTrack]
    -> [Block (Either Text NTrack)]
make_tracks get_ext_dur source blocks = Map.elems memo
    where
    memo = Map.fromList
        [ (_block_id block, resolve_block block)
        | block <- blocks
        ]
    resolve_block block = block
        { _tracks = zipWith (resolve (_block_id block)) [1..] (_tracks block)
        }
    resolve block_id tracknum (config, title, tokens) = do
        tokens <- first (T.show_error source) $
            sequence $ Check.process get_dur config tokens
        let pitches = map pitch_event $ Seq.map_maybe_snd T.note_pitch tokens
        return $ NTrack
            { _note = Track
                { _track_id = make_track_id block_id tracknum False
                , _title = if Text.null title then ">" else title
                , _events = Events.from_list $ map (uncurry note_event) tokens
                }
            , _controls = if null pitches then [] else (:[]) $ Track
                { _track_id = make_track_id block_id tracknum True
                , _title = "*"
                , _events = Events.from_list pitches
                }
            }
    -- I could memoize external calls in the same way, but a tracklang call
    -- duration is set manually, so it can't affect another call duration, so
    -- the recursive thing doesn't happen.  Which is good, because there's
    -- a phase difference between tscore and tracklang (that's the
    -- integration), so it wouldn't be reliable anyway.  So a non-memoized
    -- lookup shouldn't have the same quadratic performance.
    get_dur call = case Map.lookup block_id memo of
        Nothing -> case get_ext_dur call of
            -- TODO I should save the logs so I can write them, but I think
            -- I have to put everything in LogMonad.
            (Left err, logs) -> Left $ "call " <> pretty call <> ": " <> err
            (Right dur, logs) -> Right $ from_track_time dur
        Just block -> bimap (("in block " <> pretty block_id <> ": ")<>)
            (maximum . (0:) . map track_end)
            (sequence (_tracks block))
        where
        block_id = Check.call_block_id call

track_end :: NTrack -> T.Time
track_end (NTrack note controls) = from_track_time $
    maximum $ map (Events.time_end . _events) (note : controls)

-- * integrate

integrate_block :: Ui.M m => Block NTrack -> m (Maybe BlockId)
integrate_block block = do
    let block_id = _block_id block
    ruler_id <- ui_ruler block
    exists <- Maybe.isJust <$> Ui.lookup_block block_id
    if exists
        then Ui.set_block_title block_id (_block_title block)
        else void $ Ui.create_block (Id.unpack_id block_id) (_block_title block)
            [Block.track (Block.RId ruler_id) Config.ruler_width]
    existing_tracks <- Set.fromList <$> Ui.track_ids_of block_id
    -- This will wipe out diffs... do I really want that?
    let new = Set.fromList $ concatMap track_ids (_tracks block)
    let gone = existing_tracks `Set.difference` new
    mapM_ Ui.destroy_track (Set.toList gone)
    mapM_ (create_track block_id ruler_id) (_tracks block)

    dests <- Map.findWithDefault [] source_key . Block.block_integrated_manual
        <$> Ui.get_block (_block_id block)
    dests <- return $ pair_destinations block dests
    new_dests <- Merge.merge_tracks block_id
        [ (convert note, map convert controls)
        | NTrack note controls <- _tracks block
        ]
        dests
    Ui.set_integrated_manual block_id source_key (Just new_dests)

    unless exists $
        BlockConfig.toggle_merge_all block_id
    return $ if exists then Nothing else Just block_id
    where
    convert track = Convert.Track
        { track_title = _title track
        , track_events = Events.ascending (_events track)
        }

-- | Pair dests by TrackId, and then make Block.empty_destination for the rest.
pair_destinations :: Block NTrack -> [Block.NoteDestination]
    -> [Block.NoteDestination]
pair_destinations block dests =
    map (pair_note (destinations_by_track_id dests)) (_tracks block)
    where
    pair_note dests (NTrack note controls) =
        case Map.lookup (_track_id note) dests of
            Nothing -> Block.empty_destination (_track_id note)
                [(_title track, _track_id track) | track <- controls]
            Just (index, dest_controls) -> Block.NoteDestination
                { dest_note = (_track_id note, index)
                , dest_controls =
                    Map.fromList $ map (pair_control dest_controls) controls
                }
    pair_control dest_controls control =
        (_title control, (_track_id control, index))
        where
        index = Map.findWithDefault mempty (_track_id control) dest_controls

-- | Re-index NoteDestinations by TrackId.
destinations_by_track_id :: [Block.NoteDestination]
    -> Map TrackId (Block.EventIndex, Map TrackId Block.EventIndex)
destinations_by_track_id dests = Map.fromList $ do
    Block.NoteDestination (track_id, index) controls <- dests
    return (track_id, (index, Map.fromList (map control (Map.toList controls))))
    where
    -- I toss the original track name, because I'm indexing by TrackId.  If
    -- tscore changed the name, I want to replace with the new one anyway.
    control (_name, (track_id, index)) = (track_id, index)

-- BUT I don't have persistent TrackIds for controls anyway, since tscore
-- doesn't have explicit TrackIds like it does BlockIds.  At the moment though
-- I think it doesn't matter, since I only every have [pitch] or [] for
-- controls.  Actually I'll probably key on control name, so maybe I should
-- do that... or get rid of TrackIds here entirely, and let integrate take care
-- of it, like it wants to.

create_track :: Ui.M m => BlockId -> RulerId -> NTrack -> m ()
create_track block_id ruler_id (NTrack note controls) = do
    exists <- Maybe.isJust <$> Ui.lookup_track (_track_id note)
    unless exists $ forM_ (note : controls) $ \track -> do
        Ui.create_track (Id.unpack_id (_track_id track)) $
            Track.track (_title track) mempty
        Ui.insert_track block_id 999 $
            Block.track (Block.TId (_track_id track) ruler_id)
                Config.track_width
    Ui.set_track_title (_track_id note) (_title note)

source_key :: Block.SourceKey
source_key = "tscore"

-- * ui_state

ui_state :: GetExternalCallDuration -> Text -> Either Text Ui.State
ui_state get_ext_dur source = do
    blocks <- track_blocks get_ext_dur source
    first pretty $ Ui.exec Ui.empty $ do
        Ui.set_namespace Parse.default_namespace
        mapM_ ui_block blocks

ui_block :: Ui.M m => Block NTrack -> m ()
ui_block block = do
    track_ids <- sequence
        [ Ui.create_track (Id.unpack_id track_id) (Track.track title events)
        | NTrack note controls <- _tracks block
        , Track track_id title events <- note : controls
        ]
    ruler_id <- ui_ruler block
    let tracks =
            [ Block.track (Block.TId tid ruler_id) Config.track_width
            | tid <- track_ids
            ]
    Ui.create_block (Id.unpack_id (_block_id block)) (_block_title block) $
        Block.track (Block.RId ruler_id) Config.ruler_width : tracks
    -- The first block will become the root_id implicitly.
    return ()

ui_ruler :: Ui.M m => Block NTrack -> m RulerId
ui_ruler block = make_ruler (_block_id block) (_meter block) end
    where
    end = maximum $ 0 :
        [ Events.time_end (_events t1)
        | NTrack note controls <- _tracks block, t1 <- note : controls
        ]

make_ruler :: Ui.M m => BlockId -> [Meter.LabeledMark] -> TrackTime -> m RulerId
make_ruler block_id meter end = do
    whenM (Maybe.isNothing <$> Ui.lookup_ruler ruler_id) $
        void $ Ui.create_ruler (Id.unpack_id ruler_id) (Ruler.ruler [])
    Ui.modify_ruler ruler_id (generate_ruler meter end)
    return ruler_id
    where
    ruler_id = Id.RulerId $ Id.unpack_id block_id

generate_ruler :: [Meter.LabeledMark] -> TrackTime
    -> (Ruler.Ruler -> Either Text Ruler.Ruler)
generate_ruler meter end = Ruler.Modify.meter (const generate)
    where
    generate = trim $ cycle $ Seq.rdrop 1 meter
    trim = map snd . Then.takeWhile1 ((<end) . fst)
        . Seq.scanl_on (+) Meter.m_duration 0

-- * make_blocks

parsed_blocks :: Text -> Either T.Error [Block ParsedTrack]
parsed_blocks source = do
    T.Score defs <- first (T.Error (T.Pos 0) . txt) $ Parse.parse_score source
    fst <$> foldM collect ([], Check.default_config) defs
    where
    collect (accum, config) def = do
        (block, config) <- interpret_toplevel config def
        return (maybe id (:) block accum, config)

interpret_toplevel :: Check.Config -> (T.Pos, T.Toplevel)
    -> Either T.Error (Maybe (Block ParsedTrack), Check.Config)
interpret_toplevel config (pos, T.ToplevelDirective dir) = (Nothing,) <$>
    first (T.Error pos) (Check.parse_directive dir config)
interpret_toplevel config (pos, T.BlockDefinition block) = do
    block <- first (T.Error pos) $ interpret_block config block
    return (Just block, config)

interpret_block :: Check.Config -> T.Block -> Either Text (Block ParsedTrack)
interpret_block config
        (T.Block block_id directives title (T.Tracks tracks)) = do
    config <- Check.parse_directives config directives
    return $ Block
        { _block_id = block_id
        , _block_title = title
        , _meter = Check.meter_labeled $ Check.config_meter config
        , _tracks =
            [ (config, title, Check.preprocess config tokens)
            | T.Track title tokens <- tracks
            ]
        }

-- * local util

make_track_id :: BlockId -> TrackNum -> Bool -> TrackId
make_track_id block_id tracknum is_pitch =
    Id.TrackId $ Id.id ns $
        ident <> ".t" <> showt tracknum <> if is_pitch then ".pitch" else ""
    where
    (ns, ident) = Id.un_id $ Id.unpack_id block_id

note_event :: T.Time -> T.Note (Maybe Text) T.Time -> Event.Event
note_event start note = add_stack $
    Event.event (track_time start)
        (if T.note_zero_duration note then 0
            else track_time (T.note_duration note))
        call
    where T.Call call = T.note_call note

pitch_event :: (T.Time, Text) -> Event.Event
pitch_event (start, pitch) = add_stack $ Event.event (track_time start) 0 pitch

track_time :: T.Time -> TrackTime
track_time = realToFrac

from_track_time :: TrackTime -> T.Time
from_track_time = realToFrac

-- | A stack marks these events as being from an integration.  Event style uses
-- this, but I think that's all since I have SourceKey hardcoded.
add_stack :: Event.Event -> Event.Event
add_stack event =
    Event.stack_ #= Just (Event.Stack stack (Event.start event)) $ event
    where stack = Stack.add (Stack.Call source_key) Stack.empty
