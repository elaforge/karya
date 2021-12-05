-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
-- | This ties together the lower level tscore components.
--
-- Parse tscore, check and postprocess it, convert to Ui.State, and integrate.
module Derive.TScore.TScore (
    cmd_integrate
    , parse_score
#ifdef TESTING
    , module Derive.TScore.TScore
#endif
) where
import qualified Control.Monad.Identity as Identity
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Manual as Manual
import qualified Cmd.Perf as Perf
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Modify as Ruler.Modify

import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Note
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.TScore.Check as Check
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.GenId as GenId
import qualified Ui.Id as Id
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- * types

data Block track = Block {
    _block_id :: !BlockId
    , _block_title :: !Text
    , _meter :: ![Meter.LabeledMark]
    -- | True if this was created via T.SubBlock.
    , _is_sub :: !Bool
    , _tracks :: ![track]
    } deriving (Eq, Show, Functor)

-- | A tscore track consists of multiple tracklang tracks, since it includes
-- both rhythm and pitch.
data NTrack = NTrack {
    _note :: !Track
    , _key :: !Text
    , _controls :: ![Track]
    -- | End of the track.  This could be past the end of the last event if
    -- there was a rest on the end.  This is intentionally not strict, because
    -- you should iterate over the tracks and events before looking at it.
    , _end :: T.Time
    } deriving (Eq, Show)

-- | This track has been parsed, and directives propagated to Check.Config,
-- but not yet converted to a Track.
data ParsedTrack = ParsedTrack {
    track_config :: !Check.Config
    , track_key :: !Text
    , track_title :: !Text
    , track_tokens :: ![Token (T.NPitch T.Pitch)]
    , track_pos :: !T.Pos
    } deriving (Show)

type Token pitch = T.Token T.CallText pitch T.NDuration T.Duration

-- | A complete track, ready to be integrated or directly put in a block.
data Track = Track {
    _title :: !Text
    , _events :: !Events.Events
    } deriving (Eq, Show)

type Error = Text

instance Pretty track => Pretty (Block track) where
    format (Block block_id block_title meter is_sub tracks) =
        Pretty.record "Block"
            [ ("block_id", Pretty.format block_id)
            , ("block_title", Pretty.format block_title)
            , ("meter", Pretty.format (length meter) <> " marks")
            , ("is_sub", Pretty.format is_sub)
            , ("tracks", Pretty.format tracks)
            ]

instance Pretty NTrack where
    format (NTrack note key controls end) = Pretty.record "NTrack"
        [ ("note", Pretty.format note)
        , ("key", Pretty.format key)
        , ("controls", Pretty.format controls)
        , ("end", Pretty.format end)
        ]

instance Pretty Track where
    format (Track title events) = Pretty.format title <> Pretty.format events

-- * toplevel

cmd_integrate :: Cmd.M m => Text -> m [BlockId]
cmd_integrate source = do
    ui_state <- Ui.get
    cmd_state <- Cmd.get
    integrate (get_external_duration ui_state cmd_state) source

integrate :: Ui.M m => GetExternalCallDuration -> Text -> m [BlockId]
    -- ^ newly created blocks
integrate get_ext_dur source = do
    ns <- Ui.get_namespace
    (blocks, config) <- Ui.require_right id $ track_blocks ns get_ext_dur source
    unless (config == ScoreConfig mempty mempty) $
        Ui.throw $ "instruments or ky are only for standalone tscore,\
            \ put those in the Ui.State directly: " <> showt config
    let (subs, parents) = List.partition _is_sub blocks
    -- Unlike normal blocks, sub-blocks aren't integrated, but deleted and
    -- created from scratch each time.  This is so I don't have to worry about
    -- making their generated BlockIds stable.
    destroy_subs
    sub_ids <- mapM ui_block subs
    -- Mark them as sub blocks so I can delete them on the next integrate.
    mapM_ (flip Ui.modify_block_meta (Map.insert sub_meta "")) sub_ids
    old <- Ui.gets Ui.state_blocks
    let renames = find_block_renames old parents
    unless (null renames) $ Transform.map_block_ids $ \id ->
        maybe id Id.unpack_id (lookup (Id.BlockId id) renames)
    mapMaybeM integrate_block parents

destroy_subs :: Ui.M m => m ()
destroy_subs = do
    blocks <- filter (is_sub_block . snd) . Map.toList <$>
        Ui.gets Ui.state_blocks
    mapM_ (Ui.destroy_block . fst) blocks
    mapM_ Ui.destroy_track $ concatMap (Block.block_track_ids . snd) blocks

track_blocks :: Id.Namespace -> GetExternalCallDuration -> Text
    -> Either Error ([Block NTrack], ScoreConfig)
track_blocks namespace get_ext_dur source = do
    (blocks, config) <- first (T.show_error source) $ parse_blocks source
    whenJust (check_recursion $ map (track_tokens <$>) blocks) Left
    case check_recursive_copy_from blocks of
        [] -> return ()
        errs -> Left $ "recursive %f: "
            <> mconcat (map (T.show_error source) errs)
    case concatMap check_unique_keys blocks of
        [] -> return ()
        errs -> Left $ mconcat (map (T.show_error source) errs)

    -- -- TODO not implemented yet
    -- let cmd_config = undefined
    -- derive_args <- make_derive_args cmd_config (config_instruments config)
    --     (config_ky config)
    -- let get_ext_dur = get_external_duration2 derive_args

    (errs, blocks) <- return $
        partition_errors $ resolve_blocks get_ext_dur source blocks
    unless (null errs) $
        Left $ Text.intercalate "; " errs
    return (map (set_namespace namespace) blocks, config)

check_unique_keys :: Block ParsedTrack -> [T.Error]
check_unique_keys block =
    concatMap mkerror $ Seq.find_dups key_of (_tracks block)
    where
    mkerror (track, tracks) =
        T.Error (track_pos track)
            ("non-unique track key " <> pretty (key_of track))
        : [T.Error (track_pos t) "" | t <- NonEmpty.toList tracks]
    key_of t = (track_key t, track_title t)

-- | Replace the namespace.
--
-- I used to put tscore-generated things in 'Parse.default_namespace', so
-- they wouldn't clash with non-tscore blocks.  But then it turns out that
-- means tscore blocks can't call other tscore blocks without qualification!
-- Since I want more seemless integration, I no longer want a separate tscore
-- namespace.
set_namespace :: Id.Namespace -> Block track -> Block track
set_namespace ns block =
    block { _block_id = Id.modify update (_block_id block) }
    where
    update block_id
        | Id.ident_namespace block_id == Parse.default_namespace =
            Id.set_namespace ns block_id
        | otherwise = block_id

partition_errors :: [Block (Either err track)] -> ([err], [Block track])
partition_errors = first concat . unzip . map partition_block
    where
    partition_block block = (errs, block { _tracks = tracks })
        where (errs, tracks) = Either.partitionEithers (_tracks block)

-- | Get the duration of a block call from the tracklang performance, not
-- tscore.
--
-- transformers -> call -> (duration, logs)
type GetExternalCallDuration =
    [Text] -> Text -> (Either Error TrackTime, [Log.Msg])

-- TODO I'll need some way to get the logs out, but I'd prefer to not make
-- everything monadic.
get_external_duration :: Ui.State -> Cmd.State -> GetExternalCallDuration
get_external_duration ui_state cmd_state transformers call =
    first adapt $ Identity.runIdentity $
        Cmd.eval ui_state cmd_state (lookup_call_duration transformers call)
    where
    adapt (Left err) = Left (txt err)
    adapt (Right Nothing) = Left "call doesn't support CallDuration"
    adapt (Right (Just dur)) = Right dur

lookup_call_duration :: Cmd.M m => [Text] -> Text -> m (Maybe TrackTime)
lookup_call_duration transformers call = do
    -- I need BlockId, TrackId to get the Dynamic, for deriving context.
    -- I think it shouldn't really matter for call duration, but of course it
    -- could.  If I pick the root block then I get global transform and
    -- whatever transform is in the root.
    (block_id, track_id) <- root_block
    result <- Perf.derive_at_throw block_id track_id $
        Derive.get_score_duration deriver
    case result of
        Left err -> Cmd.throw $ pretty err
        Right Derive.Unknown -> return Nothing
        Right (Derive.CallDuration dur) -> return $ Just dur
    where
    -- I think if I have a root block with a performance then I don't need
    -- with_default_imported, but for tests I don't have a Performance.
    -- TODO it would be better to get a Performance for tests.

    transform = map (Eval.eval_transform_expr "lookup_call_duration") $
        filter (not . Text.null) transformers
    deriver = Derive.with_default_imported $
        foldr (.) id transform $
        Perf.derive_event (Derive.Note.track_info track [])
            (Event.event 0 1 call)
    track = TrackTree.make_track "title" mempty 1

root_block :: Ui.M m => m (BlockId, TrackId)
root_block = do
    block_id <- Ui.get_root_id
    track_id <- Ui.require "root block has no tracks" . Seq.head
        =<< Ui.track_ids_of block_id
    return (block_id, track_id)

-- * get_external_duration2

-- TODO: not implemented yet

data DeriveArgs =
    DeriveArgs Cmd.Config UiConfig.Allocations Derive.Builtins
        Derive.InstrumentAliases
    deriving (Show)

make_derive_args :: Cmd.Config -> [T.Allocation] -> Text
    -> Either Error DeriveArgs
make_derive_args cmd_config allocs ky = do
    (builtins, aliases) <- parse_ky ky
    allocations <- convert_allocations allocs
    return $ DeriveArgs cmd_config allocations builtins aliases
    where
    parse_ky :: Text -> Either Error (Derive.Builtins, Derive.InstrumentAliases)
    parse_ky = undefined -- TODO
    convert_allocations :: [T.Allocation] -> Either Error UiConfig.Allocations
    convert_allocations = undefined -- TODO

get_external_duration2 :: DeriveArgs -> GetExternalCallDuration
get_external_duration2 derive_args transformers call =
    ( case result of
        Left err -> Left $ pretty err
        Right (Left err) -> Left $ pretty err
        Right (Right Derive.Unknown) ->
            Left "call doesn't support CallDuration"
        Right (Right (Derive.CallDuration dur)) -> Right dur
    , logs
    )
    where
    (result, logs) = mini_derive derive_args $
        Derive.with_default_imported $
        Derive.get_score_duration $
        foldr (.) id transform $
        Perf.derive_event (Derive.Note.track_info track [])
            (Event.event 0 1 call)
    transform = map (Eval.eval_transform_expr "lookup_call_duration") $
        filter (not . Text.null) transformers
    track = TrackTree.make_track "title" mempty 1

mini_derive :: DeriveArgs -> Derive.Deriver a
    -> (Either Derive.Error a, [Log.Msg])
mini_derive (DeriveArgs cmd_config allocs builtins aliases) deriver = do
    let ui_state = Ui.config#UiConfig.allocations #= allocs $ Ui.empty
    Perf.mini_derive ui_state cmd_config builtins aliases deriver

-- * detect moves

{- | Use a heuristic to see if any blocks have been renamed.

    I can't know for sure because tscore is just text, and there's no identetiy
    for the chunk of test that represents a block.  But I do rename blocks
    often, so I'd like something smarter than making an unrelated copy.  I could
    exit and issue a rename command, but I'd have to also modify the text, and
    besides the UI seems awkward.  So I detect the rename, but you have to
    just rename the block, not modify any events.

    A block is a rename of another one if:
    - I am its source: An existing one has integrated_manual = "tscore"
    - With NoteDestinations equal to the ones I would produce.
    - It was deleted: BlockId is not in current blocks.

    I don't care if its actual events differ, because I want to retain local
    edits across the rename.
-}
find_block_renames :: Map BlockId Block.Block -> [Block NTrack]
    -> [(BlockId, BlockId)]
find_block_renames old_blocks new_blocks = mapMaybe renamed new_blocks
    where
    renamed new_block = case List.find ((==converted) . snd) candidates of
        Nothing -> Nothing
        Just (old_block_id, _) -> Just (old_block_id, _block_id new_block)
        where converted = map strip_title $ convert_tracks new_block
    -- See convert_destination below.
    strip_title (note, controls) = (note { Convert.track_title = "" }, controls)
    candidates :: [(BlockId, [(Convert.Track, [Convert.Track])])]
    candidates = map (second (map convert_destination)) $
        mapMaybe (traverse
            (Map.lookup source_key . Block.block_integrated_manual))
        deleted
    deleted = filter ((`notElem` block_ids) . fst) $ Map.toList old_blocks
    block_ids = map _block_id new_blocks

convert_destination :: Block.NoteDestination -> (Convert.Track, [Convert.Track])
convert_destination (Block.NoteDestination _key note controls) =
    -- NoteDestinations don't record the track title.  I guess it could, but
    -- I'm just going to replace it anyway.  So strip it out of the new blocks
    -- too before comparing.  But this means I should do a merge after the
    -- rename, or I'll miss a possible simultaneous track title change.
    ( convert "" (snd note)
    , map (uncurry convert) $ Map.toAscList (snd <$> controls)
    )
    where
    convert title index = Convert.Track
        { track_title = title
        , track_events = Map.elems index
        }

-- * resolve blocks

-- | Look for recursive block calls.  If there are none, it's safe to
-- 'resolve_blocks'.
check_recursion :: [Block [Token pitch]] -> Maybe Error
check_recursion blocks =
    either Just (const Nothing) $ mapM_ (check_block []) blocks
    where
    by_block_id = Map.fromList $ Seq.key_on _block_id blocks
    check_block stack_ block
        | _block_id block `elem` stack_ =
            Left $ "recursive loop: "
                <> Text.intercalate ", " (map Parse.show_block (reverse stack))
        | otherwise =
            mapM_ (check_track stack (_block_id block)) (_tracks block)
        where stack = _block_id block : stack_
    check_track stack parent =
        mapM_ (check_call stack parent) . mapMaybe call_of
    check_call stack parent call =
        whenJust (Check.call_block_id parent call) $ \block_id ->
            whenJust (Map.lookup block_id by_block_id) $
                check_block stack

call_of :: T.Token T.CallText pitch ndur rdur -> Maybe T.CallText
call_of (T.TNote _ note) = Just $ T.note_call note
call_of _ = Nothing

type ResolvedNote = (T.Time, T.Note T.CallText (Maybe T.PitchText) T.Time)

-- | Check and resolve pitches and durations with 'Check.check'.
--
-- This has to be interleaved across blocks because 'T.CallDuration' means the
-- duration of a note can depend on the duration of other blocks, and so forth.
-- I can get this interleaved and cached via a lazy memo table, but it's only
-- safe because I previously did 'check_recursion'.
resolve_blocks :: GetExternalCallDuration -> Text -> [Block ParsedTrack]
    -> [Block (Either Error NTrack)]
resolve_blocks get_ext_dur source blocks =
    map (fmap (fmap make_track)) $ Map.elems resolved_notes
    where
    resolved_notes :: Map Id.BlockId
        (Block (Either Error ((Bool, [ResolvedNote], Text, Text), T.Time)))
    resolved_notes = Map.fromList
        [(_block_id block, resolve_block block) | block <- blocks]
    -- Memoized duration of blocks.
    block_durations :: Map Id.BlockId (Either Error T.Time)
    block_durations = Map.mapWithKey block_duration resolved_notes

    -- Convert [ResolvedNote] to NTrack.
    make_track ((negative, notes, key, title), end) = NTrack
        { _note = Track
            { _title = if title == "" then ">" else title
            , _events = Events.from_list $ map (uncurry note_event) notes
            }
        , _key = key
        , _controls = if null pitches then [] else (:[]) $ Track
            { _title = "*"
            , _events = Events.from_list pitches
            }
        , _end = end
        }
        where
        pitches = map (pitch_event negative) $
            Seq.map_maybe_snd T.note_pitch notes

    resolve_block block = block
        { _tracks = resolve_tracks (_block_id block) (_block_title block)
            (_tracks block)
        }
    resolve_tracks block_id block_title =
        snd . List.mapAccumL (resolve block_id block_title) Nothing . zip [1..]
    resolve block_id block_title mb_asserts
            (tracknum, ParsedTrack config key title tokens _pos) =
        (Just $ fromMaybe asserts mb_asserts,) $ first (T.show_error source)$ do
            whenJust (Seq.head errs) Left
            let to_tracks = map (fmap ((\(_, notes, _, _) -> notes) . fst))
                    . _tracks
            mb_from_track <- case Check.config_from config of
                Just from -> Just <$> do
                    from_track <- resolve_from (to_tracks <$> resolved_notes)
                        block_id tracknum from
                    first (T.Error (Check.from_pos from)
                        . ("can't copy from a broken track: "<>)) from_track
                Nothing -> return Nothing
            notes <- resolve_copy_from mb_from_track uncopied
            whenJust mb_asserts $ \prev ->
                whenJust (match_asserts prev asserts notes) Left
            return ((Check.config_negative config, notes, key, title), end)
        where
        ((metas, uncopied), end) = first Either.partitionEithers $
            Check.check (get_dur (to_transformers block_title title) block_id)
                config tokens
        (errs, asserts) = Either.partitionEithers metas
    -- I could memoize external calls in the same way as internal ones, but
    -- a tracklang call duration is set manually, so it can't affect another
    -- call duration, so the recursive thing doesn't happen.  Which is good,
    -- because there's a phase difference between tscore and tracklang (that's
    -- the integration), so it wouldn't be reliable anyway.  So a non-memoized
    -- lookup shouldn't have the same quadratic performance.
    get_dur transformers parent call = case Check.call_block_id parent call of
        Nothing -> (Left $ "not a block call: " <> showt call, [])
        Just block_id -> case Map.lookup block_id block_durations of
            Nothing ->
                ( bimap (("call " <> showt call <> ": ")<>) from_track_time
                    result
                , logs
                )
                where (result, logs) = get_ext_dur transformers call
            Just err_dur -> (err_dur, [])
    to_transformers block_title track_title = block_title
        : if track_title == "" then [] else [note_to_transform track_title]
    note_to_transform =
        either (const "") ShowVal.show_val . ParseTitle.parse_note
    block_duration block_id block = do
        tracks <- forM (zip [1..] (_tracks block)) $
            \(tracknum, track) -> first
                (("can't get duration of broken track: "
                    <> pretty (block_id, tracknum :: TrackNum) <> ": ")<>)
                track
        return $ maximum $ 0 : map snd tracks

-- | If an expected assert isn't found, or if I got one that wasn't expected,
-- emit an error with the location.
match_asserts :: [Check.AssertCoincident] -> [Check.AssertCoincident]
    -> [(T.Time, T.Note call pitch dur)] -> Maybe T.Error
match_asserts [] (Check.AssertCoincident _ pos : _) _ =
    Just $ T.Error pos "got unexpected assert"
match_asserts (Check.AssertCoincident t1 _ : expected) asserts time_notes =
    case asserts of
        Check.AssertCoincident t2 _ : asserts
            | t1 == t2 -> match_asserts expected asserts time_notes
        _ -> case find_pos t1 time_notes of
            Nothing -> Nothing
            Just pos -> Just $ T.Error pos "expected assert here"
match_asserts [] [] _ = Nothing

find_pos :: T.Time -> [(T.Time, T.Note call pitch dur)] -> Maybe T.Pos
find_pos t time_notes = case dropWhile ((<t) . fst) time_notes of
    (_, note) : _ -> Just $ T.note_pos note
    -- Don't enforce asserts past the end of the notes.
    [] -> Nothing

-- ** copy from

-- | Each %f must refer to a track without %f.  This also forbids non-recursive
-- multiple levels of %f, which 'resolve_copy_from' would handle fine, but I
-- don't mind forbidding those too.
--
-- This does a redundant 'resolve_from' since 'resolve_blocks' will do it in
-- again, but it seems like a hassle to try to stash the resolved froms in a
-- Block.
check_recursive_copy_from :: [Block ParsedTrack] -> [T.Error]
check_recursive_copy_from blocks = concatMap check_block blocks
    where
    check_block block =
        mapMaybe (check (_block_id block)) (zip [1..] (_tracks block))
    check block_id (tracknum, track) = do
        from <- Check.config_from (track_config track)
        track2 <- either (const Nothing) Just $
            resolve_from block_tracks block_id tracknum from
        from2 <- Check.config_from (track_config track2)
        return $ T.Error (Check.from_pos from) $
            Parse.show_block_track block_id tracknum
            <> " has %f=" <> pretty from <> ", which has %f="
            <> pretty from2
    block_tracks =
        Map.fromList [(_block_id block, _tracks block) | block <- blocks]

-- | Resolve 'Check.From' to the track it names.  On a track %f=n copies
-- from the track number.  On a block, %f=name is like a %f on each track
-- with its corresponding track on the given block.
resolve_from :: Map Id.BlockId [track]
    -> Id.BlockId -> TrackNum -> Check.From -> Either T.Error track
resolve_from blocks current_block current_tracknum
        (Check.From mb_block_id tracknum pos) = do
    let block_id = fromMaybe current_block mb_block_id
    tracks <- tryJust (mkerror
            ("block not found: " <> Parse.show_block block_id)) $
        Map.lookup block_id blocks
    when (block_id == current_block && tracknum == current_tracknum) $
        Left $ mkerror $ "can't copy from the same track: "
            <> Parse.show_block block_id <> ":" <> pretty tracknum
    tryJust (mkerror $ Parse.show_block block_id
            <> " doesn't have track " <> pretty tracknum) $
        Seq.at tracks (tracknum - 1)
    where
    mkerror = T.Error pos

-- | Resolve T.CopyFrom.
--
-- Since I use the 'resolve_blocks' memo table, they will chain, even though
-- I'd sort of rather they didn't.
resolve_copy_from :: Maybe [ResolvedNote]
    -> [(T.Time, T.Note T.CallText (T.NPitch (Maybe T.PitchText)) T.Time)]
    -> Either T.Error [ResolvedNote]
resolve_copy_from Nothing = mapM (traverse no_from)
    where
    no_from note = case T.note_pitch note of
        T.NPitch pitch -> Right $ note { T.note_pitch = pitch }
        T.CopyFrom -> Left $ T.Error (T.note_pos note) "no %f for track"
resolve_copy_from (Just from_track) = concatMapM resolve
    where
    resolve (t, note) = case T.note_pitch note of
        T.NPitch pitch -> Right [(t, note { T.note_pitch = pitch })]
        T.CopyFrom -> copy_from (T.note_pos note) t (t + T.note_duration note)
    copy_from pos start end
        | null copied = Left $ T.Error pos $
            "no notes to copy in range " <> pretty start <> "--" <> pretty end
        | otherwise = Right copied
        where
        copied = takeWhile ((<end) . fst) $
            dropWhile ((<start) . fst) from_track

-- * integrate

-- | Tracks are written in reverse order.  This is because when notation is
-- horizontal, it's natural to write higher parts above above lower parts, as
-- with staff notation.  But when notation is vertical, it's natural to put
-- higher parts on the right, by analogy to instruments that are layed out that
-- way.
reverse_tracks :: Block track -> [track]
reverse_tracks = reverse . _tracks

integrate_block :: Ui.M m => Block NTrack -> m (Maybe BlockId)
integrate_block block = do
    ruler_id <- ui_ruler block
    Manual.block source_key (_block_id block) ruler_id (_block_title block)
        (convert_tracks block)

convert_tracks :: Block NTrack -> [(Convert.Track, [Convert.Track])]
convert_tracks block =
    [ (convert (_note track), map convert (_controls track))
    | track <- reverse_tracks block
    ]
    where
    convert track = Convert.Track
        { track_title = _title track
        , track_events = Events.ascending (_events track)
        }

source_key :: Block.SourceKey
source_key = "tscore"

-- * ui_state

parse_score :: Text -> Either Error (Ui.State, [T.Allocation])
parse_score = score_to_ui get_ext_dur
    where get_ext_dur _ _ = (Left "external call duration not supported", [])

score_to_ui :: GetExternalCallDuration -> Text
    -> Either Error (Ui.State, [T.Allocation])
score_to_ui get_ext_dur source = do
    (blocks, ScoreConfig instruments ky) <- track_blocks
        (UiConfig.config_namespace UiConfig.empty_config)
        get_ext_dur source
    first pretty $ second (, instruments) $ Ui.exec Ui.empty $ do
        mapM_ ui_block blocks
        Ui.modify_config $ UiConfig.ky #= ky

-- | Turn a Block NTrack into a UI block directly.  'integrate_block' does same
-- thing, but does so via the integrate machinery.
ui_block :: Ui.M m => Block NTrack -> m BlockId
ui_block block = do
    let tracks = reverse_tracks block
    track_ids <- fmap concat $
        forM tracks $ \(NTrack note _ controls _) ->
            forM (note : controls) $ \(Track title events) -> do
                track_id <- GenId.track_id (_block_id block)
                Ui.create_track track_id (Track.track title events)
    ruler_id <- ui_ruler block
    let btracks =
            [ Block.track (Block.TId tid ruler_id) Config.track_width
            | tid <- track_ids
            ]
    -- No longer necessary with Block.Implicit.
    -- Ui.set_skeleton block_id $ ui_skeleton tracks
    Ui.create_block (Id.unpack_id (_block_id block))
        (_block_title block)
        (Block.track (Block.RId ruler_id) Config.ruler_width : btracks)

-- ui_skeleton :: [NTrack] -> Skeleton.Skeleton
-- ui_skeleton = Skeleton.make . concat . snd . List.mapAccumL make 1
--     where
--     make tracknum track = (tracknum+len, zip ns (drop 1 ns))
--         where
--         len = length (_controls track) + 1
--         ns = [tracknum .. tracknum+len - 1]

ui_ruler :: Ui.M m => Block NTrack -> m RulerId
ui_ruler block = Ruler.Modify.replace (_block_id block) $
    Ruler.Modify.generate_until end (_meter block)
    where end = track_time $ maximum $ 0 : map _end (_tracks block)

-- * make_blocks

data ScoreConfig = ScoreConfig {
    config_instruments :: ![T.Allocation]
    , config_ky :: !Text
    } deriving (Eq, Show)

parse_blocks :: Text -> Either T.Error ([Block ParsedTrack], ScoreConfig)
parse_blocks source = do
    T.Score defs <- first (T.Error (T.Pos 0) . txt) $ Parse.parse_score source
    (blocks, config) <- foldM collect ([], Check.default_config) defs
    return
        ( blocks
        , ScoreConfig
            { config_instruments = Check.config_instruments config
            , config_ky = Check.config_ky config
            }
        )
    where
    collect (accum, config) (_pos, def) = do
        (blocks, config) <- interpret_toplevel config def
        return (blocks ++ accum, config)

interpret_toplevel :: Check.Config -> T.Toplevel
    -> Either T.Error ([Block ParsedTrack], Check.Config)
interpret_toplevel config (T.ToplevelDirective dir) =
    ([],) <$> Check.parse_directive Check.Global dir config
interpret_toplevel config (T.BlockDefinition block) = do
    block <- unwrap_block_tracks block
    (block, subs) <- return $ resolve_sub_block block
    block <- interpret_block config False block
    subs <- mapM (interpret_block config True) subs
    return (block : subs, config)

unwrap_block_tracks :: T.Block T.WrappedTracks
    -> Either T.Error (T.Block (T.Tracks T.Call))
unwrap_block_tracks block = do
    tracks <- unwrap_tracks $ T.block_tracks block
    return $ block { T.block_tracks = tracks }

-- | The number of tracks must match, and their titles must match.
unwrap_tracks :: T.WrappedTracks -> Either T.Error (T.Tracks T.Call)
unwrap_tracks (T.WrappedTracks _ []) = Right $ T.Tracks []
unwrap_tracks (T.WrappedTracks _ [tracks]) = Right tracks
unwrap_tracks (T.WrappedTracks pos (T.Tracks tracks1 : wrapped))
    | Just different <- List.find (/= titles1) titles =
        Left $ T.Error pos $ "wrapped track titles must match: "
            <> showt titles1 <> " /= " <> showt different
    | otherwise = Right $ T.Tracks $ zipWith merge tracks1 $ Seq.rotate $
        map T.untracks wrapped
    where
    -- [Tracks, Tracks] -> map untracks
    -- [[Track "a" a1, Track "b" b1], [Track "a" a2, Track "b" b2]] -> rotate
    -- [[Track "a" a1, Track "a" a2], [Track "b" b1, Track "b" b2]] -> merge
    -- [Track "a" (a1 ++ a2), Track "b" (b1 ++ b2)]
    merge track1 tracks = track1
        { T.track_tokens = concat $ T.track_tokens track1 : map tokens tracks
        }
    tokens track = case T.track_tokens track of
        ts@(t : _) -> coincident (T.token_pos t) : ts
        [] -> []
    titles1 = map T.track_title tracks1
    titles = map (map T.track_title . T.untracks) wrapped
    coincident pos = T.TBarline pos T.AssertCoincident

interpret_block :: Check.Config -> Bool -> T.Block (T.Tracks T.CallText)
    -> Either T.Error (Block ParsedTrack)
interpret_block config is_sub
        (T.Block block_id directives title (T.Tracks tracks)) = do
    block_config <- Check.parse_directives Check.Block config directives
    track_configs <- mapM
        (Check.parse_directives Check.Track block_config . T.track_directives)
        tracks
    track_configs <- return $
        zipWith (Check.apply_block_from block_config) [1..] track_configs
    return $ Block
        { _block_id = block_id
        , _block_title = title
        , _is_sub = is_sub
        , _meter = Check.meter_labeled $ Check.config_meter block_config
        , _tracks =
            [ ParsedTrack
                { track_config = track_config
                , track_key = key
                , track_title = title
                , track_tokens = tokens
                , track_pos = pos
                }
            | (track_config, T.Track key title _ tokens pos)
                <- zip track_configs tracks
            ]
        }

-- * sub-blocks

-- | Replace T.SubBlock with T.CallText, and return the generated blocks.
resolve_sub_block :: T.Block (T.Tracks T.Call)
    -> (T.Block (T.Tracks T.CallText), [T.Block (T.Tracks T.CallText)])
resolve_sub_block block = Logger.runId $ do
    tracks <- resolve_sub_tracks (T.block_id block) (T.block_tracks block)
    return $ block { T.block_tracks = tracks }

type ResolveM a = Logger.Logger (T.Block (T.Tracks T.CallText)) a

resolve_sub_tracks :: BlockId -> T.Tracks T.Call
    -> ResolveM (T.Tracks T.CallText)
resolve_sub_tracks block_id (T.Tracks tracks) =
    -- Since tracks will be reversed, start from the end, so the tracknums
    -- in the generated block names match up.
    T.Tracks <$> mapM resolve (zip (Seq.range_ (length tracks) (-1)) tracks)
    where
    resolve (tracknum, track) = do
        tokens <- resolve_sub_tokens block_id tracknum (T.track_tokens track)
        return $ track { T.track_tokens = tokens }

resolve_sub_tokens :: BlockId -> TrackNum -> [T.Token T.Call pitch ndur rdur]
    -> ResolveM [T.Token T.CallText pitch ndur rdur]
resolve_sub_tokens block_id tracknum = fmap snd . Seq.mapAccumLM resolve [1..]
    where
    resolve (n:ns) (T.TNote pos note) = case T.note_call note of
        T.SubBlock prefix subs -> do
            sub_calls <- resolve_sub_tracks_to_calls block_id tracknum n subs
            let call = pipe_tweak prefix `Texts.unwords2` Text.unwords sub_calls
            return (ns, T.TNote pos (note { T.note_call = call }))
        T.Call call -> return (n:ns, T.TNote pos (note { T.note_call = call }))
    resolve [] (T.TNote {}) = error "unreached, infinite list"
    resolve ns (T.TBarline pos bar) = return (ns, T.TBarline pos bar)
    resolve ns (T.TRest pos rest) = return (ns, T.TRest pos rest)

-- | Add a space so "foo|" -> "foo |", because of the xyz|[a] syntax.  But
-- don't add a space if it looks like it's not coming from that syntax, e.g.
-- "x |"[a].
pipe_tweak :: Text -> Text
pipe_tweak prefix
    | Just rest <- Text.stripSuffix "|" prefix,
        Just (_, c) <- Text.unsnoc rest, c `notElem` ['|', ' '] = rest <> " |"
    | otherwise = prefix

resolve_sub_tracks_to_calls :: BlockId -> TrackNum -> TrackNum
    -> [T.Tracks T.Call] -> ResolveM [T.CallText]
resolve_sub_tracks_to_calls parent_block_id tracknum callnum subs =
    forM (zip subs sub_callnums) $ \(tracks, sub_callnum) -> do
        let block_id = make_relative parent_block_id tracknum callnum
                sub_callnum
        tracks <- resolve_sub_tracks block_id tracks
        Logger.log $ T.Block
            { block_id = block_id
            , block_directives = []
            , block_title = ""
            , block_tracks = tracks
            }
        return $ Eval.block_id_to_call True parent_block_id block_id
    where
    sub_callnums
        | len == 1 = [Nothing]
        | len <= 26 = map (Just . Text.singleton) ['a'..'z']
        | otherwise = map (Just . ("-"<>) . showt) [1..]
        where len = length subs

make_relative :: BlockId -> TrackNum -> TrackNum -> Maybe Text -> BlockId
make_relative parent tracknum callnum sub_callnum =
    Id.BlockId $ Id.set_name relative (Id.unpack_id parent)
    where
    relative = Eval.make_relative parent $
        "t" <> showt tracknum <> "c" <> showt callnum
            <> fromMaybe "" sub_callnum

is_sub_block :: Block.Block -> Bool
is_sub_block = Map.member sub_meta . Block.block_meta

-- | This marks a sub-block.
sub_meta :: Text
sub_meta = "is_sub"

-- * local util

note_event :: T.Time -> T.Note T.CallText pitch T.Time -> Event.Event
note_event start note =
    add_stack $ Event.event (track_time start)
        (if T.note_zero_duration note then 0
            else track_time (T.note_duration note))
        (T.note_call note)

pitch_event :: Bool -> (T.Time, T.PitchText) -> Event.Event
pitch_event negative (start, pitch) = add_stack $
    Event.event (track_time start) (if negative then -0 else 0) pitch

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
