-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
-- | This ties together the lower level tscore components.
--
-- Parse tscore, check and postprocess it, convert to Ui.State, and integrate.
module Derive.TScore.TScore (
    cmdIntegrate
    -- * for TScoreMain
    , GetExternalCallDuration
    , noExternalDuration
    , MakeExternalCallDuration
    , getExternalDuration
    , scoreToUi
    , uiBlock
    , trackBlocks_
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

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty
import qualified Util.Texts as Texts

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Manual as Manual
import qualified Cmd.Perf as Perf
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import qualified Derive.C.Prelude.Block as Prelude.Block
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
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- * types

data Block track = Block
    { _blockId :: !BlockId
    , _blockTitle :: !Text
    , _meter :: !(Meter.Config, Meter.MSection)
    -- | True if this was created via T.SubBlock.
    , _isSub :: !Bool
    , _tracks :: ![track]
    } deriving (Eq, Show, Functor)

-- | A tscore track consists of multiple tracklang tracks, since it includes
-- both rhythm and pitch.
data NTrack = NTrack
    { _note :: !Track
    , _key :: !Text
    , _controls :: ![Track]
    -- | End of the track.  This could be past the end of the last event if
    -- there was a rest on the end.  This is intentionally not strict, because
    -- you should iterate over the tracks and events before looking at it.
    , _end :: T.Time
    } deriving (Eq, Show)

-- | This track has been parsed, and directives propagated to Check.Config,
-- but not yet converted to a Track.
data ParsedTrack = ParsedTrack
    { trackConfig :: !Check.Config
    , trackKey :: !Text
    , trackTitle :: !Text
    , trackTokens :: ![Token (T.NPitch T.Pitch)]
    , trackPos :: !T.Pos
    } deriving (Show)

type Token pitch = T.Token T.CallText pitch T.NDuration T.Duration

-- | A complete track, ready to be integrated or directly put in a block.
data Track = Track
    { _title :: !Text
    , _events :: !Events.Events
    } deriving (Eq, Show)

instance Pretty track => Pretty (Block track) where
    format (Block blockId blockTitle meter isSub tracks) =
        Pretty.record "Block"
            [ ("block_id", Pretty.format blockId)
            , ("block_title", Pretty.format blockTitle)
            , ("meter", Pretty.format meter)
            , ("is_sub", Pretty.format isSub)
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

-- | Errors in the tscore itself are returned as T.Errors, others thrown in
-- Ui.M.
cmdIntegrate :: Ui.M m => Cmd.State -> T.Score -> m (Either [T.Error] [BlockId])
cmdIntegrate cmdState score = do
    uiState <- Ui.get
    -- Embedded tscore isn't allowed its own %ky, it uses the one already in
    -- the Cmd.State, so ignore it.
    integrate (const $ Right $ getExternalDuration uiState cmdState) score

integrate :: Ui.M m => MakeExternalCallDuration -> T.Score
    -> m (Either [T.Error] [BlockId]) -- ^ newly created blocks
integrate makeExtDur score = do
    ns <- Ui.getNamespace
    case trackBlocks ns makeExtDur score of
        Left errs -> pure $ Left errs
        Right (blocks, config) -> do
            unless (config == ScoreConfig mempty) $
                Ui.throw $ "ky is only for standalone tscore,\
                    \ put those in the Ui.State directly: " <> showt config
            Right <$> integrateBlocks blocks

integrateBlocks :: Ui.M m => [Block NTrack] -> m [BlockId]
integrateBlocks blocks = do
    let (subs, parents) = List.partition _isSub blocks
    -- Unlike normal blocks, sub-blocks aren't integrated, but deleted and
    -- created from scratch each time.  This is so I don't have to worry about
    -- making their generated BlockIds stable.
    destroySubs
    subIds <- mapM uiBlock subs
    -- Mark them as sub blocks so I can delete them on the next integrate.
    mapM_ (flip Ui.modifyBlockMeta (Map.insert subMeta "")) subIds
    old <- Ui.gets Ui.stateBlocks
    let renames = findBlockRenames old parents
    unless (null renames) $ Transform.mapBlockIds $ \id ->
        maybe id Id.unpackId (lookup (Id.BlockId id) renames)
    mapMaybeM integrateBlock parents

destroySubs :: Ui.M m => m ()
destroySubs = do
    blocks <- filter (isSubBlock . snd) . Map.toList <$>
        Ui.gets Ui.stateBlocks
    mapM_ (Ui.destroyBlock . fst) blocks
    mapM_ Ui.destroyTrack $ concatMap (Block.blockTrackIds . snd) blocks

trackBlocks :: Id.Namespace -> MakeExternalCallDuration -> T.Score
    -> Either [T.Error] ([Block NTrack], ScoreConfig)
trackBlocks namespace makeExtDur score = do
    (errs, blocks, config) <- trackBlocks_ namespace makeExtDur score
    unless (null errs) $ Left errs
    pure (blocks, config)

-- | Like 'track_blocks', but return the tracks that did resolve along with
-- the errors, instead of failing entirely.  This is for 'standalone_duration',
-- which wants whatever blocks it can get.
trackBlocks_ :: Id.Namespace -> MakeExternalCallDuration -> T.Score
    -> Either [T.Error] ([T.Error], [Block NTrack], ScoreConfig)
trackBlocks_ namespace makeExtDur score = do
    (blocks, config) <- first (:[]) $ parseBlocks score
    whenJust (checkRecursion $ map (trackTokens <$>) blocks) $ Left . (:[])
    case checkRecursiveCopyFrom blocks of
        [] -> pure ()
        errs -> Left errs
    case concatMap checkUniqueKeys blocks of
        [] -> pure ()
        errs -> Left errs
    -- Parsing doesn't need durations, so the %ky is available before
    -- 'resolve_blocks', which is what needs it.
    getExtDur <- makeExtDur (configKy config)
    (errs, blocks) <- pure $ partitionErrors $ resolveBlocks getExtDur blocks
    pure (errs, map (setNamespace namespace) blocks, config)

checkUniqueKeys :: Block ParsedTrack -> [T.Error]
checkUniqueKeys block =
    concatMap mkerror $ Lists.findDups keyOf (_tracks block)
    where
    mkerror (track, tracks) =
        T.err (trackPos track)
            ("non-unique track key " <> pretty (keyOf track))
        : [T.err (trackPos t) "" | t <- NonEmpty.toList tracks]
    keyOf t = (trackKey t, trackTitle t)

-- | Replace the namespace.
--
-- I used to put tscore-generated things in 'Parse.default_namespace', so
-- they wouldn't clash with non-tscore blocks.  But then it turns out that
-- means tscore blocks can't call other tscore blocks without qualification!
-- Since I want more seemless integration, I no longer want a separate tscore
-- namespace.
setNamespace :: Id.Namespace -> Block track -> Block track
setNamespace ns block =
    block { _blockId = Id.modify update (_blockId block) }
    where
    update blockId
        | Id.identNamespace blockId == Parse.defaultNamespace =
            Id.setNamespace ns blockId
        | otherwise = blockId

partitionErrors :: [Block (Either err track)] -> ([err], [Block track])
partitionErrors = first concat . unzip . map partitionBlock
    where
    partitionBlock block = (errs, block { _tracks = tracks })
        where (errs, tracks) = Either.partitionEithers (_tracks block)

type Error = Text

-- | Get the duration of a block call from the tracklang performance, not
-- tscore.
--
-- transformers -> call -> (duration, logs)
type GetExternalCallDuration =
    [Text] -> Text -> (Either Error TrackTime, [Log.Msg])

-- | For when there's no deriver to ask, e.g. tests, and the first pass of
-- 'standalone_duration'.
noExternalDuration :: GetExternalCallDuration
noExternalDuration _ _ = (Left "external call duration not supported", [])

-- | Make a 'GetExternalCallDuration' given the score's %ky.  Standalone tscore
-- has to build a derive environment out of that ky, while embedded tscore
-- already has one in its Cmd.State and ignores the argument.
type MakeExternalCallDuration =
    Text -> Either [T.Error] GetExternalCallDuration

-- TODO I'll need some way to get the logs out, but I'd prefer to not make
-- everything monadic.
getExternalDuration :: Ui.State -> Cmd.State -> GetExternalCallDuration
getExternalDuration uiState cmdState transformers call =
    first adapt $ Identity.runIdentity $
        Cmd.eval uiState cmdState (lookupCallDuration transformers call)
    where
    adapt (Left err) = Left (txt err)
    adapt (Right Nothing) = Left "call doesn't support CallDuration"
    adapt (Right (Just dur)) = Right dur

lookupCallDuration :: Cmd.M m => [Text] -> Text -> m (Maybe TrackTime)
lookupCallDuration transformers call = do
    (result, logs) <- Perf.deriveNoBlock $ Derive.getScoreDuration deriver
    mapM_ Log.write logs
    result <- Cmd.requireRight pretty result
    case result of
        Left err -> Cmd.throw $ pretty err
        Right Derive.Unknown -> pure Nothing
        Right (Derive.CallDuration dur) -> pure $ Just dur
    where
    -- with_default_imported has to be outside global_transform, since GLOBAL
    -- is itself a ky definition, and hence in Module.local.
    deriver = Derive.withDefaultImported $
        Prelude.Block.globalTransform $
        foldr (.) id transform $
        Perf.deriveEvent (Derive.Note.trackInfo track [])
            (Event.event 0 1 call)
    transform = map (Eval.evalTransformExpr "lookup_call_duration") $
        filter (not . Text.null) transformers
    track = TrackTree.makeTrack "title" mempty 1

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
findBlockRenames :: Map BlockId Block.Block -> [Block NTrack]
    -> [(BlockId, BlockId)]
findBlockRenames oldBlocks newBlocks = mapMaybe renamed newBlocks
    where
    renamed newBlock = case List.find ((==converted) . snd) candidates of
        Nothing -> Nothing
        Just (oldBlockId, _) -> Just (oldBlockId, _blockId newBlock)
        where converted = map stripTitle $ convertTracks newBlock
    -- See convert_destination below.
    stripTitle (note, controls) = (note { Convert.trackTitle = "" }, controls)
    candidates :: [(BlockId, [(Convert.Track, [Convert.Track])])]
    candidates = map (second (map convertDestination)) $
        mapMaybe (traverse
            (Map.lookup sourceKey . Block.blockIntegratedManual))
        deleted
    deleted = filter ((`notElem` blockIds) . fst) $ Map.toList oldBlocks
    blockIds = map _blockId newBlocks

convertDestination :: Block.NoteDestination -> (Convert.Track, [Convert.Track])
convertDestination (Block.NoteDestination _key note controls) =
    -- NoteDestinations don't record the track title.  I guess it could, but
    -- I'm just going to replace it anyway.  So strip it out of the new blocks
    -- too before comparing.  But this means I should do a merge after the
    -- rename, or I'll miss a possible simultaneous track title change.
    ( convert "" (snd note)
    , map (uncurry convert) $ Map.toAscList (snd <$> controls)
    )
    where
    convert title index = Convert.Track
        { trackTitle = title
        , trackEvents = Map.elems index
        }

-- * resolve blocks

-- | Look for recursive block calls.  If there are none, it's safe to
-- 'resolve_blocks'.
checkRecursion :: [Block [Token pitch]] -> Maybe T.Error
checkRecursion blocks =
    either Just (const Nothing) $ mapM_ (checkBlock []) blocks
    where
    byBlockId = Map.fromList $ Lists.keyOn _blockId blocks
    checkBlock stack_ block
        | _blockId block `elem` stack_ =
            Left $ T.Error Nothing $ "recursive loop: "
                <> Text.intercalate ", " (map Parse.showBlock (reverse stack))
        | otherwise =
            mapM_ (checkTrack stack (_blockId block)) (_tracks block)
        where stack = _blockId block : stack_
    checkTrack stack parent =
        mapM_ (checkCall stack parent) . mapMaybe callOf
    checkCall stack parent call =
        whenJust (Check.callBlockId parent call) $ \blockId ->
            whenJust (Map.lookup blockId byBlockId) $
                checkBlock stack

callOf :: T.Token T.CallText pitch ndur rdur -> Maybe T.CallText
callOf (T.TNote _ note) = Just $ T.noteCall note
callOf _ = Nothing

type ResolvedNote = (T.Time, T.Note T.CallText (Maybe T.PitchText) T.Time)

-- | Check and resolve pitches and durations with 'Check.check'.
--
-- This has to be interleaved across blocks because 'T.CallDuration' means the
-- duration of a note can depend on the duration of other blocks, and so forth.
-- I can get this interleaved and cached via a lazy memo table, but it's only
-- safe because I previously did 'check_recursion'.
resolveBlocks :: GetExternalCallDuration -> [Block ParsedTrack]
    -> [Block (Either T.Error NTrack)]
resolveBlocks getExtDur blocks =
    map (fmap (fmap makeTrack)) $ Map.elems resolvedNotes
    where
    resolvedNotes :: Map Id.BlockId
        (Block (Either T.Error ((Bool, [ResolvedNote], Text, Text), T.Time)))
    resolvedNotes = Map.fromList
        [(_blockId block, resolveBlock block) | block <- blocks]
    -- Memoized duration of blocks.
    blockDurations :: Map Id.BlockId (Either Error T.Time)
    blockDurations = Map.mapWithKey blockDuration resolvedNotes

    -- Convert [ResolvedNote] to NTrack.
    makeTrack ((negative, notes, key, title), end) = NTrack
        { _note = Track
            { _title = if title == "" then ">" else title
            , _events = Events.fromList $ map (uncurry noteEvent) notes
            }
        , _key = key
        , _controls = if null pitches then [] else (:[]) $ Track
            { _title = "*"
            , _events = Events.fromList pitches
            }
        , _end = end
        }
        where
        pitches = map (pitchEvent negative) $
            Lists.mapMaybeSnd T.notePitch notes

    resolveBlock block = block
        { _tracks = resolveTracks (_blockId block) (_blockTitle block)
            (_tracks block)
        }
    resolveTracks blockId blockTitle =
        snd . List.mapAccumL (resolve blockId blockTitle) Nothing . zip [1..]
    resolve blockId blockTitle mbAsserts
            (tracknum, ParsedTrack config key title tokens _pos) =
        (Just $ fromMaybe asserts mbAsserts,) $ do
            whenJust (Lists.head errs) Left
            let toTracks = map (fmap ((\(_, notes, _, _) -> notes) . fst))
                    . _tracks
            mbFromTrack <- case Check.configFrom config of
                Just from -> Just <$> do
                    fromTrack <- resolveFrom (toTracks <$> resolvedNotes)
                        blockId tracknum from
                    -- The broken track reports its own error with its
                    -- position, so only the message is needed here.
                    first (\(T.Error _ msg) -> T.err (Check.fromPos from) $
                            "can't copy from a broken track: " <> msg)
                        fromTrack
                Nothing -> pure Nothing
            notes <- resolveCopyFrom mbFromTrack uncopied
            whenJust mbAsserts $ \prev ->
                whenJust (matchAsserts prev asserts notes) Left
            pure ((Check.configNegative config, notes, key, title), end)
        where
        ((metas, uncopied), end) = first Either.partitionEithers $
            Check.check (getDur (toTransformers blockTitle title) blockId)
                config tokens
        (errs, asserts) = Either.partitionEithers metas
    -- I could memoize external calls in the same way as internal ones, but
    -- a tracklang call duration is set manually, so it can't affect another
    -- call duration, so the recursive thing doesn't happen.  Which is good,
    -- because there's a phase difference between tscore and tracklang (that's
    -- the integration), so it wouldn't be reliable anyway.  So a non-memoized
    -- lookup shouldn't have the same quadratic performance.
    getDur transformers parent call = case Check.callBlockId parent call of
        Nothing -> (Left $ "not a block call: " <> showt call, [])
        Just blockId -> case Map.lookup blockId blockDurations of
            Nothing ->
                ( bimap (("call " <> showt call <> ": ")<>) fromTrackTime
                    result
                , logs
                )
                where (result, logs) = getExtDur transformers call
            Just errDur -> (errDur, [])
    toTransformers blockTitle trackTitle = blockTitle
        : if trackTitle == "" then [] else [noteToTransform trackTitle]
    noteToTransform =
        either (const "") ShowVal.showVal . ParseTitle.parseNote
    -- The broken track also reports its own error with its position, so this
    -- only needs the message.
    blockDuration blockId block = do
        tracks <- forM (zip [1..] (_tracks block)) $
            \(tracknum, track) -> first
                (\(T.Error _ msg) -> "can't get duration of broken track: "
                    <> pretty (blockId, tracknum :: TrackNum) <> ": " <> msg)
                track
        pure $ maximum $ 0 : map snd tracks

-- | If an expected assert isn't found, or if I got one that wasn't expected,
-- emit an error with the location.
matchAsserts :: [Check.AssertCoincident] -> [Check.AssertCoincident]
    -> [(T.Time, T.Note call pitch dur)] -> Maybe T.Error
matchAsserts [] (Check.AssertCoincident _ pos : _) _ =
    Just $ T.err pos "got unexpected assert"
matchAsserts (Check.AssertCoincident t1 _ : expected) asserts timeNotes =
    case asserts of
        Check.AssertCoincident t2 _ : asserts
            | t1 == t2 -> matchAsserts expected asserts timeNotes
        _ -> case findPos t1 timeNotes of
            Nothing -> Nothing
            Just pos -> Just $ T.err pos "expected assert here"
matchAsserts [] [] _ = Nothing

findPos :: T.Time -> [(T.Time, T.Note call pitch dur)] -> Maybe T.Pos
findPos t timeNotes = case dropWhile ((<t) . fst) timeNotes of
    (_, note) : _ -> Just $ T.notePos note
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
checkRecursiveCopyFrom :: [Block ParsedTrack] -> [T.Error]
checkRecursiveCopyFrom blocks = concatMap checkBlock blocks
    where
    checkBlock block =
        mapMaybe (check (_blockId block)) (zip [1..] (_tracks block))
    check blockId (tracknum, track) = do
        from <- Check.configFrom (trackConfig track)
        track2 <- either (const Nothing) Just $
            resolveFrom blockTracks blockId tracknum from
        from2 <- Check.configFrom (trackConfig track2)
        pure $ T.err (Check.fromPos from) $
            "recursive %f: " <> Parse.showBlockTrack blockId tracknum
            <> " has %f=" <> pretty from <> ", which has %f="
            <> pretty from2
    blockTracks =
        Map.fromList [(_blockId block, _tracks block) | block <- blocks]

-- | Resolve 'Check.From' to the track it names.  On a track %f=n copies
-- from the track number.  On a block, %f=name is like a %f on each track
-- with its corresponding track on the given block.
resolveFrom :: Map Id.BlockId [track]
    -> Id.BlockId -> TrackNum -> Check.From -> Either T.Error track
resolveFrom blocks currentBlock currentTracknum
        (Check.From mbBlockId tracknum pos) = do
    let blockId = fromMaybe currentBlock mbBlockId
    tracks <- tryJust (mkerror
            ("block not found: " <> Parse.showBlock blockId)) $
        Map.lookup blockId blocks
    when (blockId == currentBlock && tracknum == currentTracknum) $
        Left $ mkerror $ "can't copy from the same track: "
            <> Parse.showBlock blockId <> ":" <> pretty tracknum
    tryJust (mkerror $ Parse.showBlock blockId
            <> " doesn't have track " <> pretty tracknum) $
        Lists.at tracks (tracknum - 1)
    where
    mkerror = T.err pos

-- | Resolve T.CopyFrom.
--
-- Since I use the 'resolve_blocks' memo table, they will chain, even though
-- I'd sort of rather they didn't.
resolveCopyFrom :: Maybe [ResolvedNote]
    -> [(T.Time, T.Note T.CallText (T.NPitch (Maybe T.PitchText)) T.Time)]
    -> Either T.Error [ResolvedNote]
resolveCopyFrom Nothing = mapM (traverse noFrom)
    where
    noFrom note = case T.notePitch note of
        T.NPitch pitch -> Right $ note { T.notePitch = pitch }
        T.CopyFrom -> Left $ T.err (T.notePos note) "no %f for track"
resolveCopyFrom (Just fromTrack) = concatMapM resolve
    where
    resolve (t, note) = case T.notePitch note of
        T.NPitch pitch -> Right [(t, note { T.notePitch = pitch })]
        T.CopyFrom -> copyFrom (T.notePos note) t (t + T.noteDuration note)
    copyFrom pos start end
        | null copied = Left $ T.err pos $
            "no notes to copy in range " <> pretty start <> "--" <> pretty end
        | otherwise = Right copied
        where
        copied = takeWhile ((<end) . fst) $
            dropWhile ((<start) . fst) fromTrack

-- * integrate

-- | Tracks are written in reverse order.  This is because when notation is
-- horizontal, it's natural to write higher parts above above lower parts, as
-- with staff notation.  But when notation is vertical, it's natural to put
-- higher parts on the right, by analogy to instruments that are layed out that
-- way.
reverseTracks :: Block track -> [track]
reverseTracks = reverse . _tracks

integrateBlock :: Ui.M m => Block NTrack -> m (Maybe BlockId)
integrateBlock block = do
    rulerId <- uiRuler block
    Manual.block sourceKey (_blockId block) rulerId (_blockTitle block)
        (convertTracks block)

convertTracks :: Block NTrack -> [(Convert.Track, [Convert.Track])]
convertTracks block =
    [ (convert (_note track), map convert (_controls track))
    | track <- reverseTracks block
    ]
    where
    convert track = Convert.Track
        { trackTitle = _title track
        , trackEvents = Events.ascending (_events track)
        }

sourceKey :: Block.SourceKey
sourceKey = "tscore"

-- * ui_state

scoreToUi :: MakeExternalCallDuration -> T.Score -> Either [T.Error] Ui.State
scoreToUi makeExtDur score = do
    (blocks, ScoreConfig ky) <- trackBlocks
        (UiConfig.configNamespace UiConfig.emptyConfig)
        makeExtDur score
    first (pure . T.Error Nothing . pretty) $ Ui.exec Ui.empty $ do
        mapM_ uiBlock blocks
        Ui.modifyConfig $ UiConfig.ky #= ky

-- | Turn a Block NTrack into a UI block directly.  'integrate_block' does same
-- thing, but does so via the integrate machinery.
uiBlock :: Ui.M m => Block NTrack -> m BlockId
uiBlock block = do
    let tracks = reverseTracks block
    trackIds <- fmap concat $
        forM tracks $ \(NTrack note _ controls _) ->
            forM (note : controls) $ \(Track title events) -> do
                trackId <- GenId.trackId (_blockId block)
                Ui.createTrack trackId (Track.track title events)
    rulerId <- uiRuler block
    let btracks =
            [ Block.track (Block.TId tid rulerId) Config.trackWidth
            | tid <- trackIds
            ]
    -- No longer necessary with Block.Implicit.
    -- Ui.setSkeleton block_id $ uiSkeleton tracks
    Ui.createBlock (Id.unpackId (_blockId block))
        (_blockTitle block)
        (Block.track (Block.RId rulerId) Config.rulerWidth : btracks)

-- This was obsoleted by Block.Implicit
-- uiSkeleton :: [NTrack] -> Skeleton.Skeleton
-- uiSkeleton = Skeleton.make . concat . snd . List.mapAccumL make 1
--     where
--     make tracknum track = (tracknum+len, zip ns (drop 1 ns))
--         where
--         len = length (_controls track) + 1
--         ns = [tracknum .. tracknum+len - 1]

uiRuler :: Ui.M m => Block NTrack -> m RulerId
uiRuler block = RulerUtil.replace (_blockId block) $ const $ Right $
    Ruler.meterRuler $ Meter.modifyConfig (const config) $
        RulerUtil.meterUntil meter measureDur perSection end
    where
    (config, Meter.MSection perSection measureDur meter) = _meter block
    end = trackTime $ maximum $ 0 : map _end (_tracks block)

-- * make_blocks

newtype ScoreConfig = ScoreConfig { configKy :: Text } deriving (Eq, Show)

parseBlocks :: T.Score -> Either T.Error ([Block ParsedTrack], ScoreConfig)
parseBlocks (T.Score defs) = do
    (blocks, config) <- foldM collect ([], Check.defaultConfig) defs
    pure
        ( blocks
        , ScoreConfig { configKy = Check.configKy config }
        )
    where
    collect (accum, config) (_pos, def) = do
        (blocks, config) <- interpretToplevel config def
        pure (blocks ++ accum, config)

interpretToplevel :: Check.Config -> T.Toplevel
    -> Either T.Error ([Block ParsedTrack], Check.Config)
interpretToplevel config (T.ToplevelDirective dir) =
    ([],) <$> Check.parseDirective Check.Global dir config
interpretToplevel config (T.BlockDefinition block) = do
    block <- unwrapBlockTracks block
    (block, subs) <- pure $ resolveSubBlock block
    block <- interpretBlock config False block
    subs <- mapM (interpretBlock config True) subs
    pure (block : subs, config)

unwrapBlockTracks :: T.Block T.WrappedTracks
    -> Either T.Error (T.Block (T.Tracks T.Call))
unwrapBlockTracks block = do
    tracks <- unwrapTracks $ T.blockTracks block
    pure $ block { T.blockTracks = tracks }

-- | The number of tracks must match, and their titles must match.
unwrapTracks :: T.WrappedTracks -> Either T.Error (T.Tracks T.Call)
unwrapTracks (T.WrappedTracks _ []) = Right $ T.Tracks []
unwrapTracks (T.WrappedTracks _ [tracks]) = Right tracks
unwrapTracks (T.WrappedTracks pos (T.Tracks tracks1 : wrapped))
    | Just different <- List.find (/= titles1) titles =
        Left $ T.err pos $ "wrapped track titles must match: "
            <> showt titles1 <> " /= " <> showt different
    | otherwise = Right $ T.Tracks $ zipWith merge tracks1 $ Lists.rotate $
        map T.untracks wrapped
    where
    -- [Tracks, Tracks] -> map untracks
    -- [[Track "a" a1, Track "b" b1], [Track "a" a2, Track "b" b2]] -> rotate
    -- [[Track "a" a1, Track "a" a2], [Track "b" b1, Track "b" b2]] -> merge
    -- [Track "a" (a1 ++ a2), Track "b" (b1 ++ b2)]
    merge track1 tracks = track1
        { T.trackTokens = concat $ T.trackTokens track1 : map tokens tracks
        }
    tokens track = case T.trackTokens track of
        ts@(t : _) -> coincident (T.tokenPos t) : ts
        [] -> []
    titles1 = map T.trackTitle tracks1
    titles = map (map T.trackTitle . T.untracks) wrapped
    coincident pos = T.TBarline pos T.AssertCoincident

interpretBlock :: Check.Config -> Bool -> T.Block (T.Tracks T.CallText)
    -> Either T.Error (Block ParsedTrack)
interpretBlock config isSub
        (T.Block blockId directives title (T.Tracks tracks)) = do
    blockConfig <- Check.parseDirectives Check.Block config directives
    trackConfigs <- mapM
        (Check.parseDirectives Check.Track blockConfig . T.trackDirectives)
        tracks
    trackConfigs <- pure $
        zipWith (Check.applyBlockFrom blockConfig) [1..] trackConfigs
    pure $ Block
        { _blockId = blockId
        , _blockTitle = title
        , _isSub = isSub
        , _meter = Check.meterUi $ Check.configMeter blockConfig
        , _tracks =
            [ ParsedTrack
                { trackConfig = trackConfig
                , trackKey = key
                , trackTitle = title
                , trackTokens = tokens
                , trackPos = pos
                }
            | (trackConfig, T.Track key title _ tokens pos)
                <- zip trackConfigs tracks
            ]
        }

-- * sub-blocks

-- | Replace T.SubBlock with T.CallText, and return the generated blocks.
resolveSubBlock :: T.Block (T.Tracks T.Call)
    -> (T.Block (T.Tracks T.CallText), [T.Block (T.Tracks T.CallText)])
resolveSubBlock block = Logger.runId $ do
    tracks <- resolveSubTracks (T.blockId block) (T.blockTracks block)
    pure $ block { T.blockTracks = tracks }

type ResolveM a = Logger.Logger (T.Block (T.Tracks T.CallText)) a

resolveSubTracks :: BlockId -> T.Tracks T.Call
    -> ResolveM (T.Tracks T.CallText)
resolveSubTracks blockId (T.Tracks tracks) =
    -- Since tracks will be reversed, start from the end, so the tracknums
    -- in the generated block names match up.
    T.Tracks <$> mapM resolve (zip (Lists.range_ (length tracks) (-1)) tracks)
    where
    resolve (tracknum, track) = do
        tokens <- resolveSubTokens blockId tracknum (T.trackTokens track)
        pure $ track { T.trackTokens = tokens }

resolveSubTokens :: BlockId -> TrackNum -> [T.Token T.Call pitch ndur rdur]
    -> ResolveM [T.Token T.CallText pitch ndur rdur]
resolveSubTokens blockId tracknum = fmap snd . Lists.mapAccumLM resolve [1..]
    where
    resolve (n:ns) (T.TNote pos note) = case T.noteCall note of
        T.SubBlock prefix subs -> do
            subCalls <- resolveSubTracksToCalls blockId tracknum n subs
            let call = pipeTweak prefix `Texts.unwords2` Text.unwords subCalls
            pure (ns, T.TNote pos (note { T.noteCall = call }))
        T.Call call -> pure (n:ns, T.TNote pos (note { T.noteCall = call }))
    resolve [] (T.TNote {}) = error "unreached, infinite list"
    resolve ns (T.TBarline pos bar) = pure (ns, T.TBarline pos bar)
    resolve ns (T.TRest pos rest) = pure (ns, T.TRest pos rest)

-- | Add a space so "foo|" -> "foo |", because of the xyz|[a] syntax.  But
-- don't add a space if it looks like it's not coming from that syntax, e.g.
-- "x |"[a].
pipeTweak :: Text -> Text
pipeTweak prefix
    | Just rest <- Text.stripSuffix "|" prefix,
        Just (_, c) <- Text.unsnoc rest, c `notElem` ['|', ' '] = rest <> " |"
    | otherwise = prefix

resolveSubTracksToCalls :: BlockId -> TrackNum -> TrackNum
    -> [T.Tracks T.Call] -> ResolveM [T.CallText]
resolveSubTracksToCalls parentBlockId tracknum callnum subs =
    forM (zip subs subCallnums) $ \(tracks, subCallnum) -> do
        let blockId = makeRelative parentBlockId tracknum callnum
                subCallnum
        tracks <- resolveSubTracks blockId tracks
        Logger.log $ T.Block
            { blockId = blockId
            , blockDirectives = []
            , blockTitle = ""
            , blockTracks = tracks
            }
        pure $ Eval.blockIdToCall True parentBlockId blockId
    where
    subCallnums
        | len == 1 = [Nothing]
        | len <= 26 = map (Just . Text.singleton) ['a'..'z']
        | otherwise = map (Just . ("-"<>) . showt) [1..]
        where len = length subs

makeRelative :: BlockId -> TrackNum -> TrackNum -> Maybe Text -> BlockId
makeRelative parent tracknum callnum subCallnum =
    Id.BlockId $ Id.setName relative (Id.unpackId parent)
    where
    relative = Eval.makeRelative parent $
        "t" <> showt tracknum <> "c" <> showt callnum
            <> fromMaybe "" subCallnum

isSubBlock :: Block.Block -> Bool
isSubBlock = Map.member subMeta . Block.blockMeta

-- | This marks a sub-block.
subMeta :: Text
subMeta = "is_sub"

-- * local util

noteEvent :: T.Time -> T.Note T.CallText pitch T.Time -> Event.Event
noteEvent start note =
    addStack $ Event.event (trackTime start)
        (if T.noteZeroDuration note then 0
            else trackTime (T.noteDuration note))
        (T.noteCall note)

pitchEvent :: Bool -> (T.Time, T.PitchText) -> Event.Event
pitchEvent negative (start, pitch) = addStack $
    Event.event (trackTime start) (if negative then -0 else 0) pitch

trackTime :: T.Time -> TrackTime
trackTime = realToFrac

fromTrackTime :: TrackTime -> T.Time
fromTrackTime = realToFrac

-- | A stack marks these events as being from an integration.  Event style uses
-- this, but I think that's all since I have SourceKey hardcoded.
addStack :: Event.Event -> Event.Event
addStack event =
    Event.stack_ #= Just (Event.Stack stack (Event.start event)) $ event
    where stack = Stack.add (Stack.Call sourceKey) Stack.empty
