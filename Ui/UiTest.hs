-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Ui.UiTest where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)

import qualified Util.Debug as Debug
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Parse as Parse
import qualified Util.Rect as Rect
import qualified Util.Test.Testing as Testing
import qualified Util.Texts as Texts
import qualified Util.Then as Then

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.GenId as GenId
import qualified Ui.Id as Id
import qualified Ui.Meter.Make as Meter.Make
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.Meter.Mark as Mark
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Zoom as Zoom

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Simple as Simple
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Lilypond.Constants as Lilypond.Constants
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified App.Config as Config

import           Global
import           Types


-- Test functions do things I don't want to include in non-testing code, such
-- as freely call 'error' and define convenient but incorrect orphan instances.
-- The orphans should be protected behind an ifdef in "Derive.TestInstances",
-- but I still don't want to mix test and non-test code.
--
-- UiTest is the most fundamental of the test modules, so I should only need
-- to check here.
#ifndef TESTING
#error "don't import testing modules from non-test code"
#endif


-- | (10, 50) seems to be the smallest x,y OS X will accept.  Apparently
-- fltk's sizes don't take the menu bar into account, which is about 44 pixels
-- high, so a y of 44 is the minimum.
defaultRect :: Rect.Rect
defaultRect = Rect.xywh 10 50 200 200

defaultDivider :: Block.Divider
defaultDivider = Block.Divider Color.blue

-- state

mkid :: Text -> Id.Id
mkid = Id.readShort test_ns

bid :: Text -> BlockId
bid = Id.BlockId . mkid

vid :: Text -> ViewId
vid = Id.ViewId . mkid

tid :: Text -> TrackId
tid = Id.TrackId . mkid

rid :: Text -> RulerId
rid = Id.RulerId . mkid

test_ns :: Id.Namespace
test_ns = Id.namespace "test"

defaultZoom :: Zoom.Zoom
defaultZoom = Config.zoom

-- * fmt

-- | Visualize event ranges.  This can be used with 'Testing.equal_fmt'.
fmtEvents :: [EventSpec] -> Text
fmtEvents [] = ""
fmtEvents events = Text.unlines
    [ fmtRuler minStart (eventsEnd events)
    , fmtEventsOnly minStart events
    ]
    where minStart = min 0 (eventsStart events)

fmtUiEvents :: [Event.Event] -> Text
fmtUiEvents = fmtEvents . map extractEvent

fmtEventsOnly :: Int -> [EventSpec] -> Text
fmtEventsOnly minStart = Text.unlines . map event
    where
    event (start, dur, text) = gap <> arrow <> label
        where
        gap = Text.replicate (toSpaces (min start (start + dur))) " "
        arrow
            | dur == 0 && ScoreTime.isNegative dur = "<"
            | dur == 0 = ">"
            | dur < 0 = "<" <> middle <> "|"
            | otherwise = "|" <> middle <> ">"
        middle = Text.replicate (timeToSpaces (abs dur) - 1) "-"
        label = if Text.null text then "" else " [" <> text <> "]"
    toSpaces t = offset + timeToSpaces t
    offset = timeToSpaces $ fromIntegral (abs minStart)

fmtRuler :: Int -> Int -> Text
fmtRuler start end = Text.stripEnd $ mconcatMap (space . pretty) ts
    where
    space t = t <> Text.replicate (timeToSpaces step - Text.length t) " "
    ts = Then.takeWhile1 (<end) (Lists.range_ start 1)
    step = 1

fmtStartDuration :: [(ScoreTime, ScoreTime)] -> Text
fmtStartDuration = fmtEvents . map (\(s, d) -> (s, d, ""))

fmtBlocks :: [BlockSpec] -> Text
fmtBlocks = Text.strip . Text.unlines
    . concatMap (\(block, tracks) -> [block <> ":", fmtTracks tracks])

fmtTracks :: [TrackSpec] -> Text
fmtTracks [] = ""
fmtTracks tracks = Text.unlines $
    (indent <> fmtRuler 0 (maximum (map (eventsEnd . snd) tracks)))
    : concatMap track tracks
    where
    track (title, events) = case Text.lines (fmtEventsOnly 0 events) of
        [] -> []
        x : xs -> fmtTitle title <> x : map (indent<>) xs
    indent = Text.replicate (titleLength + 2) " "
    fmtTitle title = Text.justifyLeft titleLength ' ' title <> ": "
    titleLength = maximum $ map (Text.length . fst) tracks

timeToSpaces :: ScoreTime -> Int
timeToSpaces = floor . (*4)

eventsEnd :: [(ScoreTime, ScoreTime, x)] -> Int
eventsEnd = maybe 0 ceiling . Lists.maximum .  map (\(s, d, _) -> max s (s+d))

eventsStart :: [(ScoreTime, ScoreTime, x)] -> Int
eventsStart = maybe 0 floor . Lists.minimum . map (\(s, d, _) -> min s (s+d))

-- | Extract and fmt the fst . right element.  Many DeriveTest extractors
-- return Either Error (val, [log]).
rightFst :: (a -> Text) -> Either x (a, y) -> Text
rightFst fmt = either (const "") (fmt . fst)

-- * monadic mk- functions

-- | (block_id, tracks)
--
-- If the name ends with @=ruler@, then the length of the ruler is derived from
-- the events inside, rather than being hardcoded.  This is convenient for
-- tests and lets them avoid hardcoding the default_ruler end.
--
-- Also, if the name contains @--@, the text after it becomes the block title.
type BlockSpec = (Text, [TrackSpec])

-- | (track_title, events)
type TrackSpec = (Text, [EventSpec])

-- | (start, dur, text)
type EventSpec = (ScoreTime, ScoreTime, Text)

-- | Parse a block spec, which looks like @name[=ruler] [-- title]@
parseBlockSpec :: Text -> (BlockId, Text, Bool)
parseBlockSpec spec = (bid blockId, Text.strip title, hasRuler)
    where
    (name, title) = Texts.split1 "--" spec
    (blockId, hasRuler) = maybe (Text.strip name, False) (, True) $
        Text.stripSuffix "=ruler" (Text.strip name)

-- | Often tests work with a single block, or a single view.  To make them
-- less verbose, there is a default block and view so functions can omit the
-- parameter if convenient.
defaultBlockId :: BlockId
defaultBlockId = bid defaultBlockName

defaultBlockName :: Text
defaultBlockName = "b1"

defaultViewId :: ViewId
defaultViewId = mkVid defaultBlockId

defaultRulerId :: RulerId
defaultRulerId = rid "r0"
    -- r1 would conflict with ruler automatically generated because of the
    -- =ruler suffix

-- | Return the val and state, throwing an IO error on an exception.  Intended
-- for tests that don't expect to fail here.
run :: HasCallStack => Ui.State -> Ui.StateId a -> (a, Ui.State)
run state m = case result of
    Left err -> error $ "state error: " <> show err
    Right (val, state', _) -> (val, state')
    where result = Ui.runId state m

exec :: HasCallStack => Ui.State -> Ui.StateId a -> Ui.State
exec state m = case Ui.exec state m of
    Left err -> error $ "state error: " <> prettys err
    Right state' -> state'

eval :: HasCallStack => Ui.State -> Ui.StateId a -> a
eval state m = case Ui.eval state m of
    Left err -> error $ "state error: " <> show err
    Right val -> val

runMkview :: [TrackSpec] -> ([TrackId], Ui.State)
runMkview tracks = run Ui.empty $ mkblockView (defaultBlockName, tracks)

runMkblocks :: [BlockSpec] -> ([BlockId], Ui.State)
runMkblocks = run Ui.empty . mkblocks

mkblocks :: Ui.M m => [BlockSpec] -> m [BlockId]
mkblocks blocks = mapM (fmap fst . mkblockNamed) blocks

mkviews :: Ui.M m => [BlockSpec] -> m [ViewId]
mkviews blocks = mapM mkview =<< mkblocks blocks

runMkblock :: [TrackSpec] -> ([TrackId], Ui.State)
runMkblock = run Ui.empty . mkblock

mkblock :: Ui.M m => [TrackSpec] -> m [TrackId]
mkblock = fmap snd . mkblockNamed . (defaultBlockName,)

mkblockNamed :: Ui.M m => BlockSpec -> m (BlockId, [TrackId])
mkblockNamed (spec, tracks) = do
    let (blockId, title, hasRuler) = parseBlockSpec spec
    rulerId <- if hasRuler
        then do
            let len = eventEnd tracks
                rid = Id.id test_ns ("r" <> showt len)
            ifM (Maybe.isJust <$> Ui.lookupRuler (Id.RulerId rid))
                (pure (Id.RulerId rid))
                (Ui.createRuler rid (mkruler44 len 1))
        else maybe
            (Ui.createRuler (Id.unpackId defaultRulerId) defaultRuler)
            (const (pure defaultRulerId))
                =<< Ui.lookupRuler defaultRulerId
    mkblockRulerId rulerId blockId title tracks
    where
    eventEnd :: [TrackSpec] -> Int
    eventEnd = ceiling . ScoreTime.toDouble . maximum . (0:)
        . concatMap (map (\(s, d, _) -> max s (s+d)) . snd)

mkblocksSkel :: Ui.M m => [(BlockSpec, [Skeleton.Edge])] -> m ()
mkblocksSkel blocks = forM_ blocks $ \(block, skel) -> do
    (blockId, trackIds) <- mkblockNamed block
    Ui.setSkeleton blockId (Skeleton.make skel)
    pure (blockId, trackIds)

-- | Like 'mkblock', but uses the provided ruler instead of creating its
-- own.  Important if you are creating multiple blocks and don't want
-- a separate ruler for each.
mkblockRulerId :: Ui.M m => RulerId -> BlockId -> Text -> [TrackSpec]
    -> m (BlockId, [TrackId])
mkblockRulerId rulerId blockId title tracks = do
    Ui.setNamespace test_ns
    -- Start at 1 because track 0 is the ruler.
    tids <- forM (zip [1..] tracks) $ \(i, track) ->
        Ui.createTrack (Id.unpackId (mkTidBlock blockId i))
            (makeTrack track)
    createBlock (Id.unpackId blockId) "" $ (Block.RId rulerId)
        : [Block.TId tid rulerId | tid <- tids]
    unless (Text.null title) $
        Ui.setBlockTitle blockId title
    Ui.setSkeleton blockId =<< parseSkeleton blockId
    -- This ensures that any state created via these functions will have the
    -- default midi config.  This saves some hassle since all tests can assume
    -- there are some instruments defined.
    Ui.modify setDefaultAllocations
    pure (blockId, tids)

mkblockRuler :: Ui.M m => Ruler.Ruler -> BlockId -> Text -> [TrackSpec]
    -> m (BlockId, [TrackId])
mkblockRuler ruler blockId title tracks = do
    rulerId <- Create.ruler "r" ruler
    mkblockRulerId rulerId blockId title tracks

createBlock :: Ui.M m => Id.Id -> Text -> [Block.TracklikeId] -> m BlockId
createBlock blockId title tracks =
    Ui.createConfigBlock blockId $ Block.block config title
        [Block.track tid 30 | tid <- tracks]
    where
    -- TODO use Implicit and remove the parse_skeleton stuff.
    config = Block.defaultConfig { Block.configSkeleton = Block.Explicit }

parseSkeleton :: Ui.M m => BlockId -> m Skeleton.Skeleton
parseSkeleton blockId = do
    tracks <- TrackTree.tracksOf blockId
    pure $ ParseSkeleton.defaultParser
        [ ParseSkeleton.Track (Ui.trackTracknum t) (Ui.trackTitle t)
        | t <- tracks
        ]

mkview :: Ui.M m => BlockId -> m ViewId
mkview blockId = do
    block <- Ui.getBlock blockId
    Ui.createView (Id.unpackId (mkVid blockId)) $
        Block.view block blockId defaultRect defaultZoom

mkblockView :: Ui.M m => BlockSpec -> m [TrackId]
mkblockView blockSpec = (snd <$> mkblockNamed blockSpec) <* mkview blockId
    where (blockId, _, _) = parseBlockSpec (fst blockSpec)

mkVid :: BlockId -> ViewId
mkVid blockId = Id.ViewId $ Id.id ns ("v." <> blockName)
    where (ns, blockName) = Id.unId (Id.unpackId blockId)

mkVidName :: Text -> ViewId
mkVidName = mkVid . bid

-- | Make a TrackId as mkblock does.  This is so tests can independently come
-- up with the track IDs mkblock created just by knowing their tracknum.
mkTid :: TrackNum -> TrackId
mkTid = mkTidBlock defaultBlockId

mkTidBlock :: HasCallStack => BlockId -> TrackNum -> TrackId
mkTidBlock blockId i
    | i < 1 = error $ "mk_tid_block: event tracknums start at 1: " <> show i
    | otherwise = Id.TrackId $ GenId.idsFor ns blockName "t" !! (i-1)
    where (ns, blockName) = Id.unId (Id.unpackId blockId)

mkTidName :: Text -> TrackNum -> TrackId
mkTidName = mkTidBlock . bid

-- | Get a TrackNum back out of a 'mk_tid' call.
tidTracknum :: TrackId -> TrackNum
tidTracknum = parse . Lists.takeWhileEnd Char.isDigit . untxt . Id.identName
    where
    parse "" = -1
    parse ds = read ds

-- * actions

insertEventIn :: Ui.M m => Text -> TrackNum -> (ScoreTime, ScoreTime, Text)
    -> m ()
insertEventIn blockName tracknum (pos, dur, text) =
    Ui.insertEvent (mkTidName blockName tracknum) (Event.event pos dur text)

insertEvent :: Ui.M m => TrackNum -> (ScoreTime, ScoreTime, Text) -> m ()
insertEvent = insertEventIn defaultBlockName

removeEventIn :: Ui.M m => Text -> TrackNum -> ScoreTime -> m ()
removeEventIn name tracknum pos =
    Ui.removeEventsRange (mkTidName name tracknum)
        (Events.Point pos Types.Positive)

removeEvent :: Ui.M m => TrackNum -> ScoreTime -> m ()
removeEvent = removeEventIn defaultBlockName

-- ** make specs

-- | This is a simplification of 'TrackSpec' that assumes one pitch per note.
-- It hardcodes the scale to @*@ and all the control tracks are under a single
-- note track, but in exchange it's easier to write than full TrackSpecs.
--
-- @(inst, [(t, dur, pitch)], [(control, [(t, val)])])@
--
-- If the pitch looks like \"a -- 4c\" then \"a\" is the note track's event and
-- \"4c\" is the pitch track's event.  If the pitch is missing, the empty
-- pitch event is filtered out.  This doesn't happen for the note event since
-- empty note event has a meaning.
type NoteSpec = (Text, [EventSpec], [(Text, [(ScoreTime, Text)])])

noteSpec :: NoteSpec -> [TrackSpec]
noteSpec (inst, pitches, controls) =
    -- Filter empty tracks.  Otherwise an empty pitch track will override #=.,
    -- which will be confusing.
    filter (not . null . snd) $
        noteTrack : pitchTrack : map controlTrack controls
    where
    noteTrack = (">" <> inst, [(t, dur, s) | (t, dur, (s, _)) <- track])
    pitchTrack =
        ("*", [(t, 0, p) | (t, _, (_, p)) <- track, not (Text.null p)])
    controlTrack (title, events) = (title, [(t, 0, val) | (t, val) <- events])
    track = [(t, d, split s) | (t, d, s) <- pitches]
    split s
        | "--" `Text.isInfixOf` s = (note, pitch)
        | otherwise = ("", s)
        where
        (note, pitch) = bimap Text.strip Text.strip $ Texts.split1 "--" s

-- | Abbreviation for 'note_spec' where the inst and controls are empty.
noteTrack :: [EventSpec] -> [TrackSpec]
noteTrack pitches = noteSpec ("", pitches, [])

-- | Like 'note_track', but all notes have a duration of 1.
noteTrack1 :: [Text] -> [TrackSpec]
noteTrack1 ps = noteTrack [(s, 1, p) | (s, p) <- zip (Lists.range_ 0 1) ps]

instNoteTrack :: Text -> [EventSpec] -> [TrackSpec]
instNoteTrack inst pitches = noteSpec (inst, pitches, [])

instNoteTrack1 :: Text -> [Text] -> [TrackSpec]
instNoteTrack1 title pitches = noteSpec (title, notes, [])
    where notes = [(s, 1, p) | (s, p) <- zip (Lists.range_ 0 1) pitches]

controlTrack :: [(ScoreTime, Text)] -> [EventSpec]
controlTrack ns = [(t, 0, s) | (t, s) <- ns]

regularNotes :: Int -> [TrackSpec]
regularNotes n = noteTrack $
    take n [(t, 1, p) | (t, p) <- zip (Lists.range_ 0 1) (cycle pitches)]
    where
    pitches =
        [Text.singleton o <> Text.singleton p | o <- "34567", p <- "cdefgab"]

negativeRegularNotes :: Int -> [TrackSpec]
negativeRegularNotes n = noteTrack $
    take n [(t, -1, p) | (t, p) <- zip (Lists.range_ 1 1) (cycle pitches)]
    where
    pitches =
        [Text.singleton o <> Text.singleton p | o <- "34567", p <- "cdefgab"]

-- | Parse a TrackSpec back out to a NoteSpec.
toNoteSpec :: [TrackSpec] -> [NoteSpec]
toNoteSpec =
    mapMaybe parse . Lists.splitBefore (ParseTitle.isNoteTrack . fst)
    where
    parse [] = Nothing
    parse ((inst, notes) : controls) =
        Just (Text.drop 1 inst, addPitches pitches notes, [])
        where
        pitches = maybe [] snd $
            List.find (ParseTitle.isPitchTrack . fst) controls

-- | Like 'to_note_spec' but expect just notes and pitches, no controls.
toPitchSpec :: [NoteSpec] -> [[EventSpec]]
toPitchSpec = filter (not . null) . map (\(_, events, _) -> events)

addPitches :: [EventSpec] -> [EventSpec] -> [EventSpec]
addPitches = go ""
    where
    go p pitches ns@((nt, nd, n) : notes)
        | ((pt, _, nextp) : restp) <- pitches, nt >= pt = go nextp restp ns
        | otherwise = (nt, nd, add p n) : go p pitches notes
    go _ _ [] = []
    add p n = Text.intercalate " -- " $ filter (not . Text.null) [p, n]


-- * state to spec

traceLogs :: [Log.Msg] -> a -> a
traceLogs logs val
    | null logs = val
    | otherwise = Debug.traceStr
        (Text.stripEnd $ Text.unlines $ "\tlogged:" : map Log.formatMsg logs)
        val

-- | Get the names and tracks of the default block.
extractTracks :: Ui.State -> [TrackSpec]
extractTracks = extractTracksOf defaultBlockId

extractBlocks :: Ui.State -> [BlockSpec]
extractBlocks state =
    [ (Texts.join2 " -- " (Id.identName bid) title, tracks)
    | (bid, title, tracks) <- extractBlockIds state
    ]

extractBlockId :: BlockId -> Ui.State -> Maybe [TrackSpec]
extractBlockId blockId state = Lists.head
    [block | (bid, _, block) <- extractBlockIds state, bid == blockId]

extractBlockIds :: Ui.State -> [(BlockId, Text, [TrackSpec])]
extractBlockIds state =
    [ (blockId, Block.blockTitle block, extractTracksOf blockId state)
    | (blockId, block) <- Map.toList (Ui.stateBlocks state)
    ]

extractSkeleton :: Ui.State -> [(TrackNum, TrackNum)]
extractSkeleton = maybe [] (Skeleton.flatten . Block.blockSkeleton)
    . Map.lookup defaultBlockId . Ui.stateBlocks

extractSkeletons :: Ui.State -> Map BlockId [(TrackNum, TrackNum)]
extractSkeletons =
    fmap (Skeleton.flatten . Block.blockSkeleton) . Ui.stateBlocks

extractTrackIds :: Ui.State -> [(BlockId, [TrackId])]
extractTrackIds state =
    [(blockId, tracksOf block) | (blockId, block)
        <- Map.toList (Ui.stateBlocks state)]
    where
    tracksOf = Block.trackIdsOf . map Block.tracklikeId . Block.blockTracks

-- | Like 'dump_block' but strip out everything but the tracks.
extractTracksOf :: BlockId -> Ui.State -> [TrackSpec]
extractTracksOf blockId state = tracks
    where ((_, tracks), _) = dumpBlock blockId state

dumpBlocks :: Ui.State -> [(BlockId, (BlockSpec, [Skeleton.Edge]))]
dumpBlocks state = zip blockIds (map (flip dumpBlock state) blockIds)
    where blockIds = Map.keys (Ui.stateBlocks state)

dumpBlock :: BlockId -> Ui.State -> (BlockSpec, [Skeleton.Edge])
dumpBlock blockId state =
    ((name <> if Text.null title then "" else " -- " <> title,
        map dumpTrack (Maybe.catMaybes tracks)), skel)
    where
    (idStr, title, tracks, skel) = eval state (Simple.dumpBlock blockId)
    name = snd $ Id.unId $ Id.readId idStr
    dumpTrack (_, title, events) = (title, map convert events)
    convert (start, dur, text) =
        (ScoreTime.fromDouble start, ScoreTime.fromDouble dur, text)

-- extract_rulers :: Ui.State -> [(RulerId, [Meter.Make.LabeledMark])]
-- extract_rulers =
--     map (second (map strip . Meter.ruler_meter)) . Map.toList . Ui.state_rulers
--     where
--     strip m = m
--         { Meter.Make.m_label = Meter.Make.strip_markup (Meter.Make.m_label m) }

-- * view

select :: Ui.M m => ViewId -> Sel.Selection -> m ()
select viewId sel = Ui.setSelection viewId Config.insertSelnum (Just sel)

selectPoint :: Ui.M m => ViewId -> TrackNum -> ScoreTime -> m ()
selectPoint viewId tracknum pos =
    select viewId (Sel.point tracknum pos Sel.Positive)

-- * non-monadic make_- functions

mkstack :: (TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstack (tracknum, s, e) = mkstackBlock (defaultBlockName, tracknum, s, e)

mkstackBlock :: (Text, TrackNum, ScoreTime, ScoreTime) -> Stack.Stack
mkstackBlock (block, tracknum, s, e) = Stack.fromOutermost
    [Stack.Block (bid block), Stack.Track (mkTidName block tracknum),
        Stack.Region s e]

-- ** track

makeTrack :: TrackSpec -> Track.Track
makeTrack (title, events) = Track.modifyEvents
    (Events.insert (map makeEvent events)) (emptyTrack title)

emptyTrack :: Text -> Track.Track
emptyTrack title = (Track.track title Events.empty)
    { Track.trackRender = Track.noRender }

-- ** event

makeEvent :: EventSpec -> Event.Event
makeEvent (start, dur, text) = Event.event start dur text

extractEvent :: Event.Event -> EventSpec
extractEvent e = (Event.start e, Event.duration e, Event.text e)

-- ** ruler

defaultRuler :: Ruler.Ruler
defaultRuler = mkruler44 32 1

defaultBlockEnd :: ScoreTime
defaultBlockEnd = Ruler.timeEnd defaultRuler

-- | TimeStep to step by 1 ScoreTime on the default ruler.
step1 :: TimeStep.TimeStep
step1 = TimeStep.timeStep
    (TimeStep.AbsoluteMark TimeStep.AllMarklists Meter.H)

-- | Create a ruler with a 4/4 "meter" marklist with the given number of marks
-- at the given distance.  Marks are rank [1, 2, 2, ...].
--
-- The end of the ruler should be at marks*dist.  An extra mark is created
-- since marks start at 0.
mkruler44 :: Int -> ScoreTime -> Ruler.Ruler
mkruler44 marks dist =
    mkruler 4 (fromIntegral marks * dist) (Meter.repeat 4 Meter.T)

mkruler :: TrackTime -> TrackTime -> Meter.AbstractMeter -> Ruler.Ruler
mkruler measureDur end meter =
    Ruler.meterRuler $ RulerUtil.meterUntil meter measureDur 4 end

-- | This makes a meter without a Meter.Meter, which cmds that look at Meter
-- will be confused by.
mkrulerMarks :: [(TrackTime, Mark)] -> Ruler.Ruler
mkrulerMarks marks =
    Ruler.modifyMarklists (Map.insert Ruler.meterName (Nothing, mlist))
        Ruler.empty
    where mlist = Mark.marklist $ map (second (uncurry mkmark)) marks

mkrulerRanks :: [(TrackTime, RankNum)] -> Ruler.Ruler
mkrulerRanks = mkrulerMarks . map (second (, ""))

eMark :: Mark.Mark -> Mark
eMark m = (fromEnum (Mark.markRank m), markName m)

markName :: Mark.Mark -> Mark.Label
markName = Meter.Make.stripMarkup . Mark.markName

mkmark :: RankNum -> Mark.Label -> Mark.Mark
mkmark rank label = Mark.Mark
    { markRank = toEnum rank
    , markWidth = 0
    , markColor = Color.black
    , markName = label
    , markNameZoomLevel = 0
    , markZoomLevel = 0
    }

eRulers :: Ui.State -> [(Text, Text)]
eRulers state =
    [ (Id.identName bid, eRuler bid state)
    | bid <- Map.keys (Ui.stateBlocks state)
    ]

eRuler :: BlockId -> Ui.State -> Text
eRuler bid state = eval state $
    Text.unwords . map (snd . snd) . rulerMarks <$>
        (Ui.getRuler =<< Ui.rulerOf bid)

type Mark = (RankNum, Mark.Label)
type RankNum = Int

eMeters :: Ui.State -> [(Text, [(TrackTime, Mark)])]
eMeters state =
    [ (Id.identName bid, eMeter bid state)
    | bid <- Map.keys (Ui.stateBlocks state)
    ]

eMeter :: BlockId -> Ui.State -> [(TrackTime, Mark)]
eMeter bid state = eval state $
    rulerMarks <$> (Ui.getRuler =<< Ui.rulerOf bid)

rulerMarks :: Ruler.Ruler -> [(TrackTime, Mark)]
rulerMarks =
    map (second eMark) . maybe mempty (Mark.toList . snd)
    . Map.lookup Ruler.meterName
    . Ruler.rulerMarklists

meterZoom :: Double -> Meter.Meter -> [(TrackTime, Mark)]
meterZoom zoom = map (second eMark)
    . filter ((<= zoom) . Mark.markNameZoomLevel . snd)
    . Meter.Make.makeMeasures

meterMarklist :: Double -> Meter.Meter -> [(TrackTime, Mark.Label)]
meterMarklist zoom = map (second snd) . meterZoom zoom

-- * allocations

midiConfig :: [Midi.Channel] -> Patch.Config
midiConfig chans = Patch.config [((wdev, chan), Nothing) | chan <- chans]

setDefaultAllocations :: Ui.State -> Ui.State
setDefaultAllocations = setInstruments defaultInstruments

setInstruments :: Text -> Ui.State -> Ui.State
setInstruments ky =
    (Ui.config#UiConfig.allocations #= allocs)
    . (Ui.config#UiConfig.ky #= Instruments.instrumentSection <> ":\n" <> ky)
    where
    allocs = either (error . untxt) id $
        Parse.parse Instruments.pInstruments ky

defaultInstruments :: Text
defaultInstruments =
    ">i s/1 [ms] wdev 1\n\
    \>i1 s/1 [ms] wdev 2..4\n\
    \>i2 s/2 [ms] wdev 5\n\
    \>i3 s/3 [ms] wdev 6\n"

defaultAllocations :: UiConfig.Allocations
defaultAllocations = either (error . untxt) id $
    Parse.parse Instruments.pInstruments defaultInstruments

modifyMidiConfig :: HasCallStack => ScoreT.Instrument
    -> (Patch.Config -> Patch.Config)
    -> UiConfig.Allocations -> UiConfig.Allocations
modifyMidiConfig inst modify =
    Testing.expectRight . UiConfig.modifyAllocation inst modifyAlloc
    where
    modifyAlloc alloc = do
        config <- justErr ("not a midi alloc: " <> pretty inst) $
            UiConfig.midiConfig (UiConfig.allocBackend alloc)
        pure $ alloc
            { UiConfig.allocBackend = UiConfig.Midi (modify config) }

i1, i2, i3 :: ScoreT.Instrument
i1 = ScoreT.Instrument "i1"
i2 = ScoreT.Instrument "i2"
i3 = ScoreT.Instrument "i3"

wdev :: Midi.WriteDevice
wdev = Midi.writeDevice "wdev"

-- * instrument db

defaultDb :: Cmd.InstrumentDb
defaultDb = makeDb [("s", map makePatch ["1", "2", "3"])]

makePatch :: InstT.Name -> Patch.Patch
makePatch name = Patch.patch (-2, 2) name

makeDb :: [(Text, [Patch.Patch])] -> Cmd.InstrumentDb
makeDb synthPatches = fst $ Inst.db $ ly : map make synthPatches
    where
    make (name, patches) = makeSynth name (map MidiInst.makePatch patches)
    -- Always add ly-global, lilypond tests want it, and it doesn't hurt the
    -- other ones.
    ly = Lilypond.Constants.lySynth Cmd.emptyCode

makeDb1 :: MidiInst.Patch -> Cmd.InstrumentDb
makeDb1 patch = fst $ Inst.db [makeSynth "s" [patch]]

makeSynth :: InstT.SynthName -> [MidiInst.Patch] -> MidiInst.Synth
makeSynth name patches = MidiInst.synth name "Test Synth" patches

-- * misc

btrack :: TrackId -> Block.Track
btrack trackId = Block.track (Block.TId trackId defaultRulerId) 30
