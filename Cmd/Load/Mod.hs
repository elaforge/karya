-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert a 'ModT.Module' to 'Ui.State'.
module Cmd.Load.Mod where
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Num as Num

import qualified Cmd.Create as Create
import qualified Cmd.Load.ModT as ModT
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import           Derive.ShowVal (showVal)

import qualified Perform.Pitch as Pitch
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Meter.Meters as Meters
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


data State = State
    { _tempo :: !ModT.Tempo
    , _instruments :: IntMap ModT.Instrument
    } deriving (Eq, Show)

-- |
-- Make IntMap Instrument
-- map convert_block
-- make @score block using _block_order
convert :: Id.Namespace -> ModT.Module -> Either Ui.Error Ui.State
convert ns mod = Ui.exec Ui.empty $ do
    Ui.setNamespace ns
    bids <- forM blocks $ \(tracks, blockEnd, _skel) -> do
        bid <- Create.block Ui.noRuler
        rid <- Ui.createRuler (Id.unpackId bid) $
            Ruler.meterRuler $ RulerUtil.meterUntil Meters.m44 1 4 blockEnd
        Create.setBlockRuler rid bid
        forM_ tracks $ \track -> do
            tid <- Create.trackEvents bid rid 999 40 track
            Ui.setRenderStyle (Track.Line Nothing) tid
        -- Now skeleton is implicit by default.
        -- Ui.set_skeleton bid skel
        pure bid
    mapM_ Ui.destroyRuler =<< Create.orphanRulers

    let blockEnds = [t | (_, t, _) <- blocks]
    let blockMap = IntMap.fromList $ zip [0..] (zip bids blockEnds)
    scores <- forM (Map.toList (ModT._blockOrder mod)) $
        \(name_, indices) -> do
            let name = if name_ == "" then "score" else name_
            -- TODO LRuler.extract_meters
            let rulerId = Ui.noRuler
            score <- Create.namedBlock (Id.id ns name) rulerId
            Create.trackEvents score rulerId 1 40 $
                scoreTrack blockMap indices
            Create.unfittedView score
            pure score
    whenJust (Lists.head scores) Ui.setRootId
    Ui.modifyConfig $ UiConfig.ky #= ky
    where
    blocks = map (convertBlock state) (ModT._blocks mod)
    state = State
        { _tempo = ModT._defaultTempo mod
        , _instruments = ModT._instruments mod
        }

ky :: Text
ky = Text.unlines
    [ "note generator:"
    -- convert uses `i` call to set instrument.
    , "i = inst = $inst |"
    ]

-- | Make a score track with calls to the blocks.
scoreTrack :: IntMap (BlockId, TrackTime) -> [Int] -> Track.Track
scoreTrack blocks indices = Track.track ">" $ Events.fromList
    [ Event.event start (end - start) call
    | (start, end, call) <- zip3 starts (drop 1 starts) calls
    ]
    where
    starts = scanl (+) 0 durs
    (calls, durs) = unzip $ map callOf indices
    callOf idx = case IntMap.lookup idx blocks of
        Just (bid, end) -> (Id.identName bid, end)
        Nothing -> ("block" <> showt idx, 1) -- order number with no block

-- | Figure out block length from min (max lines) (first cut_block)
-- map convert_track, merge Notes
convertBlock :: State -> ModT.Block
    -> ([Track.Track], TrackTime, Skeleton.Skeleton)
convertBlock state block =
    ( map (uncurry Track.track) (concat ctracks)
    , lineStart blockLen
    , makeSkeleton ctracks
    )
    where
    ctracks = map (mergeNotes . convertTrack state blockLen)
        (ModT._tracks block)
    blockLen = fromMaybe (ModT._blockLength block) $
        Lists.minimum $ mapMaybe trackLen (ModT._tracks block)
    trackLen = Lists.head . mapMaybe cutBlock . IntMap.toAscList
    cutBlock (linenum, line)
        | ModT.CutBlock `elem` ModT._commands line = Just (linenum+1)
        | otherwise = Nothing

makeSkeleton :: [[track]] -> Skeleton.Skeleton
makeSkeleton trackGroups =
    Skeleton.make $ concat $ zipWith make tracknums (drop 1 tracknums)
    where
    tracknums = scanl (+) 1 (map length trackGroups)
    make start end = zip ts (drop 1 ts)
        where ts = Lists.range' start end 1

mergeNotes :: [Note] -> [(Text, Events.Events)]
mergeNotes notes =
    map (second Events.fromList) $ mapMaybe cleanTrack $
        noteTrack : pitchTrack : controlTracks
    where
    noteTrack = case instruments of
        [inst] ->
            (ParseTitle.instrumentToTitle inst, map (noteEvent False) notes)
        _ -> (">", map (noteEvent True) notes)
    pitchTrack = ("*", concatMap notePitches notes)
    controlTracks =
        [ (c, concatMap (Map.findWithDefault [] c . _controls) notes)
        | c <- controls
        ]
    controls = Set.toList $ Set.delete "*" $ Set.unions $
        map (Map.keysSet . _controls) notes
    instruments = Lists.unique $ map (ModT._instrumentName . _instrument) notes

cleanTrack :: (Text, [Event.Event]) -> Maybe (Text, [Event.Event])
cleanTrack (_, []) = Nothing
cleanTrack (title, events)
    | ParseTitle.isNoteTrack title || ParseTitle.isPitchTrack title =
        Just (title, events)
    -- Remove control tracks that are just 1.
    | ParseTitle.isControlTrack title && map Event.text clean == ["`0x`ff"] =
        Nothing
    | otherwise = Just (title, clean)
    where
    clean = Lists.dropWith idempotent events
    idempotent e1 e2 = "`0x`" `Text.isPrefixOf` a && a == b
        where (a, b) = (Event.text e1, Event.text e2)

noteEvent :: Bool -> Note -> Event.Event
noteEvent setInstrument n = Event.event (_start n) (_duration n) call
    where
    call = (if setInstrument then setInst else "") <> _call n
    setInst = "i " <> showVal (ModT._instrumentName (_instrument n))

notePitches :: Note -> [Event.Event]
notePitches n = Event.event (_start n) 0 (nnToCall (_pitch n))
    : Map.findWithDefault [] "*" (_controls n)

-- | TODO surely this exists elsewhere?
nnToCall :: Pitch.NoteNumber -> Text
nnToCall nn = showt (oct-1) <> steps !! step
    where
    (oct, step) = floor nn `divMod` 12
    steps = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

-- * convert

data Note = Note
    { _start :: !TrackTime
    , _duration :: !TrackTime
    , _instrument :: !ModT.Instrument
    -- | Note call text.
    , _call :: !Text
    , _pitch :: !Pitch.NoteNumber
    , _controls :: !(Map Control [Event.Event])
    } deriving (Eq, Show)

type LineNum = Int

-- |
-- - Lookup instrument.
convertTrack :: State -> LineNum -> IntMap ModT.Line -> [Note]
convertTrack state blockLen =
    go . takeWhile ((<blockLen) . fst) . IntMap.toAscList
    where
    go ((_, ModT.Line Nothing _ _) : lines) = go lines
    go ((linenum, ModT.Line (Just pitch) instnum cmds) : lines) =
        convertNote blockLen (_tempo state) instrument linenum pitch
            cmds lines
        : go lines
        where
        instrument = fromMaybe noInstrument $
            IntMap.lookup instnum (_instruments state)
        noInstrument = ModT.Instrument (ScoreT.Instrument (showt instnum))
            Nothing
    go [] = []

-- |
-- Figure out note duration: min of time until next line with Pitch>0, or 0fff,
-- or...?  Convert Pitch. Collect cmds and convert to Command.
--
-- Convert 0d to linear, 01 02 to pitch 'u' or 'd', 03 to pitch linear.
--
-- - Interpret timing cmds like 1f.
convertNote :: LineNum -> ModT.Tempo -> ModT.Instrument -> LineNum
    -> Pitch.NoteNumber -> [ModT.Command] -> [(LineNum, ModT.Line)] -> Note
convertNote blockLen tempo instrument linenum pitch cmds futureLines = Note
    { _start = start
    , _duration = end - start
    , _instrument = instrument
    , _call = noteCall (ModT._frames tempo) cmds
    , _pitch = pitch
    , _controls = Map.fromListWith (Lists.mergeOn Event.start) $
        convertCommands instrument start cmds $
        takeWhile ((==Nothing) . cutNote) futureLines
    }
    where
    start = lineStart linenum
    end = fromMaybe (lineStart blockLen) $ msum (map cutNote futureLines)

noteCall :: Int -> [ModT.Command] -> Text
noteCall frames cmds =
    Text.strip $ Text.intercalate " | " $ Maybe.catMaybes [d, r, Just ""]
    where
    d = if delay == 0 then Nothing else
        Just $ "d " <> showt delay <> "/" <> showt (frames * linesPerT) <> "t"
    r = Nothing -- TODO?
    (delay, _repeat) = fromMaybe (0, 0) $
        Lists.head [(delay, repeat) | ModT.DelayRepeat delay repeat <- cmds]

convertCommands :: ModT.Instrument -> TrackTime -> [ModT.Command]
    -> [(LineNum, ModT.Line)] -> [(Control, [Event.Event])]
convertCommands instrument start cmds lines = groupControls controls
    where
    instVol = fromMaybe 1 $ ModT._volume instrument
    volume = if null [() | ModT.Volume _ <- cmds]
        then [ModT.Volume instVol] else []
    controls = map (start,) (commandsToControls (volume ++ cmds))
        ++ concatMap lineControls lines
    lineControls (linenum, line) = map (lineStart linenum,) $
        commandsToControls (ModT._commands line)

groupControls :: [(TrackTime, (CommandType, Control, Text))]
    -> [(Control, [Event.Event])]
groupControls controls =
    map makeControl $ Lists.keyedGroupSort (key . snd) controls
    where
    makeControl (Left control, vals) =
        (control, [Event.event start 0 call | (start, (_, _, call)) <- vals])
    makeControl (Right (_k, control), vals) =
        ( control
        , [Event.event t (last run + oneLine - t) call | run@(t:_) <- runs]
        )
        where
        (_, _, call) = snd $ head vals
        runs = Lists.splitBetween (\t1 t2 -> t2 > t1 + oneLine) (map fst vals)
        oneLine = 1 / fromIntegral linesPerT
    -- All Singles are in one group so they each get a 0 dur event.
    key (Single, control, _) = Left control
    key (Grouped k, control, _call) = Right (k, control)

type Control = Text
data Call = Call !Text !Double
    deriving (Eq, Ord, Show)

-- | Single cmds emit a single 0 dur event.  Grouped cmds emit an event with
-- the given duration as long as they stay the same.
data CommandType = Single
    -- | Take a group key since VolumeSlide can group df with d.
    | Grouped !Text
    deriving (Eq, Ord, Show)

commandsToControls :: [ModT.Command] -> [(CommandType, Control, Text)]
commandsToControls = mapMaybe convert . (\xs -> map (xs,) xs)
    where
    convert (cmds, c) = case c of
        -- The slope is not accurate, and I'd need a ScoreTime slope for 'u'
        -- and 'd' to make it accurate.  Too much bother.
        ModT.VolumeSlide val -> Just
            ( Grouped (showVal val)
            , cDyn
            , Text.unwords $ case [vol | ModT.Volume vol <- cmds] of
                vol : _ ->
                    [ c <> "f"
                    , ShowVal.showHexVal vol
                    , showVal (abs val)
                    ]
                [] -> [c, showVal (abs val)]
            )
            where c = if val >= 0 then "u" else "d"
        ModT.Volume val
            | null [() | ModT.VolumeSlide _ <- cmds] ->
                Just (Single, cDyn, ShowVal.showHexVal val)
            | otherwise -> Nothing
        ModT.Command name val -> Just
            ( Single
            , "cmd"
            , "--| " <> name <> if val == 0 then "" else " " <> Num.hex 2 val
            )
        _ -> Nothing

cDyn, cPitch :: Control
cDyn = "dyn"
cPitch = "*"

cutNote :: (LineNum, ModT.Line) -> Maybe TrackTime
cutNote (linenum, line)
    | Just _ <- ModT._pitch line = Just $ lineStart linenum
    | ModT.CutNote `elem` ModT._commands line = Just $ lineStart linenum
    | ModT.CutBlock `elem` ModT._commands line = Just $ lineStart (linenum+1)
    | otherwise = Nothing

lineStart :: LineNum -> TrackTime
lineStart = (/ fromIntegral linesPerT) . ScoreTime.fromDouble . fromIntegral

-- | Lines per 1 TrackTime.
linesPerT :: Int
linesPerT = 8
