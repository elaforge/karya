-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.LilypondTest where
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy

import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Lilypond
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Block as Prelude.Block
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import qualified Perform.Lilypond.Types as Types

import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


defaultConfig :: Types.Config
defaultConfig = Types.defaultConfig

extractRights :: (HasCallStack, Show a) => [Either a Text] -> Text
extractRights = Text.unwords . map expectRight

-- * extract

-- | Extract lilypond with no voices and concatenate.
extractSimple :: [Text] -> Either Text [Either Process.Voices Process.Ly]
    -> Either Text Text
extractSimple wanted = fmap extractRights . extractLys wanted

extractLys :: [Text]
    -- ^ Only include these lilypond backslash commands, or 'want_all'.
    -> Either Text [Either Process.Voices Process.Ly]
    -> Either Text [Either [(Process.Voice, Text)] Text]
extractLys wanted =
    fmap $ unwordsRight . map toStr . filter isWanted . splitBarlines
    where
    toStr = bimap showVoices Types.toLily
    showVoices (Process.Voices voices) =
        [(v, Text.unwords $ map Types.toLily lys) | (v, lys) <- voices]
    isWanted (Left _voices) = True -- TODO filter \s from voices?
    isWanted (Right ly) = wanted == wantAll
        || case ly of
            Process.LyNote {} -> True
            _ -> case Text.uncons $ Types.toLily ly of
                Just ('\\', text) -> Text.takeWhile (/=' ') text `elem` wanted
                _ -> True
    -- Split the time signature into a separate Code, instead of being bundled
    -- with LyBarlines.  This makes 'wanted' work properly.
    splitBarlines = concatMap $
        either ((:[]) . Left . splitVoices) (map Right . splitLy)
    splitLy (Process.LyBarline (Just meter)) =
        [ Process.LyBarline Nothing
        , Process.LyCode $ "\\time " <> Types.toLily meter
        ]
    splitLy ly = [ly]
    splitVoices (Process.Voices voices) = Process.Voices $
        map (second (concatMap splitLy)) voices

unwordsRight :: [Either a Text] -> [Either a Text]
unwordsRight = go
    where
    go [] = []
    go (Right x : xs) = Right (Text.unwords (x:pre)) : go post
        where (pre, post) = Lists.spanWhile (either (const Nothing) Just) xs
    go (Left x : xs) = Left x : go xs

-- * make data

mkstate :: [Text] -> Process.State
mkstate meters = Process.makeState defaultConfig 0 (map parseMeter meters)
    Process.defaultKey

parseMeter :: Text -> Meter.Meter
parseMeter = expectRight . Meter.parseMeter

-- | 1 == quarter, to be consistent with the default behaviour for
-- 'Types.real_to_time'.
mktime :: Double -> Types.Time
mktime wholes = Types.Time $ floor $
    wholes * fromIntegral Types.timePerWhole / 4

timeToWholes :: Types.Time -> Double
timeToWholes = (/ fromIntegral Types.timePerWhole) . fromIntegral

type SimpleEvent = (RealTime, RealTime, Types.Pitch)

simpleEvent :: SimpleEvent -> Types.Event
simpleEvent (start, dur, pitch) =
    mkevent start dur (Just pitch) defaultInst []

attrsEvent :: (RealTime, RealTime, Types.Pitch, Attrs.Attributes)
    -> Types.Event
attrsEvent (start, dur, pitch, attrs) = environEvent
    (start, dur, Just pitch, [(EnvKey.attributes, DeriveT.VAttributes attrs)])

environEvent ::
    (RealTime, RealTime, Maybe Types.Pitch, [(Env.Key, DeriveT.Val)])
    -> Types.Event
environEvent (start, dur, pitch, env) =
    mkevent start dur pitch defaultInst env

voiceEvent :: (RealTime, RealTime, Types.Pitch, Maybe Int) -> Types.Event
voiceEvent (start, dur, pitch, maybeVoice) =
    mkevent start dur (Just pitch) defaultInst $
        maybe [] ((:[]) . (,) EnvKey.voice . Typecheck.toVal) maybeVoice

mkevent :: RealTime -> RealTime -> Maybe Types.Pitch -> ScoreT.Instrument
    -> [(Env.Key, DeriveT.Val)] -> Types.Event
mkevent start dur pitch inst env = Types.Event
    { eventStart = Types.realToTime 1 start
    , eventDuration = Types.realToTime 1 dur
    , eventPitch = pitch
    , eventInstrument = inst
    , eventEnviron = Env.fromList env
    , eventStack = UiTest.mkstack (1, 0, 1)
    , eventClipped = False
    }

defaultInst :: ScoreT.Instrument
defaultInst = "test"

-- * pitches

a3, b3, c3, d3, e3, f3, g3 :: Types.Pitch
a3 = pitch Types.A
b3 = pitch Types.B
c3 = pitch Types.C
d3 = pitch Types.D
e3 = pitch Types.E
f3 = pitch Types.F
g3 = pitch Types.G

pitch :: Types.PitchClass -> Types.Pitch
pitch pc = Types.Pitch 3 pc Types.Natural


-- * derive

-- | (title, [Staff]) where Staff = Text
type StaffGroup = (Text, [Text])

-- | Like 'convert_staves', but expect only one staff.
convertMeasures :: [Text] -> [Types.Event] -> Either Text Text
convertMeasures wanted staves = case convertStaves wanted staves of
    Right [] -> Right ""
    Right [(_, [code])] -> Right code
    Right staves -> Left $ "error: expected one staff: " <> showt staves
    Left err -> Left err

-- | Convert events to lilypond score.
convertStaves ::
    [Text] -- ^ Only include lilypond backslash commands listed here.
    -- Or 'want_all' to see them all, for debugging.
    -> [Types.Event] -> Either Text [StaffGroup]
convertStaves wanted events =
    bimap Log.msgText (map extractStaves) $
        Lilypond.convertStaffGroups defaultConfig 0 global normal
    where
    (global, normal) = List.partition
        ((==Constants.lyGlobal) . Types.eventInstrument) events
    extractStaves (Lilypond.StaffGroup inst staves) =
        (ScoreT.instrumentName inst, map showStaff staves)
    showStaff = Text.unwords . mapMaybe (either showVoices showLy)
    showLy ly
        | isWanted code = Just code
        | otherwise = Nothing
        where code = Types.toLily ly
    showVoices (Process.Voices voices) = Just $
        "<< " <> Text.unwords (map showVoice voices) <> " >>"
    showVoice (v, lys) = "{ " <> showt v <> ": "
        <> Text.unwords (mapMaybe showLy lys) <> " }"
    isWanted text | "\\" `Text.isPrefixOf` text =
        Text.takeWhile (/=' ') (Text.drop 1 text) `elem` wanted
            || wanted == wantAll
    isWanted _ = True

-- | This is a magic code to see lilypond backslash commands.
wantAll :: [Text]
wantAll = ["ALL"]

-- | Generate an entire ly score.
convertScore :: Derive.Result -> (Text, [Text])
convertScore = first (makeLy defaultConfig) . partitionLogs

deriveMeasures :: [Text] -> [UiTest.TrackSpec] -> (Either Text Text, [Text])
deriveMeasures wanted = measures wanted . deriveTracks

deriveStaves :: [Text] -> [UiTest.TrackSpec]
    -> (Either Text [StaffGroup], [Text])
deriveStaves wanted = staves wanted . deriveTracks

measures :: [Text] -> Derive.Result -> (Either Text Text, [Text])
measures wanted = first (convertMeasures wanted) . partitionLogs

staves :: [Text] -> Derive.Result -> (Either Text [StaffGroup], [Text])
staves wanted = first (convertStaves wanted) . partitionLogs

extract :: (Types.Event -> a) -> Derive.Result -> ([a], [Text])
extract f = first (map f) . partitionLogs

partitionLogs :: Derive.Result -> ([Types.Event], [Text])
partitionLogs result = (events, extractLogs (dlogs ++ logs))
    where
    (events, logs) = LEvent.partition $ convert $
        Stream.eventsOf $ Derive.rEvents result
    dlogs = Stream.logsOf (Derive.rEvents result)
    extractLogs = map DeriveTest.showLog . DeriveTest.quietFilterLogs

convert :: [Score.Event] -> [LEvent.LEvent Types.Event]
convert = Convert.convert (defaultConfig { Types.configQuarterDuration = 1 })

deriveTracks :: [UiTest.TrackSpec] -> Derive.Result
deriveTracks = deriveTracksSetup mempty

deriveTracksLinear :: [UiTest.TrackSpec] -> Derive.Result
deriveTracksLinear = deriveTracksSetup DeriveTest.withLinear

deriveTracksSetup :: DeriveTest.Setup -> [UiTest.TrackSpec] -> Derive.Result
deriveTracksSetup setup tracks =
    deriveBlocksSetup setup [(UiTest.defaultBlockName, tracks)]

deriveBlocks :: [UiTest.BlockSpec] -> Derive.Result
deriveBlocks = deriveBlocksSetup mempty

deriveBlocksSetup :: DeriveTest.Setup -> [UiTest.BlockSpec] -> Derive.Result
deriveBlocksSetup setup blocks =
    deriveLilypond state $ DeriveTest.setupDeriver setup $
        Derive.withImported True Module.europe $
        Derive.withImported True Module.ly $
        Prelude.Block.evalRootBlock bid
    where
    state = DeriveTest.setupUi setup state_
    (bid:_, state_) = UiTest.runMkblocks blocks

deriveLilypond :: HasCallStack => Ui.State -> Derive.NoteDeriver
    -> Derive.Result
deriveLilypond state deriver =
    extract $ CmdTest.resultVal $
        CmdTest.run state CmdTest.defaultCmdState $
        Cmd.Lilypond.derive deriver
    where
    extract (Right (Just val)) = val
    extract (Right Nothing) = error "derive_lilypond: abort"
    extract (Left err) = error $ "derive_lilypond: " <> untxt err

makeLy :: Types.Config -> [Types.Event] -> Text
makeLy config events = Text.Lazy.toStrict $
    Lilypond.lyFile config "title" $ expectRight $
    Lilypond.extractMovements config events

-- * extract

eNote :: Types.Event -> (Types.Time, Types.Time, Text)
eNote e =
    ( Types.eventStart e
    , Types.eventDuration e
    , maybe "" Types.toLily $ Types.eventPitch e
    )

eLyEnv :: Score.Event -> [(Env.Key, Text)]
eLyEnv = DeriveTest.eEnvironLike ("ly-" `Text.isPrefixOf`)
