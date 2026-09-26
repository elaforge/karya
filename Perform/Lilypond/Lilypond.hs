-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
-- | Convert from Score events to a lilypond score.
module Perform.Lilypond.Lilypond where
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import qualified Perform.Lilypond.Types as Types

import           Global


type Error = Text

-- * config

-- | Lilypond code inserted inside the toplevel paper block.
paperConfig :: [Text]
paperConfig =
    -- Print page numbers centered at the bottom of the page, instead of
    -- in the upper right and upper left corners.
    [ "print-page-number = ##t"
    , "print-first-page-number = ##t"
    , "oddHeaderMarkup = \\markup \\null"
    , "evenHeaderMarkup = \\markup \\null"
    , "oddFooterMarkup = \\markup {"
    , "\\fill-line {"
    , "    \\on-the-fly #print-page-number-check-first"
    , "    \\fromproperty #'page:page-number-string"
    , "    }"
    , "}"
    , "evenFooterMarkup = \\oddFooterMarkup"
    ]

-- * output

type Title = Text

-- | Format a list Movements into a complete lilypond file.
lyFile :: Types.Config -> Title -> [Movement] -> Lazy.Text
lyFile config title movements = runOutput $ do
    outputs
        -- I only need 2.18.2 due to using \tuplet.  If it's a problem, I can
        -- easily switch back to 2.14.2 by switching back to \times.
        [ "\\version" <+> str "2.18.2"
        , "\\language" <+> str "english"
        -- I'm not using it, and it increases file size a lot.
        , "\\pointAndClickOff"
        , "\\include \"ly/lib.ily\""
        , "\\header { title =" <+> str title <+> "tagline = \"\" }"
        , ""
        , "\\paper {"
        ]
    outputs paperConfig
    outputs ["}", ""]
    mapM_ writeMovement movements
    where
    writeMovement (title, staffGroups) = do
        output "\\bookpart {\n"
        unless (Text.null title) $
            output $ "\\header { piece =" <+> str title <+> "}\n"
        output "\\score {\n"
        output "<<\n"
        mapM_ writeStaves $ sortStaves (Types.configStaves config)
            staffGroups
        output ">>\n}\n"
        output "}\n\n"
    writeStaves (StaffGroup _ staves, config)
        | not (Types.staffDisplay config) = pure ()
        | Types.staffAddBassStaff config =
            writeStaffGroup "StaffGroup" config $ \config -> do
                case staves of
                    staff : staves -> do
                        normalStaff config (Just "up") staff
                        mapM_ (normalStaff config Nothing) staves
                    [] -> pure ()
                whenJust (Lists.head staves) $ writeEmptyStaff config
        | otherwise = case staves of
            [staff] -> normalStaff config Nothing staff
            [up, down] -> writeStaffGroup "PianoStaff" config $ \config -> do
                normalStaff config (Just "up") up
                normalStaff config (Just "down") down
            _ -> writeStaffGroup "PianoStaff" config $ \config ->
                mapM_ (normalStaff config Nothing) staves
    writeStaffGroup name config contents = do
        outputs
            [ "\\new " <> name <> " <<"
            , lySet (name <> ".instrumentName") (Types.staffLong config)
            , lySet (name <> ".shortInstrumentName") (Types.staffShort config)
            ]
        contents $ config { Types.staffLong = "", Types.staffShort = "" }
        output ">>\n\n"
    normalStaff config maybeName lys =
        writeStaff config maybeName Nothing (mapM_ writeVoiceLy lys)

-- | Convert ly code to all hidden rests, and emit an empty staff with a bass
-- clef.  This is so xstaff commands can later cause it to appear, only when
-- needed.
writeEmptyStaff :: Types.StaffConfig -> [Either Process.Voices Process.Ly]
    -> Output ()
writeEmptyStaff config_ lys =
    writeStaff config (Just "down") (Just "\\with { \\RemoveAllEmptyStaves }")$
        mapM_ writeLy $
            Process.LyCode "\\clef bass" : Process.convertToRests lys
    where
    config = config_ { Types.staffCode = Types.staffCode config_ }

str :: Text -> Text
str = Types.toLily

(<+>) :: Text -> Text -> Text
x <+> y = x <> " " <> y
infixr 6 <+> -- same as <>

lySet :: Text -> Text -> Text
lySet name val = "\\set" <+> name <+> "=" <+> str val

writeStaff :: Types.StaffConfig -> Maybe Text -> Maybe Text -> Output ()
    -> Output ()
writeStaff config maybeName context writeContents = do
    output $ "\\new Staff " <> maybe "" (("= "<>) . str) maybeName
        <> maybe "" (" "<>) context <+> "{\n"
    unless (Text.null (Types.staffLong config)) $
        outputs [lySet "Staff.instrumentName" (Types.staffLong config)]
    unless (Text.null (Types.staffShort config)) $
        outputs [lySet "Staff.shortInstrumentName" (Types.staffShort config)]
    outputs Types.globalStaffCode
    outputs (Types.staffCode config)
    output "{\n"
    setBar 1
    writeContents
    output "\n} }\n\n"

writeVoiceLy :: Either Process.Voices Process.Ly -> Output ()
writeVoiceLy (Left (Process.Voices voices)) = do
    output "<<\n  "
    start <- State.gets outputBar
    bars <- mapM (\v -> setBar start >> writeVoice v) voices
    output ">> \\oneVoice\n  "
    unless (allEqual bars) $
        -- Lilypond will also complain.
        output $ "% WARNING: voices have different numbers of bars: "
            <> showt bars
    setBar (fromMaybe start (Lists.head bars))
    where
    allEqual [] = True
    allEqual (x:xs) = all (==x) xs
writeVoiceLy (Right ly) = writeLy ly

writeLy :: Process.Ly -> Output ()
writeLy ly@(Process.LyBarline {}) = do
    bar <- State.gets outputBar
    stack <- State.gets outputLastStack
    output $ " " <> Types.toLily ly <> " % " <> showStack stack <> showt bar
        <> "\n"
    setBar (bar+1)
writeLy ly = do
    output $ " " <> Types.toLily ly
    case ly of
        Process.LyNote note -> State.modify $ \state -> state
            { outputLastStack = Process.noteStack note }
        _ -> pure ()

showStack :: Maybe Stack.UiFrame -> Text
showStack (Just stack) =  Stack.unparseUiFrame stack <> "; "
showStack Nothing = ""

writeVoice :: (Process.Voice, [Process.Ly]) -> Output Int
writeVoice (voice, lys) = do
    output $ (if voice == Process.VoiceOne then "" else "\\new Voice ")
        <> "{ " <> Types.toLily voice <> "\n  "
    mapM_ writeLy lys
    output "} "
    State.gets outputBar

sortStaves :: [(ScoreT.Instrument, Types.StaffConfig)] -> [StaffGroup]
    -> [(StaffGroup, Types.StaffConfig)]
sortStaves instConfigs = map lookupName . Lists.sortOn instKey
    where
    lookupName staff = case lookup (instOf staff) instConfigs of
        Nothing -> (staff, Types.defaultStaffConfig (instOf staff))
        Just config -> (staff, config)
    instKey staff =
        maybe (1, 0) (0,) $ List.elemIndex (instOf staff)
            (map fst instConfigs)
    instOf (StaffGroup inst _) = inst

type Output a = State.State OutputState a

runOutput :: Output a -> Lazy.Text
runOutput m = Builder.toLazyText (outputText state)
    where state = State.execState m (OutputState mempty 1 Nothing)

data OutputState = OutputState
    { outputText :: Builder.Builder
    , outputBar :: Int
    , outputLastStack :: Maybe Stack.UiFrame
    } deriving (Show)

outputs :: [Text] -> Output ()
outputs = output . Text.unlines

output :: Text -> Output ()
output text = State.modify $ \state -> state
    { outputText = outputText state <> Builder.fromText text }

setBar :: Int -> Output ()
setBar n = State.modify $ \state -> state { outputBar = n }


-- * convert events

type Movement = (Title, [StaffGroup])

-- | If the staff group has >1 staff, it is bracketed as a grand staff.
data StaffGroup =
    StaffGroup ScoreT.Instrument [[Either Process.Voices Process.Ly]]
    deriving (Show)

instance Pretty StaffGroup where
    format (StaffGroup inst staves) = Pretty.record "StaffGroup"
        [ ("inst", Pretty.format inst)
        , ("staves", Pretty.format staves)
        ]

explicitMovements :: Types.Config -> [(Title, [Types.Event])]
    -> Either Log.Msg [Movement]
explicitMovements config sections = forM sections $ \(title, events) -> do
    let (global, normal) = List.partition
            ((==Constants.lyGlobal) . Types.eventInstrument) events
    -- TODO filter out the v_movements since I won't be using them?
    staves <- convertStaffGroups config 0 global normal
    pure (title, staves)

extractMovements :: Types.Config -> [Types.Event] -> Either Log.Msg [Movement]
extractMovements config events = do
    let (global, normal) = List.partition
            ((==Constants.lyGlobal) . Types.eventInstrument) events
    (movements, global) <- either warn pure $
        partitionKey Constants.vMovement global
    forM (splitMovements movements normal) $ \(start, title, events) -> do
        staves <- convertStaffGroups config start global events
        pure (title, staves)

-- | Group a stream of events into individual staves based on instrument, and
-- for keyboard instruments, left or right hand.  Then convert each staff of
-- Events to Notes, divided up into measures.
convertStaffGroups :: Types.Config -> Types.Time -> [Types.Event]
    -> [Types.Event] -> Either Log.Msg [StaffGroup]
convertStaffGroups config start global events = do
    let staffGroups = splitEvents events
    let staffEnd = fromMaybe 0 $ Lists.maximum (map Types.eventEnd events)
    (meters, global) <- either warn pure $ parseMeters start staffEnd global
    -- It would be nicer to partition_on Process.free_code so I don't have to
    -- re-parse it in each Process.process, but then process would have to
    -- take an Either FreeCode Event or something.
    let (globalCode, remain) = List.partition
            (not . null . Constants.environFreeCode . Types.eventEnviron)
            global
    unless (null remain) $
        warn $ "leftover ly-global events: " <> pretty global
    forM staffGroups $ \(inst, staves) ->
        staffGroup config start meters inst $
            map (distributeGlobal globalCode inst) staves

-- | Split events by instrument, and if they have 'EnvKey.hand', further split
-- into right and left hand.
splitEvents :: [Types.Event] -> [(ScoreT.Instrument, [[Types.Event]])]
splitEvents events =
    [ (inst, Lists.groupSort (lookupHand . Types.eventEnviron) events)
    | (inst, events) <- byInst
    ]
    where
    byInst = Lists.keyedGroupSort Types.eventInstrument events
    lookupHand environ = case Env.getVal EnvKey.hand environ of
        Right (val :: Text)
            | val == "r" || val == "right" -> 0
            | val == "l" || val == "left" -> 1
            | otherwise -> 2
        _ -> 0

-- | Right hand goes at the top, left hand goes at the bottom.  Any other hands
-- go below that.  Events that are don't have a hand are assumed to be in the
-- right hand.
staffGroup :: Types.Config -> Types.Time -> [Meter.Meter] -> ScoreT.Instrument
    -> [[Types.Event]] -> Either Log.Msg StaffGroup
staffGroup config start meters inst staves = do
    staffMeasures <- forM (zip [1..] staves) $ \(i, es) ->
        annotate i $ Process.process config start meters es
    pure $ StaffGroup inst staffMeasures
    where
    annotate i = first (Log.addPrefix (pretty inst <> ":" <> showt i))

-- | Global FreeCode events get distributed to all staves.
distributeGlobal :: [Types.Event] -> ScoreT.Instrument -> [Types.Event]
    -> [Types.Event]
distributeGlobal codes inst =
    Lists.mergeOn Types.eventStart
        (map (\e -> e { Types.eventInstrument = inst }) codes)

-- ** movements

-- | Use the movement break points to group the events by movement.  The events
-- are not shifted in time, so each movement has to start at the proper offset.
-- The reason is that certain calls, like tuplet, bake in lilypond code, and it
-- will be wrong if the events change position.
splitMovements :: [(Types.Time, Title)] -> [Types.Event]
    -> [(Types.Time, Title, [Types.Event])]
splitMovements movements =
    filter (not . null . eventsOf)
    . split (Lists.zipNext ((0, "") : movements))
    where
    split (((start, title), Just (next, _)) : movements) events =
        (start, title, pre) : split movements post
        where (pre, post) = break ((>=next) . Types.eventStart) events
    split (((start, title), Nothing) : _) events = [(start, title, events)]
    split [] events = [(0, "", events)]
    eventsOf (_, _, x) = x

-- ** meter

-- | Extract Meters from the Events, and emit one per measure.
parseMeters :: Types.Time -> Types.Time -> [Types.Event]
    -> Either Error ([Meter.Meter], [Types.Event])
parseMeters start staffEnd events = do
    (meters, remain) <- partitionKey Constants.vMeter events
    meters <- mapM (\(pos, meter) -> (pos,) <$> Meter.parseMeter meter) meters
    pure (generate start Meter.defaultMeter meters, remain)
    where
    generate prev prevMeter ((pos, meter) : meters) | pos < staffEnd =
        replicate measures prevMeter ++ generate next meter meters
        where
        measures = measuresIn (pos-prev) prevMeter
        next = prev + fromIntegral measures * Meter.measureTime prevMeter
    generate prev meter _ = replicate (measuresIn (staffEnd-prev) meter) meter
    -- If you try to change the meter in the middle of a meter, it rounds up to
    -- the next barline.  If you put multiple meter changes before the barline,
    -- only the first one is accepted.  TODO arguably it should be the last.
    -- TODO or an error.
    measuresIn dur meter = max 0 $ ceiling $
        fromIntegral dur / fromIntegral (Meter.measureTime meter)

-- * util

warn :: HasCallStack => Text -> Either Log.Msg a
warn = Left . Log.msg Log.Warn Nothing

partitionKey :: EnvKey.Key -> [Types.Event]
    -> Either Error ([(Types.Time, Text)], [Types.Event])
partitionKey key = go
    where
    go [] = pure ([], [])
    go (e : es) = case Env.checkedVal2 key (Types.eventEnviron e) of
        Nothing -> second (e:) <$> go es
        Just (Left err) -> Left $ pretty Constants.lyGlobal <> " event at "
            <> pretty pos <> err
        Just (Right val) -> first ((pos, val) :) <$> go es
        where pos = Types.eventStart e
