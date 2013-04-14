module Perform.Lilypond.LilypondTest where
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Lilypond
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import qualified Perform.Lilypond.Types as Types

import Types


default_config :: Lilypond.Config
default_config = Lilypond.default_config

type Output = Either Process.Voices Process.Ly

-- | Assume 4/4 and no voices.
process_simple :: [String] -- ^ only include these lilypond backslash commands
    -> [Types.Event] -> Either String String
process_simple wanted events =
    extract_simple wanted $ process meters events
    where
    end = fromMaybe 0 $ Seq.maximum $ map Types.event_end events
    bars = ceiling (time_to_wholes end)
    meters = replicate bars "4/4"

extract_rights :: (Show a) => [Either a String] -> String
extract_rights = unwords . map (expect_right "expected only ly")

process :: [String] -> [Types.Event] -> Either String [Output]
process meters = Process.process Types.default_config 0 (map mkmeter meters)

-- * extract

extract_simple :: [String] -> Either String [Output] -> Either String String
extract_simple wanted = fmap extract_rights . extract_lys (Just wanted)

extract_lys :: Maybe [String]
    -- ^ if Just, only include these lilypond backslash commands
    -> Either String [Output]
    -> Either String [Either [(Process.Voice, String)] String]
extract_lys wanted = fmap $ map to_str . filter is_wanted . split_barlines
    where
    to_str = either (Left . show_voices) (Right . untxt . Types.to_lily)
    show_voices (Process.Voices voices) =
        [(v, unwords $ map (untxt . Types.to_lily) lys) | (v, lys) <- voices]
    is_wanted (Left _voices) = True -- TODO filter \s from voices?
    is_wanted (Right ly) = case wanted of
        Nothing -> True
        Just words -> case ly of
            Process.LyNote {} -> True
            _ -> case untxt $ Types.to_lily ly of
                '\\' : text -> takeWhile (/=' ') text `elem` words
                _ -> True
    -- Split the time signature into a separate Code, instead of being bundled
    -- with Barlines.  This makes 'wanted' work properly.
    split_barlines = concatMap $
        either ((:[]) . Left . split_voices) (map Right . split_ly)
    split_ly (Process.Barline (Just meter)) =
        [ Process.Barline Nothing
        , Process.Code $ "\\time " <> Types.to_lily meter
        ]
    split_ly ly = [ly]
    split_voices (Process.Voices voices) = Process.Voices $
        map (second (concatMap split_ly)) voices

-- * make data

mkstate :: [String] -> Process.State
mkstate meters = Process.make_state Types.default_config 0 (map mkmeter meters)
    Process.default_key

mkmeter :: String -> Meter.Meter
mkmeter = expect_right "mkmeter" . Meter.parse_meter . txt

-- | 1 == quarter, to be consistent with the default behaviour for
-- 'Types.real_to_time'.
mktime :: Double -> Types.Time
mktime wholes = Types.Time $ floor $
    wholes * fromIntegral Types.time_per_whole / 4

time_to_wholes :: Types.Time -> Double
time_to_wholes = (/ fromIntegral Types.time_per_whole) . fromIntegral

type SimpleEvent = (RealTime, RealTime, String)

simple_event :: SimpleEvent -> Types.Event
simple_event (start, dur, pitch) =
    mkevent start dur pitch default_inst []

environ_event ::
    (RealTime, RealTime, String, [(TrackLang.ValName, TrackLang.Val)])
    -> Lilypond.Event
environ_event (start, dur, pitch, env) =
    mkevent start dur pitch default_inst env

voice_event :: (RealTime, RealTime, String, Maybe Int) -> Types.Event
voice_event (start, dur, pitch, maybe_voice) =
    mkevent start dur pitch default_inst $
        maybe [] ((:[]) . ((,) Constants.v_voice) . TrackLang.to_val)
            maybe_voice

mkevent :: RealTime -> RealTime -> String -> Score.Instrument
    -> [(TrackLang.ValName, TrackLang.Val)] -> Types.Event
mkevent start dur pitch inst env = Types.Event
    { Types.event_start = Types.real_to_time 1 start
    , Types.event_duration = Types.real_to_time 1 dur
    , Types.event_pitch = txt pitch
    , Types.event_instrument = inst
    , Types.event_environ = TrackLang.make_environ env
    , Types.event_stack = UiTest.mkstack (1, 0, 1)
    , Types.event_clipped = False
    }

default_inst :: Score.Instrument
default_inst = Score.Instrument "test"


-- * derive

-- | (title, [Staff]) where Staff = String
type StaffGroup = (String, [String])

-- | Like 'convert_staves', but expect only one staff.
convert_measures :: [String] -> [Lilypond.Event] -> Either String String
convert_measures wanted = fmap staff1 . convert_staves wanted
    where
    staff1 [(_, [code])] = code
    staff1 staves = error $ "expected one staff: " <> show staves

-- | Convert events to lilypond score.
convert_staves ::
    [String] -- ^ Only include lilypond backslash commands listed here.
    -- Or ["ALL"] to see them all, for debugging.
    -> [Lilypond.Event] -> Either String [StaffGroup]
convert_staves wanted events =
    map extract_staves <$> Lilypond.convert_staff_groups default_config 0 events
    where
    extract_staves (Lilypond.StaffGroup inst staves) =
        (untxt $ Lilypond.inst_name inst, map show_staff staves)
    show_staff = unwords . mapMaybe (either show_voices show_ly)
    show_ly ly
        | is_wanted code = Just code
        | otherwise = Nothing
        where code = untxt $ Lilypond.to_lily ly
    show_voices (Process.Voices voices) = Just $
        "<< " <> unwords (map show_voice voices) <> " >>"
    show_voice (v, lys) = "{ " <> show v <> ": "
        <> unwords (mapMaybe show_ly lys) <> " }"
    is_wanted ('\\':text) = takeWhile (/=' ') text `elem` wanted
        || wanted == ["ALL"]
    is_wanted _ = True

derive_measures :: [String] -> [UiTest.TrackSpec]
    -> (Either String String, [String])
derive_measures wanted = measures wanted . derive

derive_staves :: [String] -> [UiTest.TrackSpec]
    -> (Either String [StaffGroup], [String])
derive_staves wanted = staves wanted . derive

measures :: [String] -> Derive.Result -> (Either String String, [String])
measures wanted = first (convert_measures wanted) . partition_logs

staves :: [String] -> Derive.Result -> (Either String [StaffGroup], [String])
staves wanted = first (convert_staves wanted) . partition_logs

extract :: (Lilypond.Event -> a) -> Derive.Result -> ([a], [String])
extract f = first (map f) . partition_logs

partition_logs :: Derive.Result -> ([Lilypond.Event], [String])
partition_logs result = (events, extract_logs logs)
    where
    (events, logs) = LEvent.partition $ Convert.convert 1 $
        Derive.r_events result
    extract_logs = map DeriveTest.show_log . DeriveTest.quiet_filter_logs

derive :: [UiTest.TrackSpec] -> Derive.Result
derive = derive_tracks_with_ui id id

derive_linear :: [UiTest.TrackSpec] -> Derive.Result
derive_linear tracks =
    derive_tracks_with_ui id (DeriveTest.linear_skel tracks) tracks

derive_tracks_with_ui :: (Derive.EventDeriver -> Derive.EventDeriver)
    -> (State.State -> State.State) -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks_with_ui with transform_ui tracks =
    derive_lilypond (transform_ui state) (with deriver)
    where
    deriver = Call.Block.eval_root_block global_transform bid
    global_transform = State.config#State.global_transform #$ state
    (bid:_, state) = DeriveTest.mkblocks [(UiTest.default_block_name, tracks)]

derive_lilypond :: State.State -> Derive.EventDeriver -> Derive.Result
derive_lilypond state deriver =
    extract $ CmdTest.result_val $
        CmdTest.run state CmdTest.default_cmd_state $
        Cmd.Lilypond.derive deriver
    where
    extract (Right (Just val)) = val
    extract (Right Nothing) = error "derive_lilypond: abort"
    extract (Left err) = error $ "derive_lilypond: " ++ err

make_ly :: [Lilypond.Event] -> String
make_ly events = Text.unpack $ Text.strip $ Text.concat $ fst $
    expect_right "make_ly" $
        Lilypond.make_ly Lilypond.default_config "title" events
