-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.LilypondTest where
import qualified Data.List as List
import qualified Data.Text.Lazy as Text.Lazy

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Lilypond
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import qualified Perform.Lilypond.Types as Types

import Global
import Types


default_config :: Types.Config
default_config = Types.default_config

type Output = Either Process.Voices Process.Ly

-- | Assume 4/4 and no voices.
process_simple :: [String] -- ^ only include these lilypond backslash commands
    -> [Types.Event] -> Either String String
process_simple wanted events = extract_simple wanted $ process events

extract_rights :: (Show a) => [Either a String] -> String
extract_rights = unwords . map (expect_right "expected only ly")

process :: [Types.Event] -> Either String [Output]
process events = first untxt $
    Process.process Types.default_config 0 (map mkmeter meters) events
    where
    end = fromMaybe 0 $ Seq.maximum $ map Types.event_end events
    bars = ceiling (time_to_wholes end)
    meters = replicate bars "4/4"

process_meters :: [String] -> [Types.Event] -> Either String [Output]
process_meters meters = first untxt
    . Process.process Types.default_config 0 (map mkmeter meters)

-- * extract

extract_simple :: [String] -> Either String [Output] -> Either String String
extract_simple wanted = fmap extract_rights . extract_lys (Just wanted)

extract_lys :: Maybe [String]
    -- ^ if Just, only include these lilypond backslash commands
    -> Either String [Output]
    -> Either String [Either [(Process.Voice, String)] String]
extract_lys wanted =
    fmap $ unwords_right . map to_str . filter is_wanted . split_barlines
    where
    to_str = show_voices *** untxt . Types.to_lily
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

unwords_right :: [Either a String] -> [Either a String]
unwords_right = go
    where
    go [] = []
    go (Right x : xs) = Right (unwords (x:pre)) : go post
        where (pre, post) = Seq.span_while (either (const Nothing) Just) xs
    go (Left x : xs) = Left x : go xs

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
    -> Types.Event
environ_event (start, dur, pitch, env) =
    mkevent start dur pitch default_inst env

voice_event :: (RealTime, RealTime, String, Maybe Int) -> Types.Event
voice_event (start, dur, pitch, maybe_voice) =
    mkevent start dur pitch default_inst $
        maybe [] ((:[]) . ((,) Environ.voice) . TrackLang.to_val) maybe_voice

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
convert_measures :: [String] -> [Types.Event] -> Either String String
convert_measures wanted staves = case convert_staves wanted staves of
    Right [] -> Right ""
    Right [(_, [code])] -> Right code
    Right staves -> Left $ "error: expected one staff: " <> show staves
    Left err -> Left err

-- | Convert events to lilypond score.
convert_staves ::
    [String] -- ^ Only include lilypond backslash commands listed here.
    -- Or [\"ALL\"] to see them all, for debugging.
    -> [Types.Event] -> Either String [StaffGroup]
convert_staves wanted events = first untxt $
    map extract_staves <$> Lilypond.convert_staff_groups default_config 0 events
    where
    extract_staves (Lilypond.StaffGroup inst staves) =
        (untxt $ Types.inst_name inst, map show_staff staves)
    show_staff = unwords . mapMaybe (either show_voices show_ly)
    show_ly ly
        | is_wanted code = Just code
        | otherwise = Nothing
        where code = untxt $ Types.to_lily ly
    show_voices (Process.Voices voices) = Just $
        "<< " <> unwords (map show_voice voices) <> " >>"
    show_voice (v, lys) = "{ " <> show v <> ": "
        <> unwords (mapMaybe show_ly lys) <> " }"
    is_wanted ('\\':text) = takeWhile (/=' ') text `elem` wanted
        || wanted == ["ALL"]
    is_wanted _ = True

-- | Generate an entire ly score.
convert_score :: Derive.Result -> (String, [String])
convert_score = first (make_ly default_config) . partition_logs

derive_measures :: [String] -> [UiTest.TrackSpec]
    -> (Either String String, [String])
derive_measures wanted = measures wanted . derive_tracks

derive_staves :: [String] -> [UiTest.TrackSpec]
    -> (Either String [StaffGroup], [String])
derive_staves wanted = staves wanted . derive_tracks

measures :: [String] -> Derive.Result -> (Either String String, [String])
measures wanted = first (convert_measures wanted) . partition_logs

staves :: [String] -> Derive.Result -> (Either String [StaffGroup], [String])
staves wanted = first (convert_staves wanted) . partition_logs

extract :: (Types.Event -> a) -> Derive.Result -> ([a], [String])
extract f = first (map f) . partition_logs

partition_logs :: Derive.Result -> ([Types.Event], [String])
partition_logs result = (events, extract_logs (dlogs ++ logs))
    where
    (events, logs) = LEvent.partition $ Convert.convert config $
        LEvent.events_of $ Derive.r_events result
    config = Types.default_config { Types.config_quarter_duration = 1 }
    dlogs = LEvent.logs_of (Derive.r_events result)
    extract_logs = map DeriveTest.show_log . DeriveTest.quiet_filter_logs

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = derive_tracks_with_ui id id

derive_tracks_linear :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks_linear = derive_tracks_with_ui id DeriveTest.with_linear

derive_tracks_with_ui :: (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> (State.State -> State.State) -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks_with_ui with transform_ui tracks =
    derive_blocks_with_ui with transform_ui
        [(UiTest.default_block_name, tracks)]

derive_blocks :: [UiTest.BlockSpec] -> Derive.Result
derive_blocks = derive_blocks_with_ui id id

derive_blocks_with_ui :: (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> (State.State -> State.State) -> [UiTest.BlockSpec] -> Derive.Result
derive_blocks_with_ui with transform_ui blocks =
    derive_lilypond state $ with $ Derive.with_imported True Module.europe $
        Derive.with_imported True Module.ly deriver
    where
    deriver = Call.Block.eval_root_block global_transform bid
    global_transform = State.config#State.global_transform #$ state
    state = transform_ui state_
    (bid:_, state_) = DeriveTest.mkblocks blocks

derive_lilypond :: State.State -> Derive.NoteDeriver -> Derive.Result
derive_lilypond state deriver =
    extract $ CmdTest.result_val $
        CmdTest.run state CmdTest.default_cmd_state $
        Cmd.Lilypond.derive deriver
    where
    extract (Right (Just val)) = val
    extract (Right Nothing) = error "derive_lilypond: abort"
    extract (Left err) = error $ "derive_lilypond: " ++ err

make_ly :: Types.Config -> [Types.Event] -> String
make_ly config events = Text.Lazy.unpack $
    Lilypond.ly_file config "title" $ expect_right "make_ly" $
    Lilypond.extract_movements config events

-- * extract

e_note :: Types.Event -> (Types.Time, Types.Time, Text)
e_note e = (Types.event_start e, Types.event_duration e, Types.event_pitch e)

e_ly_env :: Score.Event -> [(TrackLang.ValName, String)]
e_ly_env = DeriveTest.e_environ_like ("ly-" `List.isPrefixOf`)
