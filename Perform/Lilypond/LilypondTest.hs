-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.LilypondTest where
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Seq as Seq

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


default_config :: Types.Config
default_config = Types.default_config

extract_rights :: (CallStack.Stack, Show a) => [Either a Text] -> Text
extract_rights = Text.unwords . map expect_right

-- * extract

-- | Extract lilypond with no voices and concatenate.
extract_simple :: [Text] -> Either Text [Either Process.Voices Process.Ly]
    -> Either Text Text
extract_simple wanted = fmap extract_rights . extract_lys wanted

extract_lys :: [Text]
    -- ^ Only include these lilypond backslash commands, or 'want_all'.
    -> Either Text [Either Process.Voices Process.Ly]
    -> Either Text [Either [(Process.Voice, Text)] Text]
extract_lys wanted =
    fmap $ unwords_right . map to_str . filter is_wanted . split_barlines
    where
    to_str = bimap show_voices Types.to_lily
    show_voices (Process.Voices voices) =
        [(v, Text.unwords $ map Types.to_lily lys) | (v, lys) <- voices]
    is_wanted (Left _voices) = True -- TODO filter \s from voices?
    is_wanted (Right ly) = wanted == want_all
        || case ly of
            Process.LyNote {} -> True
            _ -> case Text.uncons $ Types.to_lily ly of
                Just ('\\', text) -> Text.takeWhile (/=' ') text `elem` wanted
                _ -> True
    -- Split the time signature into a separate Code, instead of being bundled
    -- with LyBarlines.  This makes 'wanted' work properly.
    split_barlines = concatMap $
        either ((:[]) . Left . split_voices) (map Right . split_ly)
    split_ly (Process.LyBarline (Just meter)) =
        [ Process.LyBarline Nothing
        , Process.LyCode $ "\\time " <> Types.to_lily meter
        ]
    split_ly ly = [ly]
    split_voices (Process.Voices voices) = Process.Voices $
        map (second (concatMap split_ly)) voices

unwords_right :: [Either a Text] -> [Either a Text]
unwords_right = go
    where
    go [] = []
    go (Right x : xs) = Right (Text.unwords (x:pre)) : go post
        where (pre, post) = Seq.span_while (either (const Nothing) Just) xs
    go (Left x : xs) = Left x : go xs

-- * make data

mkstate :: [Text] -> Process.State
mkstate meters = Process.make_state default_config 0 (map parse_meter meters)
    Process.default_key

parse_meter :: Text -> Meter.Meter
parse_meter = expect_right . Meter.parse_meter

-- | 1 == quarter, to be consistent with the default behaviour for
-- 'Types.real_to_time'.
mktime :: Double -> Types.Time
mktime wholes = Types.Time $ floor $
    wholes * fromIntegral Types.time_per_whole / 4

time_to_wholes :: Types.Time -> Double
time_to_wholes = (/ fromIntegral Types.time_per_whole) . fromIntegral

type SimpleEvent = (RealTime, RealTime, Types.Pitch)

simple_event :: SimpleEvent -> Types.Event
simple_event (start, dur, pitch) =
    mkevent start dur (Just pitch) default_inst []

attrs_event :: (RealTime, RealTime, Types.Pitch, Attrs.Attributes)
    -> Types.Event
attrs_event (start, dur, pitch, attrs) = environ_event
    (start, dur, Just pitch, [(EnvKey.attributes, DeriveT.VAttributes attrs)])

environ_event ::
    (RealTime, RealTime, Maybe Types.Pitch, [(Env.Key, DeriveT.Val)])
    -> Types.Event
environ_event (start, dur, pitch, env) =
    mkevent start dur pitch default_inst env

voice_event :: (RealTime, RealTime, Types.Pitch, Maybe Int) -> Types.Event
voice_event (start, dur, pitch, maybe_voice) =
    mkevent start dur (Just pitch) default_inst $
        maybe [] ((:[]) . (,) EnvKey.voice . Typecheck.to_val) maybe_voice

mkevent :: RealTime -> RealTime -> Maybe Types.Pitch -> ScoreT.Instrument
    -> [(Env.Key, DeriveT.Val)] -> Types.Event
mkevent start dur pitch inst env = Types.Event
    { event_start = Types.real_to_time 1 start
    , event_duration = Types.real_to_time 1 dur
    , event_pitch = pitch
    , event_instrument = inst
    , event_environ = Env.from_list env
    , event_stack = UiTest.mkstack (1, 0, 1)
    , event_clipped = False
    }

default_inst :: ScoreT.Instrument
default_inst = "test"

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
convert_measures :: [Text] -> [Types.Event] -> Either Text Text
convert_measures wanted staves = case convert_staves wanted staves of
    Right [] -> Right ""
    Right [(_, [code])] -> Right code
    Right staves -> Left $ "error: expected one staff: " <> showt staves
    Left err -> Left err

-- | Convert events to lilypond score.
convert_staves ::
    [Text] -- ^ Only include lilypond backslash commands listed here.
    -- Or 'want_all' to see them all, for debugging.
    -> [Types.Event] -> Either Text [StaffGroup]
convert_staves wanted events =
    bimap Log.msg_text (map extract_staves) $
        Lilypond.convert_staff_groups default_config 0 global normal
    where
    (global, normal) = List.partition
        ((==Constants.ly_global) . Types.event_instrument) events
    extract_staves (Lilypond.StaffGroup inst staves) =
        (ScoreT.instrument_name inst, map show_staff staves)
    show_staff = Text.unwords . mapMaybe (either show_voices show_ly)
    show_ly ly
        | is_wanted code = Just code
        | otherwise = Nothing
        where code = Types.to_lily ly
    show_voices (Process.Voices voices) = Just $
        "<< " <> Text.unwords (map show_voice voices) <> " >>"
    show_voice (v, lys) = "{ " <> showt v <> ": "
        <> Text.unwords (mapMaybe show_ly lys) <> " }"
    is_wanted text | "\\" `Text.isPrefixOf` text =
        Text.takeWhile (/=' ') (Text.drop 1 text) `elem` wanted
            || wanted == want_all
    is_wanted _ = True

-- | This is a magic code to see lilypond backslash commands.
want_all :: [Text]
want_all = ["ALL"]

-- | Generate an entire ly score.
convert_score :: Derive.Result -> (Text, [Text])
convert_score = first (make_ly default_config) . partition_logs

derive_measures :: [Text] -> [UiTest.TrackSpec] -> (Either Text Text, [Text])
derive_measures wanted = measures wanted . derive_tracks

derive_staves :: [Text] -> [UiTest.TrackSpec]
    -> (Either Text [StaffGroup], [Text])
derive_staves wanted = staves wanted . derive_tracks

measures :: [Text] -> Derive.Result -> (Either Text Text, [Text])
measures wanted = first (convert_measures wanted) . partition_logs

staves :: [Text] -> Derive.Result -> (Either Text [StaffGroup], [Text])
staves wanted = first (convert_staves wanted) . partition_logs

extract :: (Types.Event -> a) -> Derive.Result -> ([a], [Text])
extract f = first (map f) . partition_logs

partition_logs :: Derive.Result -> ([Types.Event], [Text])
partition_logs result = (events, extract_logs (dlogs ++ logs))
    where
    (events, logs) = LEvent.partition $ convert $
        Stream.events_of $ Derive.r_events result
    dlogs = Stream.logs_of (Derive.r_events result)
    extract_logs = map DeriveTest.show_log . DeriveTest.quiet_filter_logs

convert :: [Score.Event] -> [LEvent.LEvent Types.Event]
convert = Convert.convert (default_config { Types.config_quarter_duration = 1 })

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = derive_tracks_setup mempty

derive_tracks_linear :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks_linear = derive_tracks_setup DeriveTest.with_linear

derive_tracks_setup :: DeriveTest.Setup -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks_setup setup tracks =
    derive_blocks_setup setup [(UiTest.default_block_name, tracks)]

derive_blocks :: [UiTest.BlockSpec] -> Derive.Result
derive_blocks = derive_blocks_setup mempty

derive_blocks_setup :: DeriveTest.Setup -> [UiTest.BlockSpec] -> Derive.Result
derive_blocks_setup setup blocks =
    derive_lilypond state $ DeriveTest.setup_deriver setup $
        Derive.with_imported True Module.europe $
        Derive.with_imported True Module.ly $
        Prelude.Block.eval_root_block bid
    where
    state = DeriveTest.setup_ui setup state_
    (bid:_, state_) = UiTest.run_mkblocks blocks

derive_lilypond :: CallStack.Stack => Ui.State -> Derive.NoteDeriver
    -> Derive.Result
derive_lilypond state deriver =
    extract $ CmdTest.result_val $
        CmdTest.run state CmdTest.default_cmd_state $
        Cmd.Lilypond.derive deriver
    where
    extract (Right (Just val)) = val
    extract (Right Nothing) = errorStack "derive_lilypond: abort"
    extract (Left err) = errorStack $ "derive_lilypond: " <> err

make_ly :: Types.Config -> [Types.Event] -> Text
make_ly config events = Text.Lazy.toStrict $
    Lilypond.ly_file config "title" $ expect_right $
    Lilypond.extract_movements config events

-- * extract

e_note :: Types.Event -> (Types.Time, Types.Time, Text)
e_note e =
    ( Types.event_start e
    , Types.event_duration e
    , maybe "" Types.to_lily $ Types.event_pitch e
    )

e_ly_env :: Score.Event -> [(Env.Key, Text)]
e_ly_env = DeriveTest.e_environ_like ("ly-" `Text.isPrefixOf`)
