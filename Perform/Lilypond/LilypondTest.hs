module Perform.Lilypond.LilypondTest where
import qualified Data.Text as Text

import Util.Control
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.RealTime as RealTime


default_config :: Lilypond.Config
default_config = Lilypond.Config
    { Lilypond.config_quarter_duration = 1
    , Lilypond.config_dotted_rests = False
    , Lilypond.config_dynamics = []
    }

-- | (title, [Staff]) where Staff = [Measure] where Measure = [String]
type StaffGroup = (String, [[[String]]])

-- | Like 'convert_events', but extract the converted staves to lists of
-- measures.
convert_staves :: [String] -> [Lilypond.Event] -> Either String [[String]]
convert_staves wanted = fmap (concatMap (concat . snd)) . convert_events wanted

-- | Convert events to lilypond score.
convert_events ::
    [String] -- ^ only include lilypond backslash commands listed here
    -> [Lilypond.Event] -> Either String [StaffGroup]
convert_events wanted events =
    map extract_staves <$> Lilypond.convert_staff_groups default_config events
    where
    extract_staves (Lilypond.StaffGroup inst staves) =
        (Lilypond.inst_name inst, map extract_staff staves)
    extract_staff (Lilypond.Staff measures) =
        map (filter is_wanted . map Lilypond.to_lily) measures
    is_wanted ('\\':note) = takeWhile (/=' ') note `elem` wanted
    is_wanted _ = True

derive :: [UiTest.TrackSpec] -> ([Lilypond.Event], [String])
derive = derive_linear False id

derive_linear :: Bool -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> [UiTest.TrackSpec] -> ([Lilypond.Event], [String])
derive_linear linear with tracks = (ly_events, extract_logs logs)
    where
    (ly_events, logs) = LEvent.partition $ Convert.convert 1 $
        Derive.r_events (derive_ly linear with tracks)
    extract_logs = map DeriveTest.show_log . DeriveTest.quiet_filter_logs

derive_ly :: Bool -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> [UiTest.TrackSpec] -> Derive.Result
derive_ly linear with tracks =
    DeriveTest.derive_tracks_with_ui
        (with . Derive.with_val TrackLang.v_lilypond_derive
            (RealTime.seconds 1))
        (with_linear . (State.config#State.default_#State.tempo #= 1))
        tracks
    where
    with_linear = if linear then DeriveTest.linear_skel tracks else id

make_ly :: [Lilypond.Event] -> String
make_ly events = Text.unpack $ Text.strip $ Text.concat $ fst $
    expect_right "make_ly" $
        Lilypond.make_ly (Lilypond.default_config 1) "title" events
