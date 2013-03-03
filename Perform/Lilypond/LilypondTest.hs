module Perform.Lilypond.LilypondTest where
import qualified Data.Text as Text

import Util.Control
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Lilypond
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond


default_config :: Lilypond.Config
default_config = Lilypond.default_config

-- | (title, [Staff]) where Staff = [Measure] where Measure = String
type StaffGroup = (String, [[String]])

-- | Like 'convert_staves', but extract the converted staves to lists of
-- measures.
convert_measures :: [String] -> [Lilypond.Event] -> Either String [String]
convert_measures wanted =
    fmap (concatMap (concat . snd)) . convert_staves wanted

-- | Convert events to lilypond score.
convert_staves ::
    [String] -- ^ only include lilypond backslash commands listed here
    -> [Lilypond.Event] -> Either String [StaffGroup]
convert_staves wanted events =
    map extract_staves <$> Lilypond.convert_staff_groups default_config events
    where
    extract_staves (Lilypond.StaffGroup inst staves) =
        (Lilypond.inst_name inst, map extract_staff staves)
    extract_staff (Lilypond.Staff measures) =
        map (unwords . filter is_wanted . map Lilypond.to_lily) measures
    is_wanted ('\\':note) = takeWhile (/=' ') note `elem` wanted
    is_wanted _ = True

derive_measures :: [String] -> [UiTest.TrackSpec]
    -> (Either String [String], [String])
derive_measures wanted = measures wanted . derive

derive_staves :: [String] -> [UiTest.TrackSpec]
    -> (Either String [StaffGroup], [String])
derive_staves wanted = staves wanted . derive

measures :: [String] -> Derive.Result -> (Either String [String], [String])
measures wanted = first (convert_measures wanted) . partition

staves :: [String] -> Derive.Result -> (Either String [StaffGroup], [String])
staves wanted = first (convert_staves wanted) . partition

extract :: (Lilypond.Event -> a) -> Derive.Result -> ([a], [String])
extract f = first (map f) . partition

partition :: Derive.Result -> ([Lilypond.Event], [String])
partition result = (events, extract_logs logs)
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
    derive_ly (transform_ui state) (with deriver)
    where
    deriver = Call.Block.eval_root_block global_transform bid
    global_transform = State.config#State.global_transform #$ state
    (bid:_, state) = DeriveTest.mkblocks [(UiTest.default_block_name, tracks)]

derive_ly :: State.State -> Derive.EventDeriver -> Derive.Result
derive_ly state deriver =
    extract $ CmdTest.result_val $
        CmdTest.run state CmdTest.default_cmd_state $
        Cmd.Lilypond.derive deriver
    where
    extract (Right (Just val)) = val
    extract (Right Nothing) = error "derive_ly: abort"
    extract (Left err) = error $ "derive_ly: " ++ err

make_ly :: [Lilypond.Event] -> String
make_ly events = Text.unpack $ Text.strip $ Text.concat $ fst $
    expect_right "make_ly" $
        Lilypond.make_ly Lilypond.default_config "title" events
