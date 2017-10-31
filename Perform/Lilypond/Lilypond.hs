-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert from Score events to a lilypond score.
module Perform.Lilypond.Lilypond where
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import qualified Perform.Lilypond.Types as Types

import Global


type Error = Text

-- * config

-- | Lilypond code inserted inside the toplevel paper block.
paper_config :: [Text]
paper_config =
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
ly_file :: Types.Config -> Title -> [Movement] -> Lazy.Text
ly_file config title movements = run_output $ do
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
    outputs paper_config
    outputs ["}", ""]
    mapM_ write_movement movements
    where
    write_movement (title, staff_groups) = do
        output "\\bookpart {\n"
        unless (Text.null title) $
            output $ "\\header { piece =" <+> str title <+> "}\n"
        output "\\score {\n"
        output "<<\n"
        mapM_ write_staves $ sort_staves (Types.config_staves config)
            staff_groups
        output ">>\n}\n"
        output "}\n\n"
    write_staves (StaffGroup _ staves, config)
        | not (Types.staff_display config) = return ()
        | Types.staff_add_bass_staff config =
            write_staff_group "StaffGroup" config $ \config -> do
                case staves of
                    staff : staves -> do
                        normal_staff config (Just "up") staff
                        mapM_ (normal_staff config Nothing) staves
                    [] -> return ()
                whenJust (Seq.head staves) $ write_empty_staff config
        | otherwise = case staves of
            [staff] -> normal_staff config Nothing staff
            [up, down] -> write_staff_group "PianoStaff" config $ \config -> do
                normal_staff config (Just "up") up
                normal_staff config (Just "down") down
            _ -> write_staff_group "PianoStaff" config $ \config ->
                mapM_ (normal_staff config Nothing) staves
    write_staff_group name config contents = do
        outputs
            [ "\\new " <> name <> " <<"
            , ly_set (name <> ".instrumentName") (Types.staff_long config)
            , ly_set (name <> ".shortInstrumentName") (Types.staff_short config)
            ]
        contents $ config { Types.staff_long = "", Types.staff_short = "" }
        output ">>\n\n"
    normal_staff config maybe_name lys =
        write_staff config maybe_name Nothing (mapM_ write_voice_ly lys)

-- | Convert ly code to all hidden rests, and emit an empty staff with a bass
-- clef.
write_empty_staff :: Types.StaffConfig -> [Either Process.Voices Process.Ly]
    -> Output ()
write_empty_staff config_ lys =
    write_staff config (Just "down") (Just "\\RemoveEmptyStaves") $
        mapM_ write_ly $
            Process.LyCode "\\clef bass" : Process.convert_to_rests lys
    where
    config = config_ { Types.staff_code = Types.staff_code config_ ++ [code] }
    -- Normally RemoveEmptyStaves won't remove the staff from the first system,
    -- even if it's empty.  This causes the first system's staff to also be
    -- removed.
    code = "\\override Staff.VerticalAxisGroup.remove-first = ##t"

str :: Text -> Text
str = Types.to_lily

(<+>) :: Text -> Text -> Text
x <+> y = x <> " " <> y
infixr 6 <+> -- same as <>

ly_set :: Text -> Text -> Text
ly_set name val = "\\set" <+> name <+> "=" <+> str val

write_staff :: Types.StaffConfig -> Maybe Text -> Maybe Text -> Output ()
    -> Output ()
write_staff config maybe_name context write_contents = do
    output $ "\\new Staff " <> maybe "" (("= "<>) . str) maybe_name
        <> maybe "" (" "<>) context <+> "{\n"
    unless (Text.null (Types.staff_long config)) $
        outputs [ly_set "Staff.instrumentName" (Types.staff_long config)]
    unless (Text.null (Types.staff_short config)) $
        outputs [ly_set "Staff.shortInstrumentName" (Types.staff_short config)]
    outputs Types.global_staff_code
    outputs (Types.staff_code config)
    output "{\n"
    set_bar 1
    write_contents
    output "\n} }\n\n"

write_voice_ly :: Either Process.Voices Process.Ly -> Output ()
write_voice_ly (Left (Process.Voices voices)) = do
    output "<<\n  "
    start <- State.gets output_bar
    bars <- mapM (\v -> set_bar start >> write_voice v) voices
    output ">> \\oneVoice\n  "
    unless (all_equal bars) $
        -- Lilypond will also complain.
        output $ "% WARNING: voices have different numbers of bars: "
            <> showt bars
    set_bar (fromMaybe start (Seq.head bars))
    where
    all_equal [] = True
    all_equal (x:xs) = all (==x) xs
write_voice_ly (Right ly) = write_ly ly

write_ly :: Process.Ly -> Output ()
write_ly ly@(Process.LyBarline {}) = do
    bar <- State.gets output_bar
    stack <- State.gets output_last_stack
    output $ " " <> Types.to_lily ly <> " % " <> show_stack stack <> showt bar
        <> "\n"
    set_bar (bar+1)
write_ly ly = do
    output $ " " <> Types.to_lily ly
    case ly of
        Process.LyNote note -> State.modify $ \state -> state
            { output_last_stack = Process.note_stack note }
        _ -> return ()

show_stack :: Maybe Stack.UiFrame -> Text
show_stack (Just stack) =  Stack.unparse_ui_frame stack <> "; "
show_stack Nothing = ""

write_voice :: (Process.Voice, [Process.Ly]) -> Output Int
write_voice (voice, lys) = do
    output $ (if voice == Process.VoiceOne then "" else "\\new Voice ")
        <> "{ " <> Types.to_lily voice <> "\n  "
    mapM_ write_ly lys
    output "} "
    State.gets output_bar

sort_staves :: [(Score.Instrument, Types.StaffConfig)] -> [StaffGroup]
    -> [(StaffGroup, Types.StaffConfig)]
sort_staves inst_configs = map lookup_name . Seq.sort_on inst_key
    where
    lookup_name staff = case lookup (inst_of staff) inst_configs of
        Nothing -> (staff, Types.default_staff_config (inst_of staff))
        Just config -> (staff, config)
    inst_key staff =
        maybe (1, 0) (0,) $ List.elemIndex (inst_of staff)
            (map fst inst_configs)
    inst_of (StaffGroup inst _) = inst

type Output a = State.State OutputState a

run_output :: Output a -> Lazy.Text
run_output m = Builder.toLazyText (output_text state)
    where state = State.execState m (OutputState mempty 1 Nothing)

data OutputState = OutputState {
    output_text :: !Builder.Builder
    , output_bar :: !Int
    , output_last_stack :: !(Maybe Stack.UiFrame)
    } deriving (Show)

outputs :: [Text] -> Output ()
outputs = output . Text.unlines

output :: Text -> Output ()
output text = State.modify $ \state -> state
    { output_text = output_text state <> Builder.fromText text }

set_bar :: Int -> Output ()
set_bar n = State.modify $ \state -> state { output_bar = n }


-- * convert events

type Movement = (Title, [StaffGroup])

-- | If the staff group has >1 staff, it is bracketed as a grand staff.
data StaffGroup =
    StaffGroup Score.Instrument [[Either Process.Voices Process.Ly]]
    deriving (Show)

instance Pretty StaffGroup where
    format (StaffGroup inst staves) = Pretty.record "StaffGroup"
        [ ("inst", Pretty.format inst)
        , ("staves", Pretty.format staves)
        ]

explicit_movements :: Types.Config -> [(Title, [Types.Event])]
    -> Either Log.Msg [Movement]
explicit_movements config sections = forM sections $ \(title, events) -> do
    let (global, normal) = List.partition
            ((==Constants.ly_global) . Types.event_instrument) events
    -- TODO filter out the v_movements since I won't be using them?
    staves <- convert_staff_groups config 0 global normal
    return (title, staves)

extract_movements :: Types.Config -> [Types.Event] -> Either Log.Msg [Movement]
extract_movements config events = do
    let (global, normal) = List.partition
            ((==Constants.ly_global) . Types.event_instrument) events
    (movements, global) <- either warn return $
        partition_key Constants.v_movement global
    forM (split_movements movements normal) $ \(start, title, events) -> do
        staves <- convert_staff_groups config start global events
        return (title, staves)

-- | Group a stream of events into individual staves based on instrument, and
-- for keyboard instruments, left or right hand.  Then convert each staff of
-- Events to Notes, divided up into measures.
convert_staff_groups :: Types.Config -> Types.Time -> [Types.Event]
    -> [Types.Event] -> Either Log.Msg [StaffGroup]
convert_staff_groups config start global events = do
    let staff_groups = split_events events
    let staff_end = fromMaybe 0 $ Seq.maximum (map Types.event_end events)
    (meters, global) <- either warn return $ parse_meters start staff_end global
    -- It would be nicer to partition_on Process.free_code so I don't have to
    -- re-parse it in each Process.process, but then process would have to
    -- take an Either FreeCode Event or something.
    let (global_code, remain) = List.partition
            (not . null . Constants.environ_free_code . Types.event_environ)
            global
    unless (null remain) $
        warn $ "leftover ly-global events: " <> pretty global
    forM staff_groups $ \(inst, staves) ->
        staff_group config start meters inst $
            map (distribute_global global_code inst) staves

-- | Split events by instrument, and if they have 'EnvKey.hand', further split
-- into right and left hand.
split_events :: [Types.Event] -> [(Score.Instrument, [[Types.Event]])]
split_events events =
    [ (inst, Seq.group_sort (lookup_hand . Types.event_environ) events)
    | (inst, events) <- by_inst
    ]
    where
    by_inst = Seq.keyed_group_sort Types.event_instrument events
    lookup_hand environ = case Env.get_val EnvKey.hand environ of
        Right (val :: Text)
            | val == "r" || val == "right" -> 0
            | val == "l" || val == "left" -> 1
            | otherwise -> 2
        _ -> 0

-- | Right hand goes at the top, left hand goes at the bottom.  Any other hands
-- go below that.  Events that are don't have a hand are assumed to be in the
-- right hand.
staff_group :: Types.Config -> Types.Time -> [Meter.Meter] -> Score.Instrument
    -> [[Types.Event]] -> Either Log.Msg StaffGroup
staff_group config start meters inst staves = do
    staff_measures <- forM (zip [1..] staves) $ \(i, es) ->
        annotate i $ Process.process config start meters es
    return $ StaffGroup inst staff_measures
    where
    annotate i = first (Log.add_prefix (pretty inst <> ":" <> showt i))

-- | Global FreeCode events get distributed to all staves.
distribute_global :: [Types.Event] -> Score.Instrument -> [Types.Event]
    -> [Types.Event]
distribute_global codes inst =
    Seq.merge_on Types.event_start
        (map (\e -> e { Types.event_instrument = inst }) codes)

-- ** movements

-- | Use the movement break points to group the events by movement.  The events
-- are not shifted in time, so each movement has to start at the proper offset.
-- The reason is that certain calls, like tuplet, bake in lilypond code, and it
-- will be wrong if the events change position.
split_movements :: [(Types.Time, Title)] -> [Types.Event]
    -> [(Types.Time, Title, [Types.Event])]
split_movements movements =
    filter (not . null . events_of) . split (Seq.zip_next ((0, "") : movements))
    where
    split (((start, title), Just (next, _)) : movements) events =
        (start, title, pre) : split movements post
        where (pre, post) = break ((>=next) . Types.event_start) events
    split (((start, title), Nothing) : _) events = [(start, title, events)]
    split [] events = [(0, "", events)]
    events_of (_, _, x) = x

-- ** meter

-- | Extract Meters from the Events, and emit one per measure.
parse_meters :: Types.Time -> Types.Time -> [Types.Event]
    -> Either Error ([Meter.Meter], [Types.Event])
parse_meters start staff_end events = do
    (meters, remain) <- partition_key Constants.v_meter events
    meters <- mapM (\(pos, meter) -> (pos,) <$> Meter.parse_meter meter) meters
    return (generate start Meter.default_meter meters, remain)
    where
    generate prev meter [] =
        replicate (measures_in (staff_end-prev) meter) meter
    generate prev prev_meter ((pos, meter) : meters) =
        replicate measures prev_meter ++ generate next meter meters
        where
        measures = measures_in (pos-prev) prev_meter
        next = prev + fromIntegral measures * Meter.measure_time prev_meter
    -- If you try to change the meter in the middle of a meter, it rounds up to
    -- the next barline.  If you put multiple meter changes before the barline,
    -- only the first one is accepted.  TODO arguably it should be the last.
    -- TODO or an error.
    measures_in dur meter = max 0 $ ceiling $
        fromIntegral dur / fromIntegral (Meter.measure_time meter)

-- * util

warn :: CallStack.Stack => Text -> Either Log.Msg a
warn = Left . Log.msg Log.Warn Nothing

partition_key :: EnvKey.Key -> [Types.Event]
    -> Either Error ([(Types.Time, Text)], [Types.Event])
partition_key key = go
    where
    go [] = return ([], [])
    go (e : es) = case Env.checked_val2 key (Types.event_environ e) of
        Nothing -> second (e:) <$> go es
        Just (Left err) -> Left $ pretty Constants.ly_global <> " event at "
            <> pretty pos <> err
        Just (Right val) -> first ((pos, val) :) <$> go es
        where pos = Types.event_start e
