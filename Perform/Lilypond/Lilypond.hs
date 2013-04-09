{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
-- | Convert from Score events to a lilypond score.
module Perform.Lilypond.Lilypond (
    module Perform.Lilypond.Lilypond
    , module Perform.Lilypond.Types
) where
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import Perform.Lilypond.Types


-- * output

type Title = String

-- | Same as 'Cmd.Cmd.StackMap', but I don't feel like importing Cmd here.
type StackMap = Map.Map Int Stack.UiFrame

make_ly :: Config -> Title -> [Event] -> Either String ([Text.Text], StackMap)
make_ly config title events =
    ly_file config title <$> convert_movements config events

-- | Make a score from multiple movements.
make_lys :: Config -> Title -> [(Title, [Event])]
    -> Either String ([Text.Text], StackMap)
make_lys config title sections = fmap (ly_file config title) $
    forM sections $ \(title, events) -> do
        staves <- convert_staff_groups config 0 events
        return (title, staves)

inst_name :: Score.Instrument -> String
inst_name = dropWhile (=='/') . dropWhile (/='/') . Score.inst_name

ly_file :: Config -> Title -> [Movement] -> ([Text.Text], StackMap)
ly_file config title movements = run_output $ do
    outputs
        [ "\\version" <+> str "2.14.2"
        , "\\language" <+> str "english"
        , "\\header { title =" <+> str title <+> "tagline = \"\" }"
        , ""
        ]
    mapM_ write_movement movements
    where
    write_movement (title, staff_groups) = do
        output "\\score {\n"
        output "<<\n"
        mapM_ write_staff_group $ sort_staves (config_staves config) staff_groups
        output ">>\n"
        when (not (null title)) $
            output $ "\\header { piece =" <+> str title <+> "}\n"
        output "}\n\n"
    write_staff_group (StaffGroup _ staves, long_inst, short_inst) =
        case staves of
            [staff] -> write_staff (Just (long_inst, short_inst)) Nothing staff
            [up, down] -> write_piano_staff long_inst short_inst $ do
                write_staff Nothing (Just "up") up
                write_staff Nothing (Just "down") down
            _ -> write_piano_staff long_inst short_inst $
                mapM_ (write_staff Nothing Nothing) staves
    write_piano_staff long short contents = do
        outputs
            [ "\\new PianoStaff <<"
            , ly_set "PianoStaff.instrumentName" long
            , ly_set "PianoStaff.shortInstrumentName" short
            ]
        contents
        output ">>\n"

    write_staff inst_names maybe_name lys = do
        output $ "\\new Staff " <> maybe "" (("= "<>) . str) maybe_name
            <+> "{\n"
        outputs
            [ "\\numericTimeSignature" -- Use 4/4 and 2/4 instead of C
            , "\\set Staff.printKeyCancellation = ##f"
            ]
        when_just inst_names $ \(long, short) -> outputs
            [ ly_set "Staff.instrumentName" long
            , ly_set "Staff.shortInstrumentName" short
            ]
        output "{\n"
        set_bar 1
        mapM_ write_voice_ly lys
        output "} }\n"

    str = Text.pack . to_lily
    x <+> y = x <> " " <> y
    ly_set name val = "\\set" <+> name <+> "=" <+> str val

write_voice_ly :: Process.VoiceLy -> Output ()
write_voice_ly (Left (Process.Voices voices)) = do
    output "<<\n  "
    start <- State.gets output_bar
    bars <- mapM (\v -> set_bar start >> write_voice v) voices
    output ">> \\oneVoice\n  "
    when (not (all_equal bars)) $
        -- Lilypond will also complain.
        output $ "% WARNING: voices have different numbers of bars: "
            <> Text.pack (show bars)
    set_bar (fromMaybe start (Seq.head bars))
    where
    all_equal [] = True
    all_equal (x:xs) = all (==x) xs
write_voice_ly (Right ly) = write_ly ly

write_ly :: Process.Ly -> Output ()
write_ly ly@(Process.Barline {}) = do
    bar <- State.gets output_bar
    output $ Text.pack (to_lily ly) <> " % " <> Text.pack (show bar) <> "\n  "
    set_bar (bar+1)
write_ly ly = output $ Text.pack (to_lily ly) <> " "

write_voice :: (Process.Voice, [Process.Ly]) -> Output Int
write_voice (voice, lys) = do
    output $ (if voice == Process.VoiceOne then "" else "\\new Voice ")
        <> "{ " <> Text.pack (to_lily voice) <> "\n  "
    mapM_ write_ly lys
    output "} "
    State.gets output_bar

sort_staves :: [(Score.Instrument, String, String)] -> [StaffGroup]
    -> [(StaffGroup, String, String)]
sort_staves staff_config = map lookup_name . Seq.sort_on inst_key
    where
    lookup_name staff =
        case List.find (\(i, _, _) -> i == inst) staff_config of
            Nothing -> (staff, inst_name inst, inst_name inst)
            Just (_, long, short) -> (staff, long, short)
        where inst = inst_of staff
    inst_key staff =
        maybe (1, 0) ((,) 0) $ List.elemIndex (inst_of staff) order
    order = [inst | (inst, _, _) <- staff_config]
    inst_of (StaffGroup inst _) = inst

type Output a = State.State OutputState a

run_output :: Output a -> ([Text.Text], StackMap)
run_output m = (reverse (output_chunks state), output_map state)
    where state = State.execState m (OutputState [] Map.empty 1 1)

data OutputState = OutputState {
    -- | Chunks of text to write, in reverse order.  I could use
    -- Text.Lazy.Builder, but this is simpler and performance is probably ok.
    output_chunks :: ![Text.Text]
    , output_map :: !StackMap
    -- | Running sum of the length of the chunks.
    , output_char_num :: !Int
    , output_bar :: !Int
    } deriving (Show)

outputs :: [Text.Text] -> Output ()
outputs = output . Text.unlines

output :: Text.Text -> Output ()
output text = State.modify $ \state -> state
    { output_chunks = text : output_chunks state
    , output_char_num = Text.length text + output_char_num state
    }

record_stack :: Stack.UiFrame -> Output ()
record_stack stack = State.modify $ \st -> st { output_map =
    Map.insert (output_char_num st) stack (output_map st) }

set_bar :: Int -> Output ()
set_bar n = State.modify $ \state -> state { output_bar = n }


-- * split staves

type Movement = (Title, [StaffGroup])

-- | If the staff group has >1 staff, it is bracketed as a grand staff.
data StaffGroup = StaffGroup Score.Instrument [[Process.VoiceLy]]
    deriving (Show)

convert_sections :: Config -> [(Title, [Event])] -> Either String [Movement]
convert_sections config sections = forM sections $ \(title, events) -> do
    staves <- convert_staff_groups config 0 events
    return (title, staves)

convert_movements :: Config -> [Event] -> Either String [Movement]
convert_movements config events = do
    movements <- get_movements $
        filter ((==Constants.ly_global) . event_instrument) events
    forM (split_movements movements events) $ \(start, title, events) -> do
        staves <- convert_staff_groups config start events
        return (title, staves)

-- | Group a stream of events into individual staves based on instrument, and
-- for keyboard instruments, left or right hand.  Then convert each staff of
-- Events to Notes, divided up into measures.
convert_staff_groups :: Config -> Time -> [Event] -> Either String [StaffGroup]
convert_staff_groups config start events = do
    let (global, normal) =
            List.partition ((==Constants.ly_global) . event_instrument) events
        staff_groups = split_events normal
    let staff_end = fromMaybe 0 $ Seq.maximum (map event_end events)
    meters <- get_meters start staff_end global
    forM staff_groups $ \(inst, staves) ->
        staff_group config start meters inst staves

split_events :: [Event] -> [(Score.Instrument, [[Event]])]
split_events events =
    [(inst, Seq.group_on (lookup_hand . event_environ) events)
        | (inst, events) <- by_inst]
    where
    by_inst = Seq.keyed_group_on event_instrument events
    lookup_hand environ = case TrackLang.get_val Constants.v_hand environ of
        Right (val :: String)
            | val == "right" -> 0
            | val == "left" -> 1
            | otherwise -> 2
        _ -> 0

-- | Right hand goes at the top, left hand goes at the bottom.  Any other hands
-- goe below that.  Events that are don't have a hand are assumed to be in the
-- right hand.
staff_group :: Config -> Time -> [Meter.Meter] -> Score.Instrument -> [[Event]]
    -> Either String StaffGroup
staff_group config start meters inst staves = do
    staff_measures <- mapM (Process.process config start meters) staves
    return $ StaffGroup inst staff_measures

-- ** movements

-- | Use the movement break points to group the events by movement.  The events
-- are not shifted in time, so each movement has to start at the proper offset.
-- The reason is that certain calls, like tuplet, bake in lilypond code, and it
-- will be wrong if the events change position.
split_movements :: [(Time, String)] -> [Event] -> [(Time, String, [Event])]
split_movements movements =
    filter (not . null . events_of) . split (Seq.zip_next ((0, "") : movements))
    where
    split (((start, title), Just (next, _)) : movements) events =
        (start, title, pre) : split movements post
        where (pre, post) = break ((>=next) . event_start) events
    split (((start, title), Nothing) : _) events = [(start, title, events)]
    split [] events = [(0, "", events)]
    events_of (_, _, x) = x

get_movements :: [Event] -> Either String [(Time, String)]
get_movements = mapMaybeM $ \event -> do
    title <- TrackLang.checked_val Constants.v_movement (event_environ event)
    return $ (,) (event_start event) <$> title

-- ** meter

get_meters :: Time -> Time -> [Event] -> Either String [Meter.Meter]
get_meters start staff_end events = do
    meters <- mapMaybeM get_meter events
    return $ generate start Meter.default_meter meters
    where
    generate prev meter [] =
        replicate (measures_in (staff_end-prev) meter) meter
    generate prev prev_meter ((pos, meter) : meters) =
        replicate mm prev_meter ++ generate next meter meters
        where
        mm = measures_in (pos-prev) prev_meter
        next = prev + fromIntegral mm * Meter.measure_time prev_meter
    -- If you try to change the meter in the middle of a meter, it rounds up to
    -- the next barline.  If you put multiple meter changes before the barline,
    -- only the first one is accepted.  TODO arguably it should be the last.
    measures_in dur meter = max 0 $ ceiling $
        fromIntegral dur / fromIntegral (Meter.measure_time meter)

    get_meter event = error_context context $ do
        maybe_val <- TrackLang.checked_val Constants.v_meter
            (event_environ event)
        case maybe_val of
            Nothing -> return Nothing
            Just val -> do
                meter <- Meter.parse_meter val
                return $ Just (event_start event, meter)
        where
        context = Pretty.pretty Constants.ly_global <> " event at "
            <> Pretty.pretty (event_start event)

error_context :: String -> Either String a -> Either String a
error_context msg = either (Left . ((msg ++ ": ") ++)) Right
