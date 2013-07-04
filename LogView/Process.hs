-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module LogView.Process where
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence

import Util.Control
import qualified Util.Log as Log
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq

import qualified Derive.Stack as Stack


-- * state

data State = State {
    state_filter :: Filter
    -- | Msgs matching this regex have their matching groups put in the
    -- status line.
    , state_catch_patterns :: [CatchPattern]
    , state_status :: Status
    -- | A cache of the most recent msgs.  When the filter is changed they can
    -- be displayed.  This way memory use is bounded but you can display recent
    -- msgs you missed because of the filter.
    , state_cached_msgs :: Sequence.Seq Log.Msg
    -- | Last displayed msg, along with the number of times it has been seen.
    -- Used to suppress duplicate msgs.
    , state_last_displayed :: Maybe (Log.Msg, Int)
    } deriving (Show)

initial_state :: String -> State
initial_state filt = State
    { state_filter = compile_filter filt
    , state_catch_patterns = []
    , state_status = Map.empty
    , state_cached_msgs = Sequence.empty
    , state_last_displayed = Nothing
    }

add_msg :: Int -> Log.Msg -> State -> State
add_msg history msg state = state { state_cached_msgs = seq }
    where seq = Sequence.take history (msg Sequence.<| state_cached_msgs state)

state_msgs :: State -> [Log.Msg]
state_msgs = Foldable.toList . state_cached_msgs

-- ** catch

-- | Transform the status line based on each msg.
type Catch = Log.Msg -> Status -> Status
type CatchPattern = (String, Regex.Regex)

-- ** status

type Status = Map.Map String String
render_status :: Status -> StyledText
render_status status = run_formatter $
    sequence_ $ List.intersperse (with_style style_divider " || ")
        (map format_status (Map.assocs status))

format_status :: (String, String) -> Formatter
format_status (k, v) = do
    with_style style_emphasis k
    with_style style_plain ": "
    regex_style style_plain clickable_braces v

clickable_braces :: [(Regex.Regex, Style)]
clickable_braces =
    [ (Regex.make "\\{.*?\\}", style_clickable)
    ]

data StyledText = StyledText {
    style_text :: String
    , style_style :: String
    } deriving (Show)

extract_style :: StyledText -> (String, String)
extract_style (StyledText text style) = (text, style)

type ProcessM = State.StateT State Identity.Identity

-- | Process an incoming log msg.  If the msg isn't filtered out, returned
-- a colorized version.  Also possibly modify the app state for things like
-- catch and timing.
process_msg :: State -> Log.Msg -> (Maybe StyledText, State)
process_msg state msg = run $ do -- suppress_last msg $ do
    filt <- State.gets state_filter
    process_catch
    let styled = format_msg msg
    return $ if eval_filter filt msg (style_text styled)
        then Just styled else Nothing
    where
    run = flip State.runState state
    process_catch = State.modify $ \st -> st
        { state_status = catch (state_status st) (state_catch_patterns st) }
    catch status patterns = List.foldl'
        (\status catch -> catch msg status) status (catches patterns)

catches :: [CatchPattern] -> [Catch]
catches patterns =
    [ catch_regexes patterns
    , catch_start
    ]

suppress_last :: Log.Msg -> ProcessM (Maybe a) -> ProcessM (Maybe a)
suppress_last msg process = do
    last_displayed <- State.gets state_last_displayed
    case last_displayed of
        Just (last_msg, times) | matches last_msg msg -> do
            State.modify $ \st ->
                st { state_last_displayed = Just (msg, times+1) }
            return Nothing
        _ -> do
            result <- process
            whenJust result $ \_ -> State.modify $ \st ->
                st { state_last_displayed = Just (msg, 0) }
            return result
    where matches m1 m2 = Log.msg_text m1 == Log.msg_text m2

-- | This searches the log msg text for a regex and puts it in the status bar
-- with the given key string.
--
-- If the regex has no groups, the entire match is used for the value.  If it
-- has one group, that group is used.  If it has two groups, the first group
-- will replace the key.  >2 groups is an error.
--
-- If the value is "", then the key is removed.
catch_regexes :: [CatchPattern] -> Catch
catch_regexes patterns msg = Map.filter (not . null) . Map.union status
    where
    status = Map.unions $
        map (($ Log.msg_string msg) . match_pattern) patterns

-- | The app sends this on startup, so I can clear out any status from the
-- last session.
catch_start :: Catch
catch_start msg status
    | Log.msg_string msg == "app starting" = Map.empty
    | otherwise = status

global_status_pattern :: CatchPattern
global_status_pattern = ("_", Regex.make "^global status: (.*?) -- (.*)")

match_pattern :: CatchPattern -> String -> Map.Map String String
match_pattern (title, reg) = Map.fromList . map extract . Regex.find_groups reg
    where
    extract (_, [match]) = (title, match)
    extract (_, [match_title, match]) = (match_title, match)
    extract _ = error $ show reg ++ " has >2 groups"


-- ** filter

-- | Filter language.
data Filter = Filter String (Log.Msg -> String -> Bool)
instance Show Filter where
    show (Filter src _) = "compile_filter " ++ show src

-- TODO implement a better language
compile_filter :: String -> Filter
compile_filter s = Filter s f
    where
    (not_has_, has) = List.partition ("-" `List.isPrefixOf`) (words s)
    not_has = map (drop 1) not_has_
    f _msg text = all (`List.isInfixOf` text) has
        && not (any (`List.isInfixOf` text) not_has)

eval_filter :: Filter -> Log.Msg -> String -> Bool
eval_filter (Filter _ pred) msg text = pred msg text


-- * format_msg

format_msg :: Log.Msg -> StyledText
format_msg msg = run_formatter $ do
    with_plain (prio_stars (Log.msg_prio msg))
    with_plain "\t"
    let style = if Log.msg_prio msg < Log.Warn
            then style_plain else style_warn
    whenJust (Log.msg_caller msg) $ \caller -> do
        emit_srcpos caller
        with_plain " "
    whenJust (Log.msg_stack msg) $ \stack -> do
        emit_stack (Stack.from_strings stack)
        with_plain " "
    regex_style style msg_text_regexes (Log.msg_string msg)
    with_plain "\n"
    where
    prio_stars Log.Timer = "-"
    prio_stars prio = replicate (fromEnum prio) '*'

-- | Pair together text along with the magic Style characters.  The Styles
-- should be the same length as the string.
type Formatter = Writer.Writer [(String, [Style])] ()

run_formatter :: Formatter -> StyledText
run_formatter = render_styles . Writer.execWriter

emit_srcpos :: (String, Maybe String, Int) -> Formatter
emit_srcpos (file, func_name, line) = do
    with_style style_filename $ file ++ ":" ++ show line ++ " "
    maybe (return ())
        (\func -> with_style style_func_name ("[" ++ func ++ "]")) func_name

emit_stack :: Stack.Stack -> Formatter
emit_stack stack = do
    with_style style_clickable $ Seq.join "/" (map fmt (Stack.to_ui stack))
    whenJust (last_call stack) $ \call ->
        with_plain $ ' ' : untxt call ++ ": "
    where
    fmt frame = "{s " ++ show (Stack.unparse_ui_frame frame) ++ "}"
    last_call = Seq.head . mapMaybe Stack.call_of . Stack.innermost

emit_msg_text :: Style -> String -> Formatter
emit_msg_text = with_style

msg_text_regexes :: [(Regex.Regex, Style)]
msg_text_regexes = map (first Regex.make)
    [ ("\\([bvt]id \".*?\"\\)", style_emphasis)
    ] ++ clickable_braces

regex_style :: Style -> [(Regex.Regex, Style)] -> String -> Formatter
regex_style default_style regex_styles txt =
    literal_style (map go [0..length txt - 1]) txt
    where
    ranges = [(range, style) | (reg, style) <- regex_styles,
        range <- Regex.find_ranges reg txt]
    go i = maybe default_style snd $ List.find ((i `within`) . fst) ranges
    within i (lo, hi) = lo <= i && i < hi

with_plain :: String -> Formatter
with_plain = with_style style_plain

with_style :: Style -> String -> Formatter
with_style style text = Writer.tell [(text, replicate (length text) style)]

literal_style :: [Style] -> String -> Formatter
literal_style style text = Writer.tell [(text, style)]

type Style = Char

render_styles :: [(String, [Style])] -> StyledText
render_styles styles =
    StyledText (concatMap fst styles) (concatMap snd styles)

style_plain, style_warn, style_clickable, style_emphasis, style_divider,
    style_func_name, style_filename :: Style
style_plain = 'A'
style_warn = 'B'
style_clickable = 'C'
style_emphasis = 'D'
style_divider = 'E'
style_func_name = 'F'
style_filename = 'G'
