module LogView.Process where
import qualified Control.Concurrent.STM as STM
import Control.Monad
import qualified Control.Monad.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified System.IO as IO

import qualified Derive.Stack as Stack

import Util.Control
import qualified Util.Log as Log
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread


-- | Only display timing msgs that take longer than this.
timing_diff_threshold :: Time.NominalDiffTime
timing_diff_threshold = 0.5

-- * state

data State = State {
    state_filter :: Filter
    -- | Msgs matching this regex have their matching groups put put in the
    -- status line.
    , state_catch_patterns :: [CatchPattern]
    , state_status :: Status
    -- | A cache of the most recent msgs.  When the filter is changed they can
    -- be displayed.  This way memory use is bounded but you can display recent
    -- msgs you missed because of the filter.
    , state_cached_msgs :: Sequence.Seq Log.Msg
    -- | Last timing message.
    , state_last_timing :: Maybe Log.Msg
    } deriving (Show)
initial_state filt = State
    (compile_filter filt) [] Map.empty Sequence.empty Nothing

add_msg :: Int -> Log.Msg -> State -> State
add_msg history msg state = state { state_cached_msgs = seq }
    where seq = Sequence.take history (msg Sequence.<| state_cached_msgs state)

state_msgs :: State -> [Log.Msg]
state_msgs = Foldable.toList . state_cached_msgs

-- ** filter

-- | Filter language.
data Filter = Filter String (Log.Msg -> String -> Bool)
instance Show Filter where
    show (Filter src _) = "compile_filter " ++ show src

-- ** catch

-- | This searches the log msg text for a regex and puts it in the status bar
-- with the given key string.
--
-- If the regex has no groups, the entire match is used for the value.  If it
-- has one group, that group is used.  If it has two groups, the first group
-- will replace the key.  >2 groups is an error.
type CatchPattern = (String, Regex.Regex)

-- ** status

type Status = Map.Map String String
render_status :: Status -> StyledText
render_status status = run_formatter $ do
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
extract_style (StyledText text style) = (text, style)

-- | Process an incoming log msg.  If the msg isn't filtered out, returned
-- a colorized version.  Also possibly modify the app state for things like
-- catch and timing.
process_msg :: State -> Log.Msg -> (State, Maybe StyledText)
process_msg state msg = (new_state { state_status = status }, msg_styled)
    where
    (new_state, new_msg) = timer_filter state msg
    msg_styled = case new_msg of
        Just msg -> let styled = format_msg msg
            in if eval_filter (state_filter new_state) msg (style_text styled)
                then Just styled
                else Nothing
        Nothing -> Nothing
    caught = catch_patterns (state_catch_patterns state) (Log.msg_string msg)
    status = Map.union caught (state_status state)

-- | Filter out timer msgs that don't have a minimum time from the previous
-- timing, and prepend the interval and bump the priority to Warn if they do.
timer_filter :: State -> Log.Msg -> (State, Maybe Log.Msg)
timer_filter state msg
    | is_timer = case state_last_timing state of
        Nothing -> (new_state, Nothing)
        Just last_msg -> (new_state, timing_msg last_msg)
    | otherwise = (state, Just msg)
    where
    is_timer = Log.msg_prio msg == Log.Timer
    new_state = if is_timer
        then state { state_last_timing = Just msg } else state
    timing_msg last_msg
        | not (Log.is_first_timer msg) && diff >= timing_diff_threshold = Just $
            msg { Log.msg_text =
                Text.unwords [Text.pack (show diff), Log.msg_text msg]
            , Log.msg_prio = Log.Warn
            }
        | otherwise = Nothing
        where
        diff = Log.msg_date msg `Time.diffUTCTime` Log.msg_date last_msg

catch_patterns :: [CatchPattern] -> String -> Status
catch_patterns patterns text = Map.fromList $ concatMap match patterns
    where
    match (title, reg) = map extract (Regex.find_groups reg text)
        where
        extract (match, []) = (title, match)
        extract (_, [match]) = (title, match)
        extract (_, [match_title, match]) = (match_title, match)
        extract _ = error $ show reg ++ " has >2 groups"

-- ** filter

-- TODO implement a better language
compile_filter :: String -> Filter
compile_filter s = Filter s f
    where
    (not_has_, has) = List.partition ("-" `List.isPrefixOf`) (words s)
    not_has = map (drop 1) not_has_
    f _msg text = all (`List.isInfixOf` text) has
        && not (any (`List.isInfixOf` text) not_has)

eval_filter (Filter _ pred) msg text = pred msg text


-- * format_msg

format_msg :: Log.Msg -> StyledText
format_msg msg = run_formatter $ do
    with_plain (prio_stars (Log.msg_prio msg))
    with_plain "\t"
    let style = if Log.msg_prio msg < Log.Warn
            then style_plain else style_warn
    when_just (Log.msg_caller msg) $ \caller -> do
        emit_srcpos caller
        with_plain " "
    when_just (Log.msg_stack msg) $ \stack -> do
        emit_stack stack
        with_plain " "
    regex_style style msg_text_regexes (Log.msg_string msg)
    with_plain "\n"
    where
    prio_stars Log.Timer = "-"
    prio_stars prio = replicate (fromEnum prio) '*'

type Formatter = Writer.Writer [(String, [Style])] ()

run_formatter :: Formatter -> StyledText
run_formatter = render_styles . Writer.execWriter

emit_srcpos :: (String, Maybe String, Int) -> Formatter
emit_srcpos (file, func_name, line) = do
    with_style style_filename $ file ++ ":" ++ show line ++ " "
    maybe (return ())
        (\func -> with_style style_func_name ("[" ++ func ++ "]")) func_name

emit_stack :: Stack.Stack -> Formatter
emit_stack stack =
    with_style style_clickable $ Seq.join "/" (map fmt (Stack.to_ui stack))
    where fmt frame = "{s " ++ show (Stack.unparse_ui_frame frame) ++ "}"

emit_msg_text :: Style -> String -> Formatter
emit_msg_text style text = with_style style text

msg_text_regexes :: [(Regex.Regex, Style)]
msg_text_regexes = map (\(reg, style) -> (Regex.make reg, style))
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

with_plain = with_style style_plain
with_style style text = Writer.tell [(text, replicate (length text) style)]
literal_style style text = Writer.tell [(text, style)]

type Style = Char

render_styles :: [(String, [Style])] -> StyledText
render_styles styles =
    StyledText (concat (map fst styles)) (concat (map snd styles))

style_plain, style_warn, style_clickable, style_emphasis, style_divider,
    style_func_name, style_filename :: Style
style_plain = 'A'
style_warn = 'B'
style_clickable = 'C'
style_emphasis = 'D'
style_divider = 'E'
style_func_name = 'F'
style_filename = 'G'


-- * tail file

tail_file :: STM.TChan Log.Msg -> FilePath
    -> Maybe Integer -- ^ no seek if Nothing, else seek n*m bytes from end
    -> IO ()
tail_file log_chan filename seek = do
    -- ReadWriteMode makes it create the file if it doesn't exist, and not
    -- die here.
    hdl <- IO.openFile filename IO.ReadWriteMode
    IO.hSetBuffering hdl IO.LineBuffering -- See tail_getline.
    case seek of
        Nothing -> return ()
        Just n -> do
            IO.hSeek hdl IO.SeekFromEnd (-n * 200)
            when (n /= 0) $ do
                IO.hGetLine hdl -- make sure I'm at a line boundary
                return ()
    forever $ do
        line <- tail_getline hdl
        msg <- deserialize_line line
        STM.atomically $ STM.writeTChan log_chan msg

deserialize_line :: String -> IO Log.Msg
deserialize_line line = do
    err_msg <- Log.deserialize_msg line
    case err_msg of
        Left exc -> Log.msg Log.Error $ "error parsing: " ++ show exc
            ++ ", line was: " ++ show line
        Right msg -> return msg

tail_getline :: IO.Handle -> IO String
tail_getline hdl = do
    while_ (IO.hIsEOF hdl) $
        Thread.delay 0.5
    -- Since hGetLine in its infinite wisdom chops the newline it's impossible
    -- to tell if this is a complete line or not.  I'll set LineBuffering and
    -- hope for the best.
    IO.hGetLine hdl
