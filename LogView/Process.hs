module LogView.Process where
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer

import qualified Data.Foldable as Foldable
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Time as Time

import qualified System.IO as IO
import qualified System.Posix.Files as Posix.Files
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.ParseBs as ParseBs
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Ui.Id as Id
import qualified Derive.Stack as Stack
import Types


-- | Only display timing msgs that take longer than this.
timing_diff_threshold :: Time.NominalDiffTime
timing_diff_threshold = 0.5

-- * state

data State = State {
    state_filter :: Filter
    -- | Msgs matching this regex have their matching groups put in the
    -- status line.
    , state_catch_patterns :: [CatchPattern]
    , state_cache :: Cache
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
    (compile_filter filt) [] initial_cache Map.empty Sequence.empty Nothing

-- | Keep track of cache msgs.
data Cache = Cache {
    cache_rederived :: Map.Map String [String]
    , cache_total :: Int
    , cache_blocks :: [String]
    } deriving (Show)

initial_cache :: Cache
initial_cache = Cache Map.empty 0 []

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
    process_cache msg
    process_catch
    let styled = format_msg msg
    return $ if eval_filter filt msg (style_text styled)
        then Just styled
        else Nothing
    where
    run = flip State.runState state
    process_catch = State.modify $ \st -> st { state_status =
        catch (state_status st) (state_catch_patterns st) }
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
            when_just result $ \_ -> State.modify $ \st ->
                st { state_last_displayed = Just (msg, 0) }
            return result
    where matches m1 m2 = Log.msg_text m1 == Log.msg_text m2

-- | This searches the log msg text for a regex and puts it in the status bar
-- with the given key string.
--
-- If the regex has no groups, the entire match is used for the value.  If it
-- has one group, that group is used.  If it has two groups, the first group
-- will replace the key.  >2 groups is an error.
catch_regexes :: [CatchPattern] -> Catch
catch_regexes patterns msg =
    Map.union (Map.fromList (concatMap match patterns))
    where
    match (title, reg) =
        map extract (Regex.find_groups reg (Log.msg_string msg))
        where
        extract (_, [match]) = (title, match)
        extract (_, [match_title, match]) = (match_title, match)
        extract _ = error $ show reg ++ " has >2 groups"

-- | The app sends this on startup, so I can clear out any status from the
-- last session.
catch_start :: Catch
catch_start msg status
    | Log.msg_string msg == "app starting" = Map.empty
    | otherwise = status


-- ** cache status

-- | Update the Cache state and Status.
--
-- The status keys start with ~ so they sort last.
process_cache :: Log.Msg -> ProcessM ()
process_cache msg
    | Regex.matches start_play_pattern (Log.msg_string msg) = do
        modify_cache $ const initial_cache
        modify_status $ Map.filter_key ((/="~") . take 1)
    -- Only keep track of block cache msgs.
    | Just bid <- get_block_id msg, Just because <- extract rederived_pattern =
        increment_rederived bid because
    | Just bid <- get_block_id msg, Just nvals <- extract cached_pattern,
            Just vals <- ParseBs.int nvals =
        increment_cached bid vals
    | otherwise = return ()
    where
    get_block_id = Stack.block_of
        <=< Seq.head . Stack.innermost . Stack.from_strings
        <=< Log.msg_stack
    extract regex = case Regex.find_groups regex (Log.msg_string msg) of
        [] -> Nothing
        [(_, [match])] -> Just match
        [(match, [])] -> Just match
        [(_, _:_:_)] -> error $ show regex ++ " has >1 group"
        matches -> error $
            "unexpected matches for " ++ show regex ++ ": " ++ show matches
    -- I clear and regenerate the cache status on every play.  It would be
    -- nicer to only do that when the score is rederived, but then I have to
    -- keep track of which block is being displayed in the cache status.
    start_play_pattern = Regex.make "^play block "
    rederived_pattern = Regex.make "^rederived generator because of (.*)"
    cached_pattern = Regex.make "^using cache, (\\d+) vals"

-- | Add the block of the given msg to the status string.  E.g.,
-- \"[13] bid1 bid2 ...\" -> \"14 bid0 bid1 ...\"
increment_rederived :: BlockId -> String -> ProcessM ()
increment_rederived bid because = do
    bids <- State.gets (Map.get [] because . cache_rederived . state_cache)
    -- append so they appear in order of appearance
    let new_bids = bids ++ [Id.ident_name bid]
    modify_cache $ \cache -> cache { cache_rederived =
        Map.insert because new_bids (cache_rederived cache) }
    modify_status $
        Map.insert ("~rederived " ++ because) (rederived new_bids)
    where
    rederived bids = ellide 25 $
        Printf.printf "[%d] %s" (length bids) (unwords bids)

-- | Add the number of cached blocks and total cached events.  E.g.,
-- \"cached: 10 [42]: bid1 bid2 bid3 ...\"
increment_cached :: BlockId -> Int -> ProcessM ()
increment_cached bid vals = do
    modify_cache $ \cache -> cache
        { cache_total = vals + cache_total cache
        , cache_blocks = cache_blocks cache ++ [Id.ident_name bid]
        }
    cache <- State.gets state_cache
    modify_status $ Map.insert "~cached" (cached cache)
    where
    cached cache = ellide 25 $ Printf.printf "%d [%d] %s"
        (length (cache_blocks cache)) (cache_total cache)
        (unwords (cache_blocks cache))

ellide :: Int -> String -> String
ellide len s
    | length s > len = take (len-3) s ++ "..."
    | otherwise = s

modify_cache :: (Cache -> Cache) -> ProcessM ()
modify_cache f = State.modify $ \st -> st { state_cache = f (state_cache st) }

modify_status :: (Status -> Status) -> ProcessM ()
modify_status f =
    State.modify $ \st -> st { state_status = f (state_status st) }

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
    when_just (Log.msg_caller msg) $ \caller -> do
        emit_srcpos caller
        with_plain " "
    when_just (Log.msg_stack msg) $ \stack -> do
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
    when_just (last_call stack) $ \call ->
        with_plain $ ' ' : call ++ ": "
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


-- * tail file

open :: FilePath
    -> Maybe Integer -- ^ no seek if Nothing, else seek n*m bytes from end
    -> IO IO.Handle
open filename seek = do
    -- ReadWriteMode makes it create the file if it doesn't exist, and not
    -- die here.
    hdl <- IO.openFile filename IO.ReadWriteMode
    IO.hSetBuffering hdl IO.LineBuffering -- See tail_getline.
    case seek of
        Nothing -> return ()
        Just n -> do
            IO.hSeek hdl IO.SeekFromEnd (-n * 200)
            when (n /= 0) $
                void $ IO.hGetLine hdl -- make sure I'm at a line boundary
    return hdl

tail_file :: STM.TChan Log.Msg -> FilePath -> IO.Handle -> IO ()
tail_file log_chan filename hdl = do
    size <- IO.hFileSize hdl
    loop (hdl, size)
    where
    loop state = do
        (line, state) <- tail_getline filename state
        msg <- deserialize_line line
        STM.atomically $ STM.writeTChan log_chan msg
        loop state

type TailState = (IO.Handle, Integer)

deserialize_line :: String -> IO Log.Msg
deserialize_line line = do
    err_msg <- Log.deserialize_msg line
    case err_msg of
        Left exc -> Log.initialized_msg Log.Error $ "error parsing: "
            ++ show exc ++ ", line was: " ++ show line
        Right msg -> return msg

tail_getline :: FilePath -> TailState -> IO (String, TailState)
tail_getline filename = go
    where
    go state@(hdl, last_size) = IO.hIsEOF hdl >>= \x -> case x of
        True -> do
            new_size <- IO.hFileSize hdl
            -- Check if the file was truncated.
            state <- if new_size < last_size
                then IO.hSeek hdl IO.AbsoluteSeek 0 >> return state
                else ifM (file_renamed filename new_size)
                    (reopen hdl new_size filename)
                    (Thread.delay 2 >> return state)
            go state
        False -> do
            -- Since hGetLine in its infinite wisdom chops the newline it's
            -- impossible to tell if this is a complete line or not.  I'll set
            -- LineBuffering and hope for the best.
            line <- IO.hGetLine hdl
            return (line, state)

-- | If the filename exists, open it and close the old file.
reopen :: IO.Handle -> Integer -> FilePath -> IO TailState
reopen hdl size filename =
    File.ignore_enoent (IO.openFile filename IO.ReadMode) >>= \x -> case x of
        Nothing -> do
            return (hdl, size)
        Just new -> do
            IO.hClose hdl
            IO.hSetBuffering new IO.LineBuffering
            IO.hSeek new IO.AbsoluteSeek 0
            size <- IO.hFileSize new
            return (new, size)

-- | Check if it looks like the file has been renamed.
file_renamed :: FilePath -> Integer -> IO Bool
file_renamed filename size = do
    -- I should really use inode, but ghc's crummy IO libs make that a pain,
    -- since handleToFd closes the handle.
    file_size <- maybe 0 Posix.Files.fileSize <$>
        File.ignore_enoent (Posix.Files.getFileStatus filename)
    return $ fromIntegral file_size /= size && file_size /= 0
