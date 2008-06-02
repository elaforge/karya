module LogViewer.Process where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Monad
import qualified Control.Monad.Writer as Writer
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.IO as IO
import qualified Text.Regex as Regex

import qualified Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq


-- * state

data State = State {
    state_filter :: Filter
    -- | Msgs matching this regex have their matching groups put put in the
    -- status line.
    , state_catch_patterns :: [CatchPattern]
    , state_status :: Status
    , state_msgs :: [Log.Msg]
    } deriving (Show)
initial_state = State "" [] Map.empty []

-- ** filter

-- | Filter language.
type Filter = String

-- ** catch

-- | (title, regex)
type CatchPattern = (String, Regex)

-- | Yay for Regex.Regex not being in Show!
data Regex = Regex String Regex.Regex

instance Show Regex where
    show (Regex reg _) = "make_regex " ++ show reg
make_regex reg = Regex reg (Regex.mkRegex reg)

-- ** status

type Status = Map.Map String String
render_status status =
    Seq.join " / " $ map (\(k, v) -> k ++ ": " ++ v) (Map.assocs status)

data StyledText = StyledText {
    style_text :: String
    , style_style :: String
    } deriving (Show)
no_text = StyledText "" ""
extract_style (StyledText text style) = (text, style)

-- (status, log_str, style_str)
process_msg :: State -> Log.Msg -> (Status, StyledText)
process_msg state msg = (status, msg_styled)
    where
    styled = format_msg msg
    msg_styled = if eval_filter (state_filter state) msg (style_text styled)
        then styled
        else no_text
    caught = catch_patterns (state_catch_patterns state) (Log.msg_text msg)
    status = Map.union caught (state_status state)

catch_patterns :: [CatchPattern] -> String -> Status
catch_patterns patterns text = Map.fromList $ concatMap match patterns
    where
    match (title, (Regex _ reg)) = case Regex.matchRegex reg text of
        Nothing -> []
        Just ms -> map ((,) title) ms

-- TODO implement filter language
eval_filter filter msg text = filter `List.isInfixOf` text


-- * format_msg

format_msg :: Log.Msg -> StyledText
format_msg msg = run_formatter $ do
    emit $ replicate (fromEnum (Log.msg_prio msg) + 1) '*'
    emit "\t"
    maybe (return ()) emit_srcpos (Log.msg_caller msg)
    let style = if Log.msg_prio msg < Log.Warn
            then style_plain else style_warn
    with_style style (Log.msg_text msg)
    emit "\n"

run_formatter = render_styles . Writer.execWriter

emit_srcpos (file, func_name, line) = do
    emit $ file ++ ":" ++ show line ++ " "
    maybe (return ())
        (\func -> with_style style_emphasis ("[" ++ func ++ "] ")) func_name

emit = with_style style_plain
with_style style text = Writer.tell [(text, style)]

type Style = Char

render_styles :: [(String, Style)] -> StyledText
render_styles styles = StyledText text style
    where
    f (s, style) (s', style') = (s ++ s', replicate (length s) style ++ style')
    (text, style) = foldr f ("", "") styles

style_plain = 'A'
style_warn = 'B'
style_clickable = 'C'
style_emphasis = 'D'


-- * tail file

tail_file log_chan filename = do
    -- ReadWriteMode makes it create the file if it doesn't exist, and not
    -- die here.
    hdl <- IO.openFile filename IO.ReadWriteMode
    IO.hSetBuffering hdl IO.LineBuffering
    forever $ do
        line <- tail_getline hdl
        process_line log_chan line

process_line log_chan line = do
    err_msg <- Log.deserialize_msg line
    let msg = case err_msg of
            Left exc ->
                Log.msg Log.Error ("error parsing: " ++ show exc) Nothing
            Right msg -> msg
    STM.atomically $ STM.writeTChan log_chan msg

tail_getline :: IO.Handle -> IO String
tail_getline hdl = do
    Util.Control.while_ (IO.hIsEOF hdl) $
        Concurrent.threadDelay 500000
    -- Since hGetLine in its infinite wisdom chops the newline it's impossible
    -- to tell if this is a complete line or not.  I'll set LineBuffering and
    -- hope for the best.
    IO.hGetLine hdl
