-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module LogView.Process (
    -- * state
    State(..), initialState, addMsg, stateMsgs
    , compileFilter
    -- * process_msg
    , processMsg
    , CatchPattern, globalStatusPattern
    , renderStatus
    , StyledText(..)

    , matchPattern
#ifdef TESTING
    , flattenRanges, runFormatter, regexStyle, stylePlain
    , msgTextRegexes
#endif
) where
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Word as Word

import qualified Util.CallStack as CallStack
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Regex as Regex

import qualified Derive.Stack as Stack

import           Global


-- * state

data State = State
    { stateFilter :: Filter
    -- | Msgs matching this regex have their matching groups put in the
    -- status line.
    , stateCatchPatterns :: [CatchPattern]
    , stateStatus :: Status
    -- | A cache of the most recent msgs.  When the filter is changed they can
    -- be displayed.  This way memory use is bounded but you can display recent
    -- msgs you missed because of the filter.
    , stateCachedMsgs :: Sequence.Seq Log.Msg
    -- | Last displayed msg, along with the number of times it has been seen.
    -- Used to suppress duplicate msgs.
    , stateLastDisplayed :: Maybe (Log.Msg, Int)
    } deriving (Show)

initialState :: Text -> State
initialState filt = State
    { stateFilter = compileFilter filt
    , stateCatchPatterns = []
    , stateStatus = Map.empty
    , stateCachedMsgs = Sequence.empty
    , stateLastDisplayed = Nothing
    }

addMsg :: Int -> Log.Msg -> State -> State
addMsg history msg state = state { stateCachedMsgs = seq }
    where seq = Sequence.take history (msg Sequence.<| stateCachedMsgs state)

stateMsgs :: State -> [Log.Msg]
stateMsgs = Foldable.toList . stateCachedMsgs

-- ** catch

-- | Transform the status line based on each msg.
type Catch = Log.Msg -> Status -> Status

-- | Extract text from a log msg and put it in 'state_status', via
-- 'catch_regexes'.
type CatchPattern = (Text, Regex.Regex)

-- ** status

type Status = Map Text Text

renderStatus :: Status -> StyledText
renderStatus status = runFormatter $
    sequence_ $ List.intersperse (withStyle styleDivider " || ")
        (map formatStatus (Map.assocs status))

formatStatus :: (Text, Text) -> Formatter
formatStatus (k, v) = do
    withStyle styleEmphasis k
    withStyle stylePlain ": "
    regexStyle stylePlain clickableBraces v

clickableBraces :: [(Regex.Regex, Style)]
clickableBraces =
    [ (Regex.compileUnsafe "\\{.*?\\}", styleClickable)
    ]

data StyledText = StyledText
    -- | UTF8-encoded text.
    { styleText :: B.ByteString
    -- | 'Style' characters, same length as style_text.
    , styleStyle :: B.ByteString
    } deriving (Eq, Show)

type ProcessM = State.StateT State Identity.Identity

-- | Process an incoming log msg.  If the msg isn't filtered out, returned
-- a colorized version.  Also possibly modify the app state for things like
-- catch and timing.
processMsg :: State -> Log.Msg -> (Maybe StyledText, State)
processMsg state msg = run $ do -- suppress_last msg $ do
    filt <- State.gets stateFilter
    processCatch
    let styled = formatMsg msg
    -- I match the filter on the styled output so that the filter is on
    -- the msg as actually displayed.
    pure $ if evalFilter filt msg $
            Text.Encoding.decodeUtf8 (styleText styled)
        then Just styled else Nothing
    where
    run = flip State.runState state
    processCatch = State.modify $ \st -> st
        { stateStatus = catch (stateStatus st) (stateCatchPatterns st) }
    catch status patterns = foldl'
        (\status catch -> catch msg status) status (catches patterns)

catches :: [CatchPattern] -> [Catch]
catches patterns =
    [ catchRegexes patterns
    , catchStart
    ]

-- | Return Nothing if the given msg is the same as the last one.
suppressLast :: Log.Msg -> ProcessM (Maybe a) -> ProcessM (Maybe a)
suppressLast msg process = do
    lastDisplayed <- State.gets stateLastDisplayed
    case lastDisplayed of
        Just (lastMsg, times) | matches lastMsg msg -> do
            State.modify $ \st ->
                st { stateLastDisplayed = Just (msg, times+1) }
            pure Nothing
        _ -> do
            result <- process
            whenJust result $ \_ -> State.modify $ \st ->
                st { stateLastDisplayed = Just (msg, 0) }
            pure result
    where matches m1 m2 = Log.msgText m1 == Log.msgText m2

-- | This searches the log msg text for a regex and puts it in the status bar
-- with the given key string.
--
-- If the regex has no groups, the entire match is used for the value.  If it
-- has one group, that group is used.  If it has two groups, the first group
-- will replace the key.  >2 groups is an error.
--
-- If the value is \"\", then the key is removed.
catchRegexes :: [CatchPattern] -> Catch
catchRegexes patterns msg = Map.filter (not . Text.null) . Map.union status
    where
    status = Map.unions $
        map (($ Log.msgText msg) . matchPattern) patterns

-- | The app sends this on startup, so I can clear out any status from the
-- last session.
catchStart :: Catch
catchStart msg status
    | Log.msgText msg == "app starting" = Map.empty
    | otherwise = status

-- | This catches msgs emitted by 'Cmd.Cmd.set_global_status'.
globalStatusPattern :: CatchPattern
globalStatusPattern =
    ("_", Regex.compileUnsafe "^global status: (.*?) -- (.*)")

matchPattern :: CatchPattern -> Text -> Map Text Text
matchPattern (title, reg) = Map.fromList . map extract . Regex.groups reg
    where
    extract (_, [match]) = (title, match)
    extract (_, [matchTitle, match]) = (matchTitle, match)
    extract _ = error $ show reg ++ " has >2 groups"


-- ** filter

-- | Filter language, created by 'compile_filter'.
data Filter = Filter Text (Log.Msg -> Text -> Bool)
instance Show Filter where
    show (Filter src _) = "Process.compile_filter " ++ show src

-- | Compile a simple filter language.  A log msg matches if all of the words
-- in the filter occur within its 'Log.msg_text', and none of the words
-- prefixed by @-@ occur.
compileFilter :: Text -> Filter
compileFilter s = Filter s pred
    where
    (notHas_, has) = List.partition ("-" `Text.isPrefixOf`) (Text.words s)
    notHas = map (Text.drop 1) notHas_
    pred _msg text = all (`Text.isInfixOf` text) has
        && not (any (`Text.isInfixOf` text) notHas)

evalFilter :: Filter -> Log.Msg -> Text -> Bool
evalFilter (Filter _ pred) msg text = pred msg text


-- * format_msg

-- | Format and colorize a single Log.Msg.
formatMsg :: Log.Msg -> StyledText
formatMsg msg = runFormatter $ do
    let width = fromEnum (maxBound :: Log.Priority)
    withStyle styleFixed $ Text.justifyLeft width ' ' $
        prioStars (Log.msgPriority msg)
    let style = if Log.msgPriority msg < Log.Warn
            then stylePlain else styleWarn
    case Log.msgCaller msg of
        CallStack.NoCaller -> pure ()
        caller@(CallStack.Caller {}) ->
            withStyle styleFilename $ CallStack.showCaller caller <> " "
    whenJust (Log.msgStack msg) $ \stack -> emitStack stack >> withPlain " "
    regexStyle style msgTextRegexes (Log.msgText msg)
    withPlain "\n"
    where
    prioStars Log.Timer = "-"
    prioStars prio = Text.replicate (fromEnum prio) "*"

-- | Pair together text along with the magic Style characters.  The Styles
-- should be the same length as the string.
type Formatter = Writer.Writer Builder ()

data Builder = Builder !Builder.Builder !Builder.Builder

instance Semigroup Builder where
    Builder a1 b1 <> Builder a2 b2 = Builder (a1 <> a2) (b1 <> b2)
instance Monoid Builder where
    mempty = Builder mempty mempty
    mappend = (<>)

runFormatter :: Formatter -> StyledText
runFormatter = build . Writer.execWriter
    where
    build (Builder text styles) = StyledText (b text) (b styles)
    b = Lazy.toStrict . Builder.toLazyByteString

emitStack :: Stack.Stack -> Formatter
emitStack stack = do
    if stack == Stack.empty then pure ()
        -- This likely means the global transform failed.
        else if null uiStack then withPlain $
            "[" <> Text.intercalate " / " (map pretty (Stack.outermost stack))
            <> "]"
        else withStyle styleClickable $
            Text.intercalate " / " (map Stack.logUiFrame uiStack)
    whenJust (lastCall stack) $ \call ->
        withPlain $ " " <> call <> ":"
    where
    uiStack = Stack.toUi stack
    lastCall = Lists.head . mapMaybe Stack.callOf . Stack.innermost

msgTextRegexes :: [(Regex.Regex, Style)]
msgTextRegexes =
    (Regex.compileUnsafe "\\([bvt]id \".*?\"\\)", styleEmphasis)
    : clickableBraces

regexStyle :: Style -> [(Regex.Regex, Style)] -> Text -> Formatter
regexStyle defaultStyle regexStyles text =
    sequence_ emits >> withStyle defaultStyle rest
    where
    (rest, emits) = List.mapAccumL emit text ranges
    emit text (i, style) = (post, withStyle style pre)
        where (pre, post) = Text.splitAt i text
    ranges = flattenRanges defaultStyle
        [ (range, style)
        | (reg, style) <- regexStyles
        , (range, _) <- Regex.groupRanges reg text
        ]

flattenRanges :: a -> [((Int, Int), a)] -> [(Int, a)]
flattenRanges deflt = filter ((>0) . fst) . snd . go 0 . Lists.sortOn fst
    where
    go n [] = (n, [])
    go n (((s, e), style) : ranges) = (,) (max lastN e) $
        (s - n, deflt)
        : (min e next - s, style)
        : rest ++ if lastN < e then [(e - lastN, style)] else []
        where
        next = maybe e (fst . fst) (Lists.head ranges)
        (lastN, rest) = go e ranges

withPlain :: Text -> Formatter
withPlain = withStyle stylePlain

withStyle :: Style -> Text -> Formatter
withStyle _ text | Text.null text = pure ()
withStyle style text = Writer.tell $
    Builder (Builder.byteString utf8)
        (Builder.byteString (B.replicate (B.length utf8) style))
    where utf8 = Text.Encoding.encodeUtf8 text

type Style = Word.Word8

stylePlain, styleWarn, styleClickable, styleEmphasis, styleDivider,
    styleFilename :: Style
stylePlain = word 'A'
styleWarn = word 'B'
styleClickable = word 'C'
styleEmphasis = word 'D'
styleDivider = word 'E'
-- I lost calling function name when I switched from hspp to implicit call
-- stacks.
-- style_func_name = word 'F'
styleFilename = word 'F'
styleFixed = word 'H'

word :: Char -> Word.Word8
word = toEnum . fromEnum
