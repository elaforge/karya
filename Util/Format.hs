-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Simple text formatter.  The pretty print libraries never seem to do what
-- I want and are hard to control.
module Util.Format (
    FormatM, run
    , indented, wrapped_words
    , newline, write
) where
import qualified Control.Applicative as Applicative
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq


data State = State {
    state_col :: !Int
    , state_indent :: !Int
    -- | Chunks of text, stored backward.
    , state_text :: ![Text]
    -- | 'wrapped_words' will wrap text at this width, or not wrap at all if
    -- it's Nothing.
    , state_wrap_width :: !(Maybe Int)
    } deriving (Show)

newtype FormatM a = FormatM (State.State State a)
    deriving (Functor, Applicative.Applicative, Monad, State.MonadState State)

run :: Maybe Int -> FormatM a -> Text
run wrap_width (FormatM m) = extract $ State.execState m state
    where
    extract = (<>"\n") . Text.stripEnd . mconcat . List.reverse . state_text
    state = State
        { state_col = 0
        , state_indent = 0
        , state_text = []
        , state_wrap_width = wrap_width
        }

-- | Run the given format in an increased indentation level.  Indentation
-- takes effect after the next newline.
indented :: Int -> FormatM a -> FormatM a
indented n m = do
    indent <- state_indent <$> get
    modify $ \state -> state { state_indent = indent + n }
    result <- m
    modify $ \state -> state { state_indent = indent }
    return result

-- | Write until the 'state_wrap_width', then wrap.  The wrapped lines get
-- extra indent.
wrapped_words :: Int -> Text -> FormatM ()
wrapped_words indent text = do
    maybe_width <- State.gets state_wrap_width
    case Text.words text of
        [] -> return ()
        w : ws -> do
            write_word maybe_width True w
            indented indent (mapM_ (write_word maybe_width False) ws)
    where
    write_word maybe_width is_first word =
        write1 maybe_width is_first word =<< get
    write1 maybe_width is_first word state
        -- +1 for the leading space.
        | Just width <- maybe_width,
            not at_begin && state_col state + Text.length word + 1 > width =
                newline >> write word
        | at_begin = write word
        | otherwise = write $ if is_first then word else " " <> word
        where at_begin = state_col state == 0

newline :: FormatM ()
newline = write "\n"

write :: Text -> FormatM ()
write text = do
    let (lines, last_line) = split_lines text
    mapM_ write_line lines
    whenJust last_line write_chunk
    where
    write_line text = modify $ \state -> state
        { state_col = 0
        , state_text = (add_indent state text <> "\n") : state_text state
        }
    write_chunk text = modify $ \state ->
        let itext = add_indent state text
        in state
            { state_col = state_col state + Text.length itext
            , state_text = itext : state_text state
            }
    add_indent state text
        | state_col state == 0 && state_indent state > 0 =
            Text.replicate (state_indent state) " " <> text
        | otherwise = text

modify :: (State -> State) -> FormatM ()
modify f = do
    state <- get
    put $! f state

split_lines :: Text -> ([Text], Maybe Text)
split_lines text
    | "\n" `Text.isSuffixOf` text = (lines, Nothing)
    | otherwise = (take (length lines - 1) lines, Seq.last lines)
    where lines = Text.lines text

get :: FormatM State
get = FormatM State.get

put :: State -> FormatM ()
put = FormatM . State.put
